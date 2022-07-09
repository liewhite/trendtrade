package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
import sttp.client3._
import binance.BinanceApi
import com.typesafe.scalalogging.Logger
import binance.TradeSide
import java.util.concurrent.TimeoutException
import java.time.Instant
import java.time.ZoneId
import java.time.Duration
import notifier.Notify

// 当前价处于A侧
// 假设价格回归到均线， 则均线顺势。 

// 开盘价和现价分别位于均线两侧
// 开盘价小于均线最近的最小值
// tick 从劣势侧冲到优势测开仓
// 收盘回撤过均线平仓
class Ma2Strategy(
    symbol:          String,
    interval:        String,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines = KlineMetric()
    val ma20   = MaMetric(klines, 20)
    val macd   = MacdMetric(klines)

    val logger                            = Logger("strategy")
    var currentPosition: Option[Position] = None
    val closed                            = mutable.ListBuffer.empty[Position]
    // 是否暂停开仓, 避免频繁调用

    // 加载历史K线
    def start() = {
        loadHistory()
        loadPosition()
        // 开始websocket
        trader.subscribeKlines(symbol, interval, k => tick(k))
    }

    def symbolMeta           = trader.symbolMeta(symbol)
    // 必须先load历史K线才能加载持仓
    def loadPosition(): Unit = {
        logger.info(s"load positions of ${symbol}")
        // 获取持仓,过滤出symbol
        val positions = trader.getPositions(symbol)
        if (positions.length == 0) {
            return
        }
        if (positions.length > 1) {
            throw Exception("positions > 1, strategy only support one position")
        }
        val p         = positions(0)
        val direction = p.positionAmt.signum
        currentPosition = Some(
          Position(
            p.positionAmt.abs,
            LocalDateTime.now,
            direction,
            p.entryPrice,
            None,
            None,
            None,
            None
          )
        )
    }

    // 币安是以k线开始时间为准的
    def loadHistory() = {
        val history = trader.getHistory(symbol, interval)
        // 去掉第一条
        history.dropRight(1).foreach(tick(_, true))
        logger.info(
          s"load history of ${symbol} , last kline: ${klines.data(0)} ma20: ${ma20.data(0)}"
        )
    }

    def closeCurrent(): Unit = {
        val k = klines.data(0)
        currentPosition match {
            case None       =>
            case Some(item) => {
                val msg = s"触发平仓:${symbol} ${item} 当前k: ${k}"
                logger.info(msg)
                ntf.sendNotify(msg)
                try {
                    trader.sendOrder(
                      symbol,
                      if (item.direction == 1) then TradeSide.SELL else TradeSide.BUY,
                      item.quantity,
                      close = true
                    )
                    val msg = s"平仓成功: ${symbol} ${item} 当前k: ${k}"
                    logger.info(msg)
                    ntf.sendNotify(msg)
                    closed.prepend(
                      item.copy(
                        closeTime = Some(klines.data(0).datetime),
                        closeAt = Some(klines.data(0).close)
                      )
                    )
                } catch {
                    case e: TimeoutException => {
                        val msg = s"挂单未成交， 请手动取消或平仓, ${symbol} ${k} ${e}"
                        logger.error(msg)
                        exceptionNotify.sendNotify(msg)
                    }
                    case e: Exception => {
                        val msg = s"平仓失败， 请检查账户是否存在不一致 ${symbol} ${k} ${e}"
                        logger.error(msg)
                        exceptionNotify.sendNotify(msg)
                    }
                }
                // 无论如何都要删除持仓， 不然容易引起不一致, 币安端可以手动操作平仓
                currentPosition = None
            }
        }

    }

    def formatQuantity(n: BigDecimal): BigDecimal = {
        BigDecimal((n / symbolMeta.stepSize).intValue) * symbolMeta.stepSize
    }
    def formatPrice(n: BigDecimal): BigDecimal    = {
        BigDecimal((n / symbolMeta.priceStep).intValue) * symbolMeta.priceStep
    }

    // 发送订单， 等待成交
    // 止盈止损
    def open(direction: Int): Unit = {
        if (currentPosition.nonEmpty) {
            logger.warn(s"${symbol} 重复开仓， 忽略")
            return
        }
        // 查询账户总额， 余额, 如果余额小于总额的10%()， 放弃开仓
        val balances    = trader.getTotalBalance()
        if (balances._2 < balances._1 * 0.1) {
            val msg = s"余额不足10%, 停止开仓 ${balances._2}/${balances._1}"
            logger.warn(msg)
            // ntf.sendNotify(msg)
            return
        }
        val k           = klines.data(0)
        val price       = k.close
        // 按精度取近似值
        val rawQuantity = ((balances._1 * 0.1) / price * trader.leverage)
        val quantity    = formatQuantity(rawQuantity)
        val side        = if (direction == 1) TradeSide.BUY else TradeSide.SELL
        val msg         = s"触发开仓 ${symbol}, ${side} ${quantity}, k: ${k}"
        logger.info(msg)
        ntf.sendNotify(msg)
        try {
            trader.sendOrder(
              symbol,
              side,
              quantity
              //   Some(formatPrice(stopLoss)),
              //   Some(formatPrice(tp))
            )
            val msg = s"开仓成功 ${symbol}, ${side} ${quantity}"
            logger.info(msg)
            ntf.sendNotify(msg)
            currentPosition = Some(
              Position(
                quantity,
                k.datetime,
                direction,
                k.close,
                None,
                None,
                None,
                None
              )
            )
        } catch {
            case e: TimeoutException => {
                val msg = s" ${symbol} 挂单未成交， 请手动取消开仓挂单, ${k}"
                logger.error(msg)
                exceptionNotify.sendNotify(msg)
            }
            case e: Exception => {
                val msg = s"${symbol} 开仓失败， 请检查账户是否存在不一致"
                logger.warn(msg)
                exceptionNotify.sendNotify(msg)
            }
        }
    }

    def metricTick(k: Kline) = {
        klines.tick(k)
        ma20.tick(k)
        macd.tick(k)
    }

    // 除当前K外的最近k线平均大小
    def avgSize(): BigDecimal = {
        val entities = klines.data
            .slice(1, 21)
            .map(item => {
                if (item.high == item.low) {
                    BigDecimal(0)
                } else {
                    (item.high - item.low).abs
                }
            })

        val avgEntitySize = entities.sum / entities.length
        avgEntitySize
    }

    // 上一tick价位处于均线的哪侧
    var lastDirection: Option[Int] = None

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (history) {
            return
        }
        // 历史数据不足， 无法参考
        if (klines.data.length < 20) {
            return
        }

        // 当前价位处于均线哪侧
        val currentDirection = (k.close - ma20.data(0).value).signum
        // macd方向
        val maDirection      = ma20.maDirection

        // 检查是否需要平仓
        if (currentPosition.nonEmpty) {
            // ma 还未反转， 收盘跌破均线平仓
            if (currentPosition.get.direction == maDirection) {
                if (k.end) {
                    if ((k.close - ma20.data(0).value) * currentPosition.get.direction <= 0) {
                        closeCurrent()
                    }
                }
            } else {
                // macd已调头， 则tick破均线就平仓
                if ((k.close - ma20.data(0).value) * currentPosition.get.direction <= 0) {
                    closeCurrent()
                    // 不能反手， 因为可能跌破后过了一阵ma反转，此时离均线已经比较远了。
                    // open(maDirection)
                }
            }
        }
        // 平仓后,再判断是否需要反手开仓
        // 上个tick信息存在 && 和当前tick处于均线两侧 && 当前侧顺势
        // 开盘价必须在劣势侧
        if (
          currentPosition.isEmpty &&                                // 无持仓
          lastDirection.nonEmpty &&
          lastDirection.get != currentDirection &&
          currentDirection == maDirection &&
          maDirection != 0 &&
          (k.open - ma20.data(0).value).signum == lastDirection.get // 开盘价在劣势侧
        ) {
            // 
            val as = avgSize()

            // 往上一个tick， 往下一个tick就会导致macd来回调头的情况一定要排除掉, 会造成巨大的震荡亏损
            // 给价格加一个负偏移量，重新生成一遍macd, 如果拐头了， 则不要开仓, 防来回震荡
            val negMacd          = macd.data(1).next(k, k.close - currentDirection * as * 0.5)
            val negMacdDirection = (negMacd.bar - macd.data(1).bar).signum
            // 加上负偏移了方向还不变， 才是开仓时机
            if (negMacdDirection == currentDirection) {
                open(currentDirection)
            } else {
                logger.info(
                  s"震荡时期不开仓: ${symbol}, avgSize: ${as} preMacd: ${macd.data(1).bar} macd:${macd.data(0).bar}, negMa: ${negMacd.bar}"
                )
            }
        }
        // 记录上一tick的状态
        lastDirection = Some(currentDirection)
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
