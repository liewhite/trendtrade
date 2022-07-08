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

// ma10,kdj(d),macd,macd(dea) =4 共振开仓， < 3平仓
class MultipleMetricStrategy(
    symbol:          String,
    interval:        String,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines = KlineMetric()
    val ma10   = MaMetric(klines, 10)
    val macd   = MacdMetric(klines)
    val kdj    = KdjMetric(klines)

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
          s"load history of ${symbol} , last kline: ${klines.data(0)} ma20: ${ma10.data(0)}"
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
        ma10.tick(k)
        macd.tick(k)
        kdj.tick(k)
    }

    def avgSize(): BigDecimal  = {
        val entities = klines.data
            .slice(1, 21)
            .map(item => {
                if (item.close == item.open) {
                    BigDecimal(0)
                } else {
                    (item.close - item.open).abs
                }
            })

        val avgEntitySize = entities.sum / entities.length
        avgEntitySize
    }

    // 只有上一tick不满足条件， 该tick满足条件才会开仓
    // 意思是只做趋势启动， 错过了就不会去追
    var lastState: Option[Int] = None

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
        val macdDir       = macd.macdDirection
        val deaDir        = macd.deaDirection
        val ma10Direction = ma10.maDirection
        val dDirection    = kdj.dDirection

        val d = if (Vector(macdDir, deaDir, ma10Direction, dDirection).forall(_ == 1)) {
            1
        } else if (Vector(macdDir, deaDir, ma10Direction, dDirection).forall(_ == -1)) {
            -1
        } else {
            0
        }

        if (currentPosition.isEmpty && lastState.nonEmpty) {
            // 有方向， 上个tick跟当前方向不一致， 偏离均线不超过2倍平均波幅
            if (
              d != 0 && lastState.get != d && (k.close - ma10.data(0).value) * d < avgSize() * 2
            ) {
                open(d)
            }
        } else if (currentPosition.nonEmpty) {
            // 检查是否清仓
            val d = currentPosition.get.direction
            // 收盘跌破两个指标， 或者tick跌破3个指标都要清仓
            if (
              (Vector(macdDir, deaDir, ma10Direction, dDirection).count(_ == d) < 3 && k.end) ||
              (Vector(macdDir, deaDir, ma10Direction, dDirection).count(_ == d) < 2 && !k.end)
            ) {
                closeCurrent()
            }
        } else {}

        // 记录上一tick的状态
        lastState = Some(d)
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
