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

// 均线顺势，价格合理， 且价格处于上涨时开仓
// 均线调头， 破均线和新低平仓, 避免来回止损
// 均线未调头，收盘价未站上均线且收阴线平仓
class MaStrategy(
    symbol:          String,
    interval:        String,
    maSize:          Int,
    maxHold:         Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines = KlineMetric()
    val maSeq  = MaMetric(klines, maSize)
    val macd   = MacdMetric(klines)

    val logger                            = Logger("strategy")
    var currentPosition: Option[Position] = None
    val closed                            = mutable.ListBuffer.empty[Position]

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
          s"load history of ${symbol} , last kline: ${klines.data(0)} ma20: ${maSeq.data(0)}"
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
        if (balances._2  * maxHold < balances._1) {
            val msg = s"余额不足, 停止开仓 ${balances._2}/${balances._1}"
            logger.warn(msg)
            // ntf.sendNotify(msg)
            return
        }
        val k           = klines.data(0)
        val price       = k.close
        // 按精度取近似值
        val rawQuantity = ((balances._1 / maxHold) / price * trader.leverage)
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
        maSeq.tick(k)
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

    // 前一tick信息
    var lastTick: Kline = null

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20) {

            val maDirection = maSeq.maDirection

            // 开仓看均线方向, 均线向上就突破high开仓， 均线向下就跌破low 开仓
            val openThreshold = if (maDirection == 1) lastTick.high else lastTick.low

            // 检查是否需要平仓, 同一K线上， 平仓一次， 下次平仓必须要超过该价位了, 不断放大, 限制平仓次数
            if (currentPosition.nonEmpty) {
                val positionDirection = currentPosition.get.direction
                // 平仓看持仓方向， 持有多单则跌破low平仓， 持有空单则
                val closeThreshold    = if (positionDirection == 1) lastTick.low else lastTick.high
                // ma 还未反转， 收盘跌破均线平仓
                if (positionDirection == maDirection) {
                    if (
                      k.end &&                                                 // 收盘
                      (k.close - k.open) * positionDirection < 0 &&            // 逆势K
                      (k.close - maSeq.data(0).value) * positionDirection <= 0 // 收盘价未站上均线
                    ) {
                        closeCurrent()
                    }
                } else {
                    // ma已调头， 则tick破均线且创K新低平仓
                    if (
                      (k.close - maSeq.data(0).value) * positionDirection <= 0 &&
                      (k.close - closeThreshold) * positionDirection < 0
                    ) {
                        closeCurrent()
                    }
                }
            }

            val as = avgSize()

            // 平仓后,再判断是否需要开仓
            if (
              currentPosition.isEmpty &&                                    // 无持仓
              maDirection != 0 &&                                           // 均线有方向
              maSeq.historyMaDirection(0) == maSeq.historyMaDirection(1) && // 均线有趋势, 避免连续的震荡K线
              maSeq.historyMaDirection(1) == maSeq.historyMaDirection(2) &&
              maSeq.historyMaDirection(2) == maSeq.historyMaDirection(3) &&
              (k.close - maSeq.data(0).value) * maDirection < as * 0.2 &&   // 不正偏离均线太多
              (k.close - openThreshold) * maDirection > 0                   // 只在突破当前K线端点时开仓, 避免单K内来回震荡触发开仓
            ) {
                open(maDirection)
            }
        }
        lastTick = k
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
