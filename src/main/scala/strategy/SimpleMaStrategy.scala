package strategy

import scala.math
import scala.collection.mutable
import sttp.client3._
import binance.BinanceApi
import com.typesafe.scalalogging.Logger
import binance.TradeSide
import java.util.concurrent.TimeoutException
import java.time.Instant
import java.time.ZoneId
import java.time.Duration
import notifier.Notify
import java.time.ZonedDateTime

// 简单均线策略
// 顺均线突破开仓
// 阴线收跌破均线平仓
// bug： 刚好tick在均线转向处, 则跟随上个均线方向
class SimpleMaStrategy(
    symbol:          String,
    interval:        String,
    maInterval:      Int,
    maxHold:         Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines      = KlineMetric()
    val ma          = MaMetric(klines, maInterval)
    // val macd        = MacdMetric(klines)
    // val kdj         = KdjMetric(klines)
    val positionMgr = PositionMgr(symbol, trader, maxHold, ntf, exceptionNotify)

    val logger = Logger("strategy")

    // 加载历史K线
    def start() = {
        loadHistory()
        positionMgr.loadPosition()
        // 开始websocket
        trader.subscribeKlines(symbol, interval, k => tick(k))
    }

    // 币安是以k线开始时间为准的
    def loadHistory() = {
        val history = trader.getHistory(symbol, interval)
        // 去掉第一条
        history.dropRight(1).foreach(tick(_, true))
        logger.info(
          s"load history of ${symbol}"
        )
    }

    def metricTick(k: Kline) = {
        klines.tick(k)
        ma.tick(k)
        // kdj.tick(k)
        // macd.tick(k)
    }

    // 根据远离均线的程度， 保护利润
    // 以k线最远端为准, 当均线逐渐跟上来， 止盈就放松了
    def updateSl(): Unit = {
        if (!positionMgr.hasPosition) {
            return
        }
        val k           = klines.data(0)
        val as          = avgSize()
        val p           = positionMgr.currentPosition.get
        // load position 的时候没有止损
        // 不设止损， 只在跌破均线且macd转向时平仓
        val basePrice   = if (p.direction == 1) {
            k.high
        } else {
            k.low
        }
        val offsetValue = (basePrice - ma.current.value) * p.direction
        val offsetRatio = offsetValue / as

        val (newSl, reason) = if (offsetRatio > 10) {
            (Some(ma.current.value + offsetValue * 0.9 * p.direction), "偏离均线达到10倍波动")
        } else if (offsetRatio > 5) {
            (Some(ma.current.value + offsetValue * 0.8 * p.direction), "偏离均线达到5倍波动")
        } else if (offsetRatio > 3) {
            (Some(ma.current.value + offsetValue * 0.7 * p.direction), "偏离均线达到3倍波动")
        } else {
            (None, "回归均线，无需设置止盈")
        }
        if (newSl != p.stopLoss) {
            logger.info(
              s"${symbol} 偏离均线倍数: ${offsetRatio} 平均波动: ${as} 移动止损位: ${p.stopLoss} -> ${newSl}, 原因: ${reason}"
            )
        }
        positionMgr.updateSl(newSl)
    }

    def checkSl(): Boolean = {
        if (positionMgr.hasPosition) {
            val k            = klines.data(0)
            val p            = positionMgr.currentPosition.get
            val currentPrice = k.close
            if (p.stopLoss.nonEmpty && (currentPrice - p.stopLoss.get) * p.direction < 0) {
                positionMgr.closeCurrent(k, "触发移动止盈平仓")
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    // 平仓判定
    def checkClose() = {
        val k = klines.data(0)
        // 收盘跌破均线
        if (positionMgr.hasPosition && k.end) {
            val p       = positionMgr.currentPosition.get
            val maValue = ma.current.value

            // 均线劣势侧收反向K线
            if (
              (k.close - maValue) * p.direction < 0 &&
              (k.close - k.open) * p.direction < 0
            ) {
                positionMgr.closeCurrent(k, "跌破均线")
            }
        }
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

    var lastTick: Kline    = null
    var lastMa: BigDecimal = null

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20 && lastTick != null) {
            updateSl()
            checkSl()
            val lastTickMa  = if (k.end) {
                ma.data(1).value
            } else {
                ma.currentValue
            }
            val maDirection = ma.maDirection()
            // val as = avgSize()

            // val basePrice = if (maDirection == 1) {
            //     k.low
            // } else if (maDirection == -1) {
            //     k.high
            // } else {
            //     BigDecimal(0)
            // }
            // 顺势K不要求k线大小
            // 逆势K则要求影线
            // val kLeastSize = if((k.close - k.open) * maDirection > 0 ) {
            //     BigDecimal(0)
            // }else {
            //     as * 0.2
            // }

            if (
              maDirection != 0 &&
              (k.close - ma.currentValue) * maDirection > 0 && // 突破均线
              (lastTick.close - lastTickMa) * maDirection <= 0 &&
              (k.close - k.open) * maDirection > 0 &&          // 顺势k线
            // k线运动方向
            //   (k.close - basePrice) * maDirection > kLeastSize &&
              // todo: 不能完全避免， 因为tick大小不同， 可能突破均线后一个大单就能让价格和均线同时转向
              lastMa == ma.currentValue                        // tick导致均线转向， 会来回开仓, 所以要求均线是稳定的
            ) {
                if (
                  positionMgr.hasPosition && positionMgr.currentPosition.get.direction != maDirection
                ) {
                    positionMgr.closeCurrent(k, "平仓反手")
                }
                if (!positionMgr.hasPosition) {
                    positionMgr.open(
                      k,
                      k.close,
                      maDirection,
                      None,
                      None,
                      false
                    )
                }
            } else {
                checkClose()
            }
        }
        if (!history) {
            lastTick = k
            lastMa = ma.currentValue
        }
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
