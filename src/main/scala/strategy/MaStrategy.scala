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

// 开仓改进： 均线顺势， 价格在均线附近或负偏离状态， 价格离k线端点有一定距离
class MaStrategy(
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
    val macd        = MacdMetric(klines)
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
        // longMa.tick(k)
        macd.tick(k)
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
            ntf.sendNotify(
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

            if ((k.close - maValue) * p.direction < 0) {
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

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20) {
            updateSl()
            checkSl()

            val maDirection   = ma.maDirection()
            // 需要一个稳定的方向， 就算突破时macd不对， 多半也会回来接上车， 那时macd就对了。
            val macdDirection = macd.macdBarTrend()

            val as     = avgSize()
            val basePrice = if(maDirection == 1) {
                k.low
            }else if(maDirection == -1) {
                k.high
            }else {
                BigDecimal(0)
            }

            if (
              maDirection != 0 &&
              macdDirection == maDirection &&                   // 均线方向与macd一致
              (k.close - ma.currentValue) * maDirection < 0.3 * as && // 价格在成本优势区间
              (k.close - basePrice) * maDirection > 0.2 * as  // 正向波动
            //   (k.close - ma.current.value) * maDirection > 0 && // 突破均线
            //   (lastTick.close - lastMa) * maDirection <= 0      // 上一tick未突破均线
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
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
