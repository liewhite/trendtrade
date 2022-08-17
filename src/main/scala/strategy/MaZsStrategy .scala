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

// 均线中枢策略
// 中枢震荡策略.md
class MaZsStrategy(
    symbol:          String,
    interval:        String,
    shortMaInterval: Int,
    longMaInterval:  Int,
    maxHold:         Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines  = KlineMetric()
    val shortMa = MaMetric(klines, shortMaInterval)
    val longMa  = MaMetric(klines, longMaInterval)
    val macd    = MacdMetric(klines)
    val kdj     = KdjMetric(klines)

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
        shortMa.tick(k)
        longMa.tick(k)
        kdj.tick(k)
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
        val offsetValue = (basePrice - shortMa.current.value) * p.direction
        val offsetRatio = offsetValue / as

        val (newSl, reason) = if (offsetRatio > 20) {
            (Some(shortMa.current.value + offsetValue * 0.9 * p.direction), "偏离均线达到20倍波动")
        } else if (offsetRatio > 13) {
            (Some(shortMa.current.value + offsetValue * 0.8 * p.direction), "偏离均线达到13倍波动")
        } else if (offsetRatio > 7) {
            (Some(shortMa.current.value + offsetValue * 0.7 * p.direction), "偏离均线达到7倍波动")
        } else {
            (None, "回归均线，取消止盈")
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

    def directions = {
        Vector(kdj.dDirection(), macd.macdBarTrend())
    }

    def baseDirection: Int = {
        if (directions.forall(_ == 1)) {
            1
        } else if (directions.forall(_ == -1)) {
            -1
        } else {
            0
        }
    }

    // 中枢宽度
    def zsWidth: BigDecimal = {
        (shortMa.currentValue - longMa.currentValue).abs
    }

    // 是否是趋势行情， true趋势， false震荡
    def isTrend: Boolean = {
        zsWidth > avgSize() * 3
    }

    // 价格是否远离中枢, 以及远离方向
    // 比如向上远离3as以上 -1
    def priceOffsetDirection: Int = {
        val maValues = Vector(shortMa.currentValue, longMa.currentValue)
        val maxMa    = maValues.max
        val minMa    = maValues.min
        val price    = klines.current.close
        val as       = avgSize()
        if ((price - maxMa) > 3 * as) {
            -1
        } else if ((price - minMa) < -3 * as) {
            1
        } else {
            0
        }
    }

    def shakeDirection: Int = {
        val offsetDirection = priceOffsetDirection
        // 价格偏移不够， 或者均线发散， 不考虑做高抛低吸
        if (offsetDirection == 0 || isTrend) {
            0
        } else {
            val macdDirection = macd.barDirection()
            val kdjDirection  = kdj.dDirection()

            val macdDirection1 = macd.barDirection(1)
            val kdjDirection1  = kdj.dDirection(1)
            // 上一k 指标不满足， 且当前满足
            if (
              macdDirection == offsetDirection &&
              kdjDirection == offsetDirection &&
              (macdDirection1 != offsetDirection ||
                  kdjDirection1 != offsetDirection)
            ) {
                macdDirection
            } else {
                0
            }
        }
    }

    // 均线排列方向
    // 短期均线在上， 多头排列
    // 长期均线在上， 空投排列
    def maRank: Int = {
        (shortMa.currentValue - longMa.currentValue).signum
    }

    // 趋势回调开仓方向
    // 均线排列方向与指标方向一致
    // 价格回调到短期均线或者更多
    def trendDirection: Int = {
        val k = klines.current
        // 价格偏移不够， 或者均线发散， 不考虑做高抛低吸
        if (!isTrend) {
            0
        } else {
            val maRankDirection = maRank
            val macdDirection = macd.barDirection()
            val kdjDirection  = kdj.dDirection()

            val macdDirection1 = macd.barDirection(1)
            val kdjDirection1  = kdj.dDirection(1)
            // 上一k 指标不满足， 且当前满足
            if (
              macdDirection == maRankDirection &&
              kdjDirection == maRankDirection &&
              (macdDirection1 != maRankDirection ||
                  kdjDirection1 != maRankDirection) &&
             (k.close - shortMa.currentValue ) * macdDirection < 0.5 * avgSize()
            ) {
                macdDirection
            } else {
                0
            }
        }
    }

    // 平仓判定
    // 亏损： 收盘指标不再满足开仓条件
    // 盈利： 指标完全反向
    def checkClose() = {
        val k = klines.data(0)
        if (positionMgr.hasPosition) {
            val p = positionMgr.currentPosition.get
            if (k.end) {

                if((k.close - p.openAt) * p.direction < 0 ) {
                    if(!directions.forall(_ == p.direction)){
                        positionMgr.closeCurrent(k, "亏损状态指标被破坏， 止损")
                    }
                }else {
                    if(directions.forall(_ == -p.direction)){
                        positionMgr.closeCurrent(k, "盈利状态指标反向， 止盈")
                    }
                }
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
            // 检查跟踪止盈
            updateSl()
            checkSl()
            val td = trendDirection
            val sd = shakeDirection

            val direction = if(td != 0) {
                td
            }else if(sd != 0) {
                sd
            }else {
                0
            }
            val reason = if(td != 0) {
                "趋势回调结束"
            }else if(sd != 0) {
                "震荡高抛低吸"
            }else {
                "方向都没有为毛开仓"
            }

            if (direction != 0) {
                if (
                  positionMgr.hasPosition && positionMgr.currentPosition.get.direction != direction
                ) {
                    positionMgr.closeCurrent(k, "平仓反手")
                }
                if (!positionMgr.hasPosition) {
                    positionMgr.open(
                      k,
                      k.close,
                      direction,
                      None,
                      None,
                      false,
                      reason = reason
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
