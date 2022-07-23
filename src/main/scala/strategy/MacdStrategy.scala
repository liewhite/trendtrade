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
import java.time.ZoneOffset
import java.time.ZonedDateTime

// 多周期macd共振。
// 1，5，15, 1H, 4H， 大于等于4个共振开仓， 小于3个共振平仓
// 开仓条件必须包括1min macd(开仓即浮盈)
class MacdStrategy(
    symbol:          String,
    maxHolds:        Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val minInterval = "1m"
    val klines1     = KlineMetric()
    val klines5     = KlineMetric()
    val klines15    = KlineMetric()
    val klines60    = KlineMetric()
    val klines240   = KlineMetric()
    val macd1       = MacdMetric(klines1)
    val macd5       = MacdMetric(klines5)
    val macd15      = MacdMetric(klines15)
    val macd60      = MacdMetric(klines60)
    val macd240     = MacdMetric(klines240)
    val logger      = Logger("strategy")

    val positionMgr = PositionMgr(symbol, trader, maxHolds, ntf, exceptionNotify)

    // 加载历史K线
    def start() = {
        loadHistory()
        positionMgr.loadPosition()
        // 开始websocket
        trader.subscribeKlines(symbol, minInterval, k => tick(k))
    }

    // 币安是以k线开始时间为准的
    def loadHistory() = {
        val history1   = trader.getHistory(symbol, minInterval)
        val history5   = trader.getHistory(symbol, "5m")
        val history15  = trader.getHistory(symbol, "15m")
        val history60  = trader.getHistory(symbol, "1h")
        val history240 = trader.getHistory(symbol, "4h")

        history1.dropRight(1).foreach(klines1.tick(_))
        history1.dropRight(1).foreach(macd1.tick(_))

        history5.dropRight(1).foreach(klines5.tick(_))
        history5.dropRight(1).foreach(macd5.tick(_))

        history15.dropRight(1).foreach(klines15.tick(_))
        history15.dropRight(1).foreach(macd15.tick(_))

        history60.dropRight(1).foreach(klines60.tick(_))
        history60.dropRight(1).foreach(macd60.tick(_))

        history240.dropRight(1).foreach(klines240.tick(_))
        history240.dropRight(1).foreach(macd240.tick(_))
    }

    // 除当前K外的最近k线平均大小
    def avgSize(): BigDecimal = {
        // 以15min线作为跟踪止盈基准
        val entities = klines15.data
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

    // 更新止损位
    def updateSl(): Unit = {
        if (!positionMgr.hasPosition) {
            return
        }

        val k     = klines1.data(0)
        val as    = avgSize()
        val p     = positionMgr.currentPosition.get
        // load position 的时候没有止损
        val oldSl = p.stopLoss match {
            case None    => p.openAt - p.direction * as * 1.5
            case Some(o) => o
        }

        def maxSl(o: BigDecimal, n: BigDecimal, d: Int): BigDecimal = {
            if (d == 1) {
                // 多单， 取最大值
                Vector(o, n).max
            } else if (d == -1) {
                // 空单， 取最小值
                Vector(o, n).min
            } else {
                throw Exception(s"持仓方向为0: ${p}")
            }
        }

        //  利润 / 平均size, >0盈利， <0 亏损
        val profit           = (k.close - p.openAt) * p.direction
        val profitForAvgSize = profit / as

        val (newSl, reason) = if (profitForAvgSize > 20) {
            // 浮盈大于20倍k线size, 跟踪止盈到最大盈利的90%
            (maxSl(oldSl, p.openAt + profit * 0.9 * p.direction, p.direction), "达到20倍波动")
        } else if (profitForAvgSize > 10) {
            // 浮盈大于10倍k线size, 跟踪止盈到最大盈利的80%
            (maxSl(oldSl, p.openAt + profit * 0.8 * p.direction, p.direction), "达到10倍波动")
        } else if (profitForAvgSize > 5) {
            // 浮盈大于5倍k线size, 跟踪止盈到最大盈利的60%
            (maxSl(oldSl, p.openAt + profit * 0.6 * p.direction, p.direction), "达到5倍波动")
        } else if (profitForAvgSize > 3) {
            // 浮盈大于3倍k线size, 跟踪止盈到最大盈利的40%
            (maxSl(oldSl, p.openAt + profit * 0.4 * p.direction, p.direction), "达到3倍波动")
        } else if (profitForAvgSize > 1.5) {
            // 浮盈大于1倍size， 保本出
            (maxSl(oldSl, p.openAt + profit * 0.4 * p.direction, p.direction), "达到1.5倍波动")
        } else if (profitForAvgSize <= 0.5) {
            // 几乎无盈利或浮亏， 0.8倍平均size止损
            // 当波动越来越小， 止损也越来越小
            // 反之， 波动大， 止损就大， 跟随市场
            (maxSl(oldSl, p.openAt - as * 1.5 * p.direction, p.direction), "无浮盈")
        } else {
            // 应该不会执行到这里
            (p.stopLoss.get, "无止损调节需求")
        }
        if (newSl != oldSl) {
            ntf.sendNotify(
              s"${symbol} 利润: ${profit} 平均波动: ${as} 移动止损位: ${oldSl} -> ${newSl}, 原因: ${reason}"
            )
        }
        positionMgr.updateSl(Some(newSl))
    }

    def checkSl(): Boolean = {
        if (positionMgr.hasPosition) {
            val k            = klines1.data(0)
            val p            = positionMgr.currentPosition.get
            val currentPrice = k.close
            if ((currentPrice - p.stopLoss.get) * p.direction < 0) {
                positionMgr.closeCurrent(k, "触发移动止盈平仓")
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    def metricTick(k: Kline) = {
        val startSecondsOfK = k.datetime.toInstant().toEpochMilli() / 1000
        klines1.tick(k)
        macd1.tick(k)

        val k5 = if ((startSecondsOfK + 60) % (5 * 60) == 0) {
            k
        } else {
            k.copy(end = false)
        }
        klines5.tick(k5)
        macd5.tick(k5)

        val k15 = if ((startSecondsOfK + 60) % (15 * 60) == 0) {
            k
        } else {
            k.copy(end = false)
        }

        klines15.tick(k15)
        macd15.tick(k15)

        val k60 = if ((startSecondsOfK + 60) % (60 * 60) == 0) {
            k
        } else {
            k.copy(end = false)
        }
        klines60.tick(k60)
        macd60.tick(k60)

        val k240 = if ((startSecondsOfK + 60) % (240 * 60) == 0) {
            k
        } else {
            k.copy(end = false)
        }
        klines240.tick(k240)
        macd240.tick(k240)
    }

    def checkClose() = {
        if (positionMgr.hasPosition) {
            val macds = Vector(
              macd5.macdDirection,
              macd15.macdDirection,
              macd60.macdDirection,
              macd240.macdDirection
            )
            if (macds.count(_ == positionMgr.currentPosition.get.direction) < 3) {
                positionMgr.closeCurrent(klines1.current, s"macd: ${macds.mkString(",")}")
            }
        }
    }

    var openTime: ZonedDateTime = null
    var lastTick: Kline         = null

    def tick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)

        // 忽略历史数据， 只处理实时数据
        if (history) {
            return
        }

        // 历史数据不足， 无法参考
        if (klines240.data.length < 26) {
            return
        }
        updateSl()
        checkSl()
        checkClose()

        if (openTime != null && Duration.between(openTime, ZonedDateTime.now()).getSeconds() < 60) {
            return
        }

        if (!positionMgr.hasPosition) {
            val as       = avgSize()
            val macds    = Vector(
              macd5.macdDirection,
              macd15.macdDirection,
              macd60.macdDirection,
              macd240.macdDirection
            )
            val preMacds = Vector(
              macd1.macdDirection,
              macd5.macdDirection,
              macd15.macdDirection,
              macd60.macdDirection,
              macd240.macdDirection
            )

            if (
              macds.count(_ == macd1.macdDirection) >= 3 && preMacds.count(
                _ == macd1.macdDirection
              ) < 4
            ) {
                logger.info(s"${symbol} pre macds: ${preMacds}, current: ${macds}")
                positionMgr.open(
                  k,
                  k.close,
                  macd1.macdDirection,
                  Some(k.close - (1.5 * as) * macd1.macdDirection),
                  None,
                  false
                )
                // 休息一分钟
                openTime = ZonedDateTime.now()
            }
        }
        lastTick = k
    }
}
