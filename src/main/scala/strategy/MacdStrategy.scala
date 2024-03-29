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

// 小周期底背离
// 大周期macd trend， kdj共振
// 跟踪止盈
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
    // val klines15    = KlineMetric()
    val klines30    = KlineMetric()
    // val klines60    = KlineMetric()
    // val klines240   = KlineMetric()
    val macd1       = MacdMetric(klines1)
    val macd5       = MacdMetric(klines5)
    // val macd15      = MacdMetric(klines15)
    val macd30      = MacdMetric(klines30)
    // val macd60      = MacdMetric(klines60)
    // val macd240     = MacdMetric(klines240)
    val ma5         = MaMetric(klines5, 20)
    val ma30        = MaMetric(klines30, 20)

    val kdj1  = KdjMetric(klines1)
    val kdj5  = KdjMetric(klines5)
    val kdj30 = KdjMetric(klines30)

    val logger   = Logger("strategy")
    val slFactor = 0.3

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
        val history1  = trader.getHistory(symbol, minInterval)
        val history5  = trader.getHistory(symbol, "5m")
        // val history15  = trader.getHistory(symbol, "15m")
        val history30 = trader.getHistory(symbol, "30m")
        // val history60  = trader.getHistory(symbol, "1h")
        // val history240 = trader.getHistory(symbol, "4h")

        history1.dropRight(1).foreach(klines1.tick(_))
        history1.dropRight(1).foreach(macd1.tick(_))
        history1.dropRight(1).foreach(kdj1.tick(_))

        history5.dropRight(1).foreach(klines5.tick(_))
        history5.dropRight(1).foreach(macd5.tick(_))
        history5.dropRight(1).foreach(kdj5.tick(_))
        history5.dropRight(1).foreach(ma5.tick(_))

        // history15.dropRight(1).foreach(klines15.tick(_))
        // history15.dropRight(1).foreach(macd15.tick(_))

        history30.dropRight(1).foreach(klines30.tick(_))
        history30.dropRight(1).foreach(macd30.tick(_))
        history30.dropRight(1).foreach(kdj30.tick(_))
        history30.dropRight(1).foreach(ma30.tick(_))

        // history60.dropRight(1).foreach(klines60.tick(_))
        // history60.dropRight(1).foreach(macd60.tick(_))

        // history240.dropRight(1).foreach(klines240.tick(_))
        // history240.dropRight(1).foreach(macd240.tick(_))
    }

    // 除当前K外的最近k线平均大小
    def avgSize(): BigDecimal = {
        // 以15min线作为跟踪止盈基准
        val entities = klines30.data
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
            case None    => p.openAt - p.direction * as * slFactor
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
            (maxSl(oldSl, p.openAt + profit * 0.7 * p.direction, p.direction), "达到5倍波动")
        } else if (profitForAvgSize > 3) {
            // 浮盈大于3倍k线size, 跟踪止盈到最大盈利的40%
            (maxSl(oldSl, p.openAt + profit * 0.6 * p.direction, p.direction), "达到3倍波动")
        } else if (profitForAvgSize > 1.5) {
            // 浮盈大于1倍size， 0.75出
            (maxSl(oldSl, p.openAt + profit * 0.5 * p.direction, p.direction), "达到1.5倍波动")
        } else if (profitForAvgSize > 1) {
            // 浮盈大于1倍size， 0.4出
            (maxSl(oldSl, p.openAt + profit * 0.4 * p.direction, p.direction), "达到1倍波动")
        } else if (profitForAvgSize > 0.5) {
            // 浮盈大于0.5倍size， 0.15出
            (maxSl(oldSl, p.openAt + profit * 0.3 * p.direction, p.direction), "达到0.5倍波动")
        } else if (profitForAvgSize <= 0.5) {
            // 几乎无盈利或浮亏， 0.8倍平均size止损
            // 当波动越来越小， 止损也越来越小
            // 反之， 波动大， 止损就大， 跟随市场
            (maxSl(oldSl, p.openAt - as * slFactor * p.direction, p.direction), "无浮盈")
        } else {
            // 应该不会执行到这里
            (oldSl, "无止损调节需求")
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

    def mergeK(ks: Seq[Kline], end: Boolean = false): Kline = {
        Kline(
          ks.last.datetime,
          ks.last.open,
          ks.map(_.low).min,
          ks.map(_.high).max,
          ks.head.close,
          ks.map(_.vol).sum,
          end
        )
    }

    // 聚合k线
    def kn(minInterval: Int): Kline = {
        val k               = klines1.current
        val startSecondsOfK = k.datetime.toInstant().toEpochMilli() / 1000
        // 计算当前一分钟线的结束时间
        // mod 得到n分钟线从开始到现在的秒数
        val mod             = (startSecondsOfK + 60) % (minInterval * 60)
        // 如果mod为0， 说明当前一分钟线结束对应n分钟线的结束
        if (mod == 0) {
            mergeK(klines1.data.slice(0, minInterval).toSeq, k.end)
        } else {
            mergeK(klines1.data.slice(0, (mod / 60).intValue).toSeq, false)
        }
    }

    def tickN(
        k: Kline,
        n: Int,
        ks: KlineMetric,
        maM: MaMetric,
        macdM: MacdMetric,
        kdjM: KdjMetric
    ) = {
        val knResult = kn(n)
        // logger.info(s"${symbol} k${n}: ${knResult}")
        ks.tick(knResult)
        macdM.tick(knResult)
        kdjM.tick(knResult)
        maM.tick(knResult)

    }

    def metricTick(k: Kline) = {
        val startSecondsOfK = k.datetime.toInstant().toEpochMilli() / 1000
        // logger.info(s"${symbol} ts: ${startSecondsOfK}")
        klines1.tick(k)
        macd1.tick(k)
        tickN(k, 5, klines5, ma5, macd5, kdj5)
        // tickN(k, 15, klines15, macd15)
        tickN(k, 30, klines30, ma30, macd30, kdj30)
        // tickN(k, 60, klines60, macd60)
        // tickN(k, 240, klines240, macd240)
    }

    var openTime: ZonedDateTime = null
    var lastCondition: Option[Boolean]  = None
    // var lastTick: Kline         = null

    def tick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)

        // 忽略历史数据， 只处理实时数据
        if (history) {
            return
        }

        // 历史数据不足， 无法参考
        if (klines30.data.length < 26) {
            return
        }

        updateSl()

        val macdDirections = Vector(
          macd5.macdBarTrend(),
          macd30.macdBarTrend()
        )

        val kdjDirections = Vector(
          kdj5.kdjRange(),
          kdj30.kdjRange()
        )
        val direction     = if (macdDirections.forall(_ == 1)) {
            1
        } else if (macdDirections.forall(_ == -1)) {
            -1
        } else {
            0
        }

        val as = avgSize()

        val condition = direction != 0 && kdjDirections.forall(
          _ == direction
        ) && (k.close - ma30.currentValue) * direction < as

        if (condition && lastCondition.isDefined && !lastCondition.get ) {
            if (positionMgr.hasPosition && positionMgr.currentPosition.get.direction != direction) {
                positionMgr.closeCurrent(k, "平仓反手")
            }

            // 有 last tick 不满足条件作为前提
            // if (
            //   openTime != null && Duration.between(openTime, ZonedDateTime.now()).getSeconds() < 60
            // ) {
            //     return
            // }

            // 可能会亏损后导致开仓额度不足
            if (!positionMgr.hasPosition) {
                positionMgr.open(
                  k,
                  k.close,
                  direction,
                  Some(k.close - (slFactor * as) * direction),
                  None,
                  false
                )
            }

            // 休息一分钟
            // openTime = ZonedDateTime.now()
        } else {
            // 不满足开仓条件则判断是否触发了跟踪止盈
            // 防止平仓后又立马开仓
            checkSl()
        }
        lastCondition = Some(condition)
        // lastTick = k
    }
}
