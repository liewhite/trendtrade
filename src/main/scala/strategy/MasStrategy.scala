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
import cats.instances.long

// 如何处理均线纠缠频繁开仓
// 只朝k线方向开仓， 比如均线纠缠了， 但是并不会在一根k线内来回开仓
// 浮动止盈止损, 或者反向信号出现，直接反手
class MasStrategy(
    symbol:          String,
    interval:        String,
    shortMaInterval: Int, // 短均线
    midMaInterval:   Int, // 中均线
    longMaInterval:  Int, // 长均线
    maxHold:         Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines      = KlineMetric()
    val shortMa     = MaMetric(klines, shortMaInterval)
    val midMa       = MaMetric(klines, midMaInterval)
    val longMa      = MaMetric(klines, longMaInterval)
    val positionMgr = PositionMgr(symbol, trader, maxHold, ntf, exceptionNotify)
    val slFactor = 0.5

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
        midMa.tick(k)
        longMa.tick(k)
    }

    // 更新止损位
    def updateSl(): Unit = {
        if (!positionMgr.hasPosition) {
            return
        }

        val k     = klines.data(0)
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
            (maxSl(oldSl, p.openAt + profit * 0.6 * p.direction, p.direction), "达到5倍波动")
        } else if (profitForAvgSize > 3) {
            // 浮盈大于3倍k线size, 跟踪止盈到最大盈利的40%
            (maxSl(oldSl, p.openAt + profit * 0.4 * p.direction, p.direction), "达到3倍波动")
        } else if (profitForAvgSize > 1.5) {
            (maxSl(oldSl, p.openAt + profit * 0.3 * p.direction, p.direction), "达到1.5倍波动")
        } else if (profitForAvgSize > 1) {
            // 浮盈大于1倍size， 保本出
            (maxSl(oldSl, p.openAt + profit * 0.2 * p.direction, p.direction), "达到1倍波动")
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
            val k            = klines.data(0)
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
    var lastTick: Kline         = null

    def aggDirections(ds: Vector[Int]): Int = {
        if (ds.forall(_ == 1)) {
            return 1
        } else if (ds.forall(_ == -1)) {
            return -1
        } else {
            return 0
        }
    }

    def openDirection(currentDirection: Int, lastKDirection: Int, lastTickDirection: Int, kDirection: Int): Int = {
        // 均线间隔太小不开仓
        val mas = Vector(shortMa.currentValue, midMa.currentValue, longMa.currentValue)
        val maDelta = mas.max - mas.min

        if(currentDirection == 0 || maDelta < avgSize() * 0.5 ) {
            0
        }else {
            //阳线突破均线, 且上一tick， 上一k都未突破均线
            if(currentDirection == kDirection && currentDirection != lastKDirection && currentDirection != lastTickDirection) {
                currentDirection
            }else {
                0
            }
        }
    }

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 60 && lastTick != null) {
            updateSl()

            // 现价方向
            val shortDirection   = (k.close - shortMa.currentValue).signum
            val midDirection     = (k.close - midMa.currentValue).signum
            val longDirection    = (k.close - longMa.currentValue).signum
            val currentDirection = aggDirections(
              Vector(shortDirection, midDirection, longDirection)
            )

            // 上一K方向, 跟上一均线做比较
            val lastKShortDirection = (klines.data(1).close - shortMa.data(1).value).signum
            val lastKMidDirection   = (klines.data(1).close - midMa.data(1).value).signum
            val lastKLongDirection  = (klines.data(1).close - longMa.data(1).value).signum
            val lastKDirection      = aggDirections(
              Vector(lastKShortDirection, lastKMidDirection, lastKLongDirection)
            )

            // 上一tick方向, 跟当前均线做比较
            // 如果上一tick是收盘， 则上一tick方向等于上一K方向
            val lastTickDirection = if (lastTick.end) {
                lastKDirection
            } else {
                val lastTickshortDirection = (lastTick.close - shortMa.currentValue).signum
                val lastTickmidDirection   = (lastTick.close - midMa.currentValue).signum
                val lastTicklongDirection  = (lastTick.close - longMa.currentValue).signum
                aggDirections(
                  Vector(lastTickshortDirection, lastTickmidDirection, lastTicklongDirection)
                )
            }
            // K线方向
            val kDirection = (k.close - k.open).signum

            // k线方向与当前突破方向一致
            // 突破的两种形式：
            // 1. k线实体突破
            // 2. 跳空突破
            // 有共同特征， 就是 上1k收盘在均线另一侧

            val direction = openDirection(currentDirection,lastKDirection, lastTickDirection, kDirection)

            // 不满足开仓条件了， 跟踪止盈
            if (positionMgr.hasPosition && direction == 0) {
                checkSl()
            }

            if (direction != 0 ) {
                if (
                  positionMgr.hasPosition && positionMgr.currentPosition.get.direction != direction
                ) {
                    positionMgr.closeCurrent(k, "反手")
                }
                if (!positionMgr.hasPosition) {
                    val as = avgSize()
                    positionMgr.open(
                      k,
                      k.close,
                      direction,
                      Some(k.close - (slFactor * as) * direction),
                      None,
                      false,
                      s"突破均线"
                    )

                }
            }
        }

        if (!history) {
            lastTick = k
        }
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
