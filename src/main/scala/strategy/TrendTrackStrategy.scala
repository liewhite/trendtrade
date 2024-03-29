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

// 趋势跟踪
// kdj 金叉死叉 + macd 顺势
// 移动止损
// 出现反向信号反手开仓
class TrendTrackStrategy(
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
    val kdj         = KdjMetric(klines)
    val positionMgr = PositionMgr(symbol, trader, maxHold, ntf, exceptionNotify)

    val logger = Logger("strategy")

    // 加载历史K线
    def start() = {
        loadHistory()
        positionMgr.loadPosition()
        // if (positionMgr.hasPosition) {
        //     positionMgr.updateSl(Some(positionMgr.currentPosition.get.openAt))
        // }
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
        kdj.tick(k)
        macd.tick(k)
        ma.tick(k)
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

    // 更新止损位
    def updateSl(): Unit = {
        if (!positionMgr.hasPosition) {
            return
        }
        val k     = klines.data(0)
        val as    = avgSize()
        val p     = positionMgr.currentPosition.get
        val oldSl = p.stopLoss match {
            case None => p.openAt - p.direction * as * 1.5
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

    var openTime: ZonedDateTime = null

    def checkSl(): Boolean = {
        if (positionMgr.hasPosition) {
            val k            = klines.data(0)
            val p            = positionMgr.currentPosition.get
            val currentPrice = k.close
            if ((currentPrice - p.stopLoss.get) * p.direction < 0) {
                positionMgr.closeCurrent(k, "触发止损位平仓")
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20) {
            // 更新止损位
            updateSl()
            checkSl()

            val macdDirection      = macd.macdDirection()
            val kdjDirection       = kdj.kdjCrossDirection()
            // 低位金叉， 高位死叉
            val strictKdjDirection = kdj.kdjCrossDirection(strict = true)

            if (
              openTime != null && Duration.between(openTime, ZonedDateTime.now()).getSeconds() < 60
            ) {
                return
            }

            val as     = avgSize()
            def open() = {
                if (
                  openTime != null && Duration
                      .between(openTime, ZonedDateTime.now())
                      .getSeconds() < 60
                ) {} else {
                    // 1倍波动止损
                    val sl = k.close - as * 1.5 * macdDirection
                    positionMgr.open(k, k.close, macdDirection, Some(sl), None, false)
                    // 休息一分钟
                    openTime = ZonedDateTime.now()
                }
            }

            // 开盘的时候或者收盘的时候才开仓
            // 均线顺势逆势偏移范围不同
            val maOffset =
            //      if (ma.maDirection == macdDirection) {
            //     0.2 * as
            // } else {
            //     -2 * as
            // }

            if (
              k.end &&
              macdDirection != 0 &&
              kdjDirection == macdDirection
            ) {
                if (
                  positionMgr.currentPosition.nonEmpty &&
                  positionMgr.currentPosition.get.direction == -macdDirection
                ) {
                    // 出现反向金叉死叉， 平仓但暂不反手
                    positionMgr.closeCurrent(k, "形态反转,平仓")
                }

                // 没有仓位
                // kdj严格金叉
                // 均线跟kdj同向加速
                if (
                  positionMgr.currentPosition.isEmpty &&
                  strictKdjDirection == kdjDirection &&
                  ((ma.data(0).value - ma.data(1).value) - (ma.data(1).value - ma.data(2).value)).signum == kdjDirection &&
                  ((ma.data(1).value - ma.data(2).value) - (ma.data(2).value - ma.data(3).value)).signum == kdjDirection
                ) {
                    open()
                }
            }
        }
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
