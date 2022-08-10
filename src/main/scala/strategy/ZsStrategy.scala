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

// 基于中枢的策略
// 选择一个均线作为中枢
// 价格离开中枢，当出现反向分型, 收盘k线与中枢没有交集， 且macd开始衰竭, 开仓, 回到中枢时后出现反向分型， 且macd反向运动， 平仓
// 如果顶分型后没有回到中枢，就出现了底分型， 平仓， 如果macd顺势， 则当作第三类买卖点
class ZsStrategy(
    symbol:          String,
    interval:        String,
    zsMaInterval:    Int, // 中枢参数
    maxHold:         Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines      = KlineMetric()
    val czscK       = CzscKMetric(klines)
    val zs          = MaMetric(klines, zsMaInterval)
    val macd        = MacdMetric(klines)
    val positionMgr = PositionMgr(symbol, trader, maxHold, ntf, exceptionNotify)
    // val slFactor    = 0.3

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
        czscK.tick(k)
        zs.tick(k)
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

    def hasProfit: Boolean = {
        (positionMgr.currentPosition.get.openAt - klines.current.close) > 0
    }

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // return
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20 && k.end) {
            val as            = avgSize()
            // 找到中枢
            val fenxing       = czscK.fenxing()
            // 没有分型， 没有操作空间
            if (fenxing == 0) {
                return
            }
            val macdDirection = macd.macdDirection()
            // 有持仓
            if (positionMgr.hasPosition) {
                if (hasProfit) {
                    // 有利润， 反向分型加 macd 平仓
                    if (
                      positionMgr.currentPosition.get.direction != fenxing && macdDirection == fenxing
                    ) {
                        positionMgr.closeCurrent(k, " 反向分型 + macd 逆势")
                    }
                } else {
                    // 无利润， 反向分型立即平仓
                    if (positionMgr.currentPosition.get.direction != fenxing) {
                        positionMgr.closeCurrent(k, " 反向分型 + 亏损")
                    }
                }

            }

            if (!positionMgr.hasPosition) {
                // 如果没有持仓或刚平仓了， 判断是否需要开仓
                // 偏离中枢的方向
                // 一类买卖点, 分型方向与偏离方向相反
                // 价格偏离达到一定程度， 并且出现反向分型 + macd
                if (
                  (k.close - (zs.currentValue - as * fenxing)).signum == -fenxing && macdDirection == fenxing
                ) {
                    positionMgr.open(
                      k,
                      k.close,
                      fenxing,
                      None,
                      None,
                      false
                    )
                } else if (
                  (k.close - (zs.currentValue + 0.2 * as * fenxing)).signum == -fenxing && macdDirection == fenxing
                ) {
                // 三类买卖点， 分型方向与偏离方向相同
                    positionMgr.open(
                      k,
                      k.close,
                      fenxing,
                      None,
                      None,
                      false
                    )
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
