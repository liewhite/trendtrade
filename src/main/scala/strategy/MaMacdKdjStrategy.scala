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

// macd kdj ma 三指标
// macd.bar 小于前一个即可
// 三指标同向， 且价格未偏离均线， 开仓
// 平仓：
// 1. 盘中浮亏固定止损
// 2. 浮盈状态， 收盘时若回撤过均线， 且三指标多数不满足条件
class MaMacdKdjStrategy(
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
    val slFactor    = 0.5

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
        macd.tick(k)
        kdj.tick(k)
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

    def metrics = {
        Vector(ma.maDirection(), macd.macdDirection(), kdj.dDirection())
    }

    // 平仓：
    // 1. 盘中浮亏固定止损
    // 2. 浮盈状态， 收盘时若回撤过均线， 且三指标多数不满足条件
    def checkClose(): Unit = {
        if (!positionMgr.hasPosition) {
            return
        }
        val as            = avgSize()
        val position      = positionMgr.currentPosition.get
        val k             = klines.current
        val pDirection    = positionMgr.currentPosition.get.direction
        val maValue       = ma.currentValue
        val macdDirection = macd.macdBarTrend(2)
        val ds = metrics
        if (!k.end) {
            // 浮亏超过阈值
            if ((k.close - position.openAt) * position.direction < -as * slFactor) {
                // 指标被破坏
                if(!metrics.forall(_ == position.direction)) {
                    positionMgr.closeCurrent(k, "盘中止损")
                }
            }
        } else {
            // 浮盈状态， 收盘时若回撤过均线， 且三指标多数不满足条件
            if (
              (k.close - ma.currentValue) * position.direction < 0 && ds.count(
                _ == position.direction
              ) < 2
            ) {
                positionMgr.closeCurrent(k, "收盘止损")
            }
        }
    }

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20) {
            checkClose()
            val as        = avgSize()
            val ms = metrics

            val direction = if(ms.forall(_ == 1)) {
                1
            }else if(ms.forall(_ == -1)) {
                -1
            }else {
                0
            }

            if (
              direction != 0 &&
              (k.close - ma.currentValue) * direction < slFactor * as
            ) {
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
