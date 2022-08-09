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

// macd trend(2), ma 同向
// tick突破均线时开仓
// 收盘跌破均线平仓
class MaMacdStrategy(
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
        macd.tick(k)
        // kdj.tick(k)
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

    var lastTick: Kline = null

    def checkClose(): Unit = {
        if (!positionMgr.hasPosition) {
            return
        }
        val k             = klines.current
        val pDirection    = positionMgr.currentPosition.get.direction
        val maValue       = ma.currentValue
        val macdDirection = macd.macdBarTrend(2)
        // 收盘跌破均线, 且macd 反向
        if (k.end && (k.close - maValue) * pDirection < 0 && macdDirection == -pDirection) {
            positionMgr.closeCurrent(k, "指标不支持持仓方向")
        }
    }

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20 && lastTick != null) {
            checkClose()

            val direction = if (ma.currentValue == 1 && macd.macdBarTrend(2) == 1) {
                1
            } else if (ma.currentValue == -1 && macd.macdBarTrend(2) == -1) {
                -1
            } else {
                0
            }
            val as        = avgSize()

            val lastMa = if (lastTick.end) {
                ma.data(1).value
            } else {
                ma.currentValue
            }

            if (
              direction != 0 &&
              (k.close - ma.currentValue) * direction > 0 &&
              (lastTick.close - lastMa) * direction <= 0
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

        lastTick = k
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
