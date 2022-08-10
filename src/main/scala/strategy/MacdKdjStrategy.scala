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

// macd trend, kdj同向瞬间
class MacdKdjStrategy(
    symbol:          String,
    interval:        String,
    maxHold:         Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines      = KlineMetric()
    val macd        = MacdMetric(klines)
    val kdj         = KdjMetric(klines)
    val positionMgr = PositionMgr(symbol, trader, maxHold, ntf, exceptionNotify)
    val slFactor    = 0.75
    val tpFactor    = 1.5

    val logger = Logger("strategy")

    // 加载历史K线
    def start() = {
        loadHistory()
        // positionMgr.loadPosition()
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
        // shortMa.tick(k)
        // longMa.tick(k)
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

    var openTime: ZonedDateTime        = null
    var lastTick: Kline                = null
    var lastTickDirection: Option[Int] = None

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20 && lastTick != null) {

            val macdDirection  = macd.macdBarTrend()
            val macd1Direction = macd.macdBarTrend(1)
            val kdjDirection   = kdj.kdjRange()
            val kdj1Direction  = kdj.kdjRange(1)
            val ds             = Vector(macdDirection, kdjDirection)
            val ds1            = Vector(macd1Direction, kdj1Direction)

            val direction = if (ds.forall(_ == 1)) {
                1
            } else if (ds.forall(_ == -1)) {
                -1
            } else {
                0
            }
            val lastKDirection = if (ds1.forall(_ == 1)) {
                1
            } else if (ds1.forall(_ == -1)) {
                -1
            } else {
                0
            }

            // 上一K不满足条件， 上一tick也不满足， 只有当前满足
            if (
              direction != 0 &&
              lastKDirection != direction &&
              lastTickDirection.nonEmpty &&
              lastTickDirection.get != direction &&
              !(openTime != null &&
                  Duration.between(openTime, ZonedDateTime.now()).getSeconds() < 60)
            ) {
                val as = avgSize()
                positionMgr.loadPosition()

                if(positionMgr.hasPosition && positionMgr.currentPosition.get.direction != direction) {
                    positionMgr.closeCurrent(k,"反手")
                }

                if (!positionMgr.hasPosition) {
                    positionMgr.open(
                      k,
                      k.close,
                      macdDirection,
                      Some(k.close - (slFactor * as) * macdDirection),
                      Some(k.close + (tpFactor * as) * macdDirection),
                      true
                    )
                }
                // 休息一分钟
                openTime = ZonedDateTime.now()
            }
            lastTickDirection = Some(direction)
        }

        lastTick = k
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
