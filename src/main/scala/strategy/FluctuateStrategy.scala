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

// 由趋势策略改良
// 价格处于均线劣势侧且出现一定幅度的顺势K线开仓
// 设置适当的止盈止损
class FluctuateStrategy(
    symbol:          String,
    interval:        String,
    maSize:          Int,
    maxHold:         Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines      = KlineMetric()
    val maSeq       = MaMetric(klines, maSize)
    val positionMgr = PositionMgr(symbol, trader, maxHold, ntf, exceptionNotify)

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
    }

    def metricTick(k: Kline) = {
        klines.tick(k)
        maSeq.tick(k)
        // macd.tick(k)
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

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 每根K线结束都要清理持仓
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20) {
            if (positionMgr.hasPosition) {
                return
            }

            val maDirection = maSeq.maDirection

            val as = avgSize()

            val openThreshold = if (maDirection == 1) lastTick.high else lastTick.low
            // 平仓后,再判断是否需要开仓
            if (
              maDirection != 0 &&                                         // 均线有方向
              maSeq.historyMaDirection(1) == maDirection &&
              maSeq.historyMaDirection(2) == maSeq.historyMaDirection(1) &&
              (k.close - maSeq.data(0).value) * maDirection < as * 0.1 && // 不正偏离均线太多
              (k.high - k.low) > avgSize() * 0.2 &&                       // 有效K线
              (k.close - k.open) * maDirection > 0                        // 阳线
            ) {
                val positions = trader.getPositions(symbol)
                if (positions.length != 0) {
                    return
                }
                positionMgr.open(
                  k,
                  k.close,
                  maDirection,
                  Some(k.close - (as * 1) * maDirection),
                  Some(k.close + (as * 1.25) * maDirection)
                )
            }
        }
        if (k.end) {
            positionMgr.cleanPosition()
        }
        lastTick = k
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
