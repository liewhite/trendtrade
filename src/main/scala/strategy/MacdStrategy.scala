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
    val klines240    = KlineMetric()
    val macd1       = MacdMetric(klines1)
    val macd5       = MacdMetric(klines5)
    val macd15      = MacdMetric(klines15)
    val macd60      = MacdMetric(klines60)
    val macd240      = MacdMetric(klines60)
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
        val history = trader.getHistory(symbol, minInterval)
        // 去掉第一条
        history.dropRight(1).foreach(tick(_, true))
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

    def metricTick(k: Kline) = {
        val t = k.datetime.toInstant().toEpochMilli()
        klines1.tick(k)
        // if(t.plusSeconds(60) (5 * 60) == 0) {

        // }
        // todo 不同周期
        macd1.tick(k)
    }

    var lastTick: Kline                                = null
    def tick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (history) {
            return
        }
        // 历史数据不足， 无法参考
        if (klines240.data.length < 30) {
            return
        }

    }
}
