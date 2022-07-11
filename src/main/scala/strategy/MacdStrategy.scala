package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
import sttp.client3._
import binance.BinanceApi
import com.typesafe.scalalogging.Logger
import binance.TradeSide
import java.util.concurrent.TimeoutException
import java.time.Instant
import java.time.ZoneId
import java.time.Duration
import notifier.Notify

// macd 金叉买， 死叉卖， 止盈止损 (-2,3)
// 出现反向信号要反手
class MacdStrategy(
    symbol:          String,
    interval:        String,
    maInterval:      Int,
    maxHolds:        Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines = KlineMetric()
    val macd   = MacdMetric(klines)
    val ma     = MaMetric(klines, maInterval)
    val logger = Logger("strategy")

    val positionMgr = PositionMgr(symbol, trader, maxHolds, ntf, exceptionNotify)

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

    def metricTick(k: Kline) = {
        klines.tick(k)
        ma.tick(k)
        macd.tick(k)
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (history) {
            return
        }
        // 历史数据不足， 无法参考
        if (klines.data.length < 20) {
            return
        }
        val direction = macd.macdCross()
        val as        = avgSize()

        // 当前K线已经开仓过了
        if(positionMgr.hasPosition) {
            return
        }

        if (
          direction != 0 &&                                            // 金叉死叉发生
          (k.close - ma.data(0).value) * direction < 0.2 * as &&       // 价格有成本优势
          Range(1, 6).map(macd.macdCross(_)).forall(item => item == 0) // 金叉死叉发生前， 反向趋势持续至少5个周期
        ) {
            // 当前无持仓才开仓
            val positions = trader.getPositions(symbol)
            if (positions.length != 0) {
                return
            }
            positionMgr.open(
              k,
              k.close,
              direction,
              Some(k.close - (as * 1.5) * direction),
              Some(k.close + (as * 1.5) * direction)
            )
        }

        // k线结束清理持仓信息， 避免以后不能开仓了
        if(k.end) {
            positionMgr.cleanPosition()
        }
    }
}
