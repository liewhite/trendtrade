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

// 以收盘价开仓, tick止盈止损
// kdj叉， macd顺势， 价格处于均线劣势方且大于平均波动的5倍
// 以tick为准
// 止盈2倍波动值， 止损1倍
class KdjStrategy(
    symbol:          String,
    interval:        String,
    maSize:          Int,
    maxHolds:        Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines = KlineMetric()
    val ma     = MaMetric(klines, maSize)
    val macd   = MacdMetric(klines)
    val kdj    = KdjMetric(klines)
    val logger                            = Logger("strategy")

    val positionMgr = PositionMgr(symbol, trader, maxHolds, ntf, exceptionNotify)

    // 加载历史K线
    def start() = {
        loadHistory()
        // 开始websocket
        trader.subscribeKlines(symbol, interval, k => tick(k))
    }


    // 币安是以k线开始时间为准的
    def loadHistory() = {
        val history = trader.getHistory(symbol, interval)
        // 去掉第一条
        history.dropRight(1).foreach(tick(_, true))
        logger.info(
          s"load history of ${symbol} , last kline: ${klines.data(0)} ma: ${ma.data(0)}"
        )
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
        kdj.tick(k)
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (history) {
            return
        }
        // 历史数据不足， 无法参考
        if (klines.data.length < maSize) {
            return
        }
        if (!k.end) {
            return
        }

        // 无持仓才开仓
        if (positionMgr.currentPosition.isEmpty) {
            // logger.info(s"${symbol} ${kdj.data(0).k} ${kdj.data(0).d} ${kdj.data(0).j}")
            // kdj叉， macd顺势
            val kdj0    = kdj.data(0)
            val kdj1    = kdj.data(1)
            // kdj方向, 金叉死叉，或者收敛方向
            val kdjDir  = if (kdj1.j > 80 && kdj1.j > kdj1.d && kdj1.j > kdj0.j) {
                -1
            } else if (kdj1.j < 20 && kdj1.j < kdj1.d && kdj1.j < kdj0.j) {
                1
            } else {
                0
            }
            val macdDir = macd.macdDirection
            val maValue = ma.data(0).value

            if (kdjDir != 0) {
                val az = avgSize()
                if (
                  macdDir == kdjDir &&
                  ((k.close - maValue) * macdDir < 0 || (k.close - maValue).abs < az * 0.2)
                ) {
                    val positions = trader.getPositions(symbol)
                    if (positions.length != 0) {
                        return
                    }
                    // 1倍止损， 1倍止盈
                    logger.info(
                      s"触发开仓: ${symbol}, price: ${k.close} ma: ${ma.data(1).value},${ma.data(0).value} kdj: ${kdj
                              .data(1)}, ${kdj.data(1)} macd: ${macd.data(1).bar},${macd.data(0).bar}"
                    )
                    positionMgr.open(k,k.close, kdjDir, Some(k.close - (az * 1.5) * kdjDir), Some(k.close + (az * 1.5) * kdjDir))
                }
            }
        }
    }
}
