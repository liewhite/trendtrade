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

// 均线顺势，价格合理， 且价格处于上涨时开仓
// 均线调头， 破均线和新低平仓, 避免来回止损
// 均线未调头，收盘价未站上均线且收阴线平仓
class MaStrategy(
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
    val macd        = MacdMetric(klines)
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
          s"load history of ${symbol} , last kline: ${klines.data(0)} ma20: ${maSeq.data(0)}"
        )
    }

    def metricTick(k: Kline) = {
        klines.tick(k)
        maSeq.tick(k)
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

    // 前一tick信息
    var lastTick: Kline = null

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20) {

            val maDirection = maSeq.maDirection

            // 开仓看均线方向, 均线向上就突破high开仓
            val openThreshold = if (maDirection == 1) lastTick.high else lastTick.low

            // 检查是否需要平仓, 同一K线上， 平仓一次， 下次平仓必须要超过该价位了, 不断放大, 限制平仓次数
            if (positionMgr.currentPosition.nonEmpty) {
                val positionDirection = positionMgr.currentPosition.get.direction
                // 平仓看持仓方向， 持有多单则跌破low平仓， 持有空单则
                val closeThreshold    = if (positionDirection == 1) lastTick.low else lastTick.high

                // ma 还未反转
                if (positionDirection == maDirection) {
                    if (
                      (k.close - positionMgr.currentPosition.get.openAt) * positionDirection < -0.2 * avgSize() // 亏损
                    ) {
                        positionMgr.closeCurrent(k, "亏损0.2倍波动")
                    } else if (
                      // 收盘确认跌破， 无论盈亏都要平仓
                      k.end &&                                      // 收盘
                      (k.close - k.open) * positionDirection < 0 && // 逆势K
                      (k.close - maSeq.data(0).value) * positionDirection <= 0                                  // 收盘价未站上均线
                    ) {
                        positionMgr.closeCurrent(k, "收盘跌破均线")
                    } else if (
                      // 价格处于逆势侧， 且破新低
                      (k.close - k.open) * positionDirection < 0 &&               // 逆势K
                      (k.open - maSeq.data(0).value) * positionDirection <= 0 &&  // 开盘价在均线劣势侧
                      (k.close - maSeq.data(0).value) * positionDirection <= 0 && // 价格在均线劣势侧
                      (k.close - closeThreshold) * positionDirection < 0 &&       // 破新低, K线边界时很容易出现反复开平仓
                      (k.close - k.open).abs > avgSize() * 0.1      // 有效K线， 过滤了开盘即平仓的尴尬
                    ) {
                        positionMgr.closeCurrent(k, "受均线压制反转")
                    }
                } else {
                    // ma已调头， 则tick破均线平仓
                    // 分两种情况
                    // 1. 从上往下跌破均线， 直接平仓没毛病
                    // 2. 从下往上突破再跌破，也应立即平仓, 大不了新高再开仓
                    // 平仓后不创新高不开仓, 有效过滤临界震荡
                    if (
                      (k.close - maSeq.data(0).value) * positionDirection <= 0
                      //   (k.close - closeThreshold) * positionDirection < 0 // 如果不创新低不平仓， 考虑反弹插针突破均线再回落， 会完整的吃回落的亏损
                    ) {
                        positionMgr.closeCurrent(k, "均线调头")
                    }
                }
            }

            val as = avgSize()
            // 考虑震荡， 只在均线顺势且价格合适的时候开仓
            // 如果是均线来回调头的震荡， 亏损比较小、
            // 稍微宽幅的震荡不会亏， 甚至还能赚钱

            // 平仓后,再判断是否需要开仓
            if (
              positionMgr.currentPosition.isEmpty &&                      // 无持仓
              maDirection != 0 &&                                         // 均线有方向
              (k.open - maSeq.data(0).value) * maDirection < as * 0.5 &&  // (其实不需要这个条件, 后两个条件包含了该条件), 开盘价不正偏离均线太多(止损在这里)
              (k.close - maSeq.data(0).value) * maDirection < as * 0.2 && // 现价不正偏离均线太多(成本优势)
              (k.close - openThreshold) * maDirection > 0                 // 只在突破当前K线端点时开仓, 避免单K内来回震荡触发开仓
            ) {
                positionMgr.open(k, k.close, maDirection, None, None)
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
