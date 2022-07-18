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

// 均线顺势， 价格
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
          s"load history of ${symbol}"
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

    var openTime: LocalDateTime = null

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20) {

            val maDirection = maSeq.maDirection

            // 检查是否需要平仓, 同一K线上， 平仓一次， 下次平仓必须要超过该价位了, 不断放大, 限制平仓次数
            if (positionMgr.currentPosition.nonEmpty) {
                val as                = avgSize()
                val positionDirection = positionMgr.currentPosition.get.direction

                // 阴线才考虑平仓
                if ((k.close - k.open) * positionDirection < 0) {
                    // ma 还未反转
                    if (positionDirection == maDirection) {
                        val preK = klines.data(1)
                        if (
                          ((k.close - maSeq.data(0).value) * positionDirection < 0 &&
                              k.end) ||                                                                      // 收在劣势侧
                          ((k.open - maSeq.data(0).value) * positionDirection <= 0 &&
                              (preK.close - preK.open) * positionDirection < 0)                              // 上一K线收阴线， 后一K开在劣势侧
                        ) {
                            positionMgr.closeCurrent(k, "劣势侧收阴线")
                        } else if (
                          (k.close - positionMgr.currentPosition.get.openAt) * positionDirection < -1.2 * as // 保证正常波动不会出局
                        ) {
                            // 浮亏强制止损
                            positionMgr.closeCurrent(k, "浮亏止损")
                        }
                    } else {
                        // 抖动条件: 阴阳线临界点 + 均线调头临界点 重合
                        // 避免方法： 开仓要求阳线且有一定涨幅
                        if (
                          (k.close - maSeq.data(0).value) * positionDirection <= 0 // 均线调头,价格在劣势侧
                        ) {
                            positionMgr.closeCurrent(k, "均线调头")
                        }
                    }
                }
            }

            if (
              openTime != null && Duration.between(openTime, LocalDateTime.now()).getSeconds() < 60
            ) {
                return
            }

            val as = avgSize()
            if (
              positionMgr.currentPosition.isEmpty &&                      // 无持仓
              maDirection != 0 &&                                         // 均线有方向
              macd.macdDirection == maDirection && // macd方向一致
            //   maSeq.historyMaDirection(1) == maDirection &&
            //   maSeq.historyMaDirection(2) == maSeq.historyMaDirection(1) &&
              (k.close - maSeq.data(0).value) * maDirection < as * 0.2 && // 现价不正偏离均线太多(成本优势)
            //   (k.close - maSeq.data(0).value) * maDirection > 0 && // 在均线上
              (k.close - k.open) * maDirection > 0 &&                     // 阳线
              (k.close - k.open) * maDirection > avgSize() * 0.1          // 有效K线， 过滤了开盘即平仓的尴尬, 以及下跌中的无限抄底
            ) {
                positionMgr.open(k, k.close, maDirection, None, None)
                // 休息一分钟
                openTime = LocalDateTime.now()
            }
        }
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
