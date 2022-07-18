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

// 趋势跟踪
// 开仓和均线策略一致
// 开仓后以K线下沿止损
// 浮盈后跟踪止盈
class TrendTrackStrategy(
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
        if(positionMgr.hasPosition) {
            positionMgr.updateSl(Some(positionMgr.currentPosition.get.openAt))
        }
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

    // 更新止损位
    def updateSl(): Unit = {
        if (!positionMgr.hasPosition) {
            return
        }
        val k     = klines.data(0)
        val as    = avgSize()
        val p     = positionMgr.currentPosition.get
        val oldSl = p.stopLoss.get

        def maxSl(o: BigDecimal, n: BigDecimal, d: Int): BigDecimal = {
            if (d == 1) {
                // 多单， 取最大值
                Vector(o, n).max
            } else if (d == -1) {
                // 空单， 取最小值
                Vector(o, n).min
            } else {
                throw Exception(s"持仓方向为0: ${p}")
            }
        }

        //  利润 / 平均size, >0盈利， <0 亏损
        val profit = (k.close - p.openAt) * p.direction
        val profitForAvgSize = profit / as

        val newSl = if (profitForAvgSize > 10) {
            // 浮盈大于10倍k线size, 跟踪止盈到最大盈利的80%
            maxSl(oldSl, k.open + profit * 0.8 * p.direction, p.direction)
        } else if (profitForAvgSize > 5) {
            // 浮盈大于5倍k线size, 跟踪止盈到最大盈利的60%
            maxSl(oldSl, k.open + profit * 0.6 * p.direction, p.direction)
        } else if (profitForAvgSize > 1) {
            // 浮盈大于1倍size， 跟踪止盈到最大盈利的30%
            maxSl(oldSl, k.open + profit * 0.3 * p.direction, p.direction)
        } else if (profitForAvgSize > 0.5) {
            // 浮盈大于0.5倍size， 止损拉到成本线
            maxSl(oldSl, p.openAt, p.direction)
        } else {
            // 浮盈不超过0.5倍, 0.5倍止损, 开仓时已经设置
            p.stopLoss.get
        }
        if(newSl != oldSl) {
            ntf.sendNotify(s"${symbol} 移动止损位: ${oldSl} -> ${newSl}")
        }
        positionMgr.updateSl(Some(newSl))
    }

    var openTime: LocalDateTime = null

    def checkSl() = {
        if (positionMgr.hasPosition) {
            val k            = klines.data(0)
            val p            = positionMgr.currentPosition.get
            val currentPrice = k.close
            if ((currentPrice - p.stopLoss.get) * p.direction < 0) {
                positionMgr.closeCurrent(k, "触发止损位平仓")
            }
        }

    }

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20) {
            // 更新止损位
            updateSl()
            // 止损
            checkSl()

            val maDirection = maSeq.maDirection

            if (
              openTime != null && Duration.between(openTime, LocalDateTime.now()).getSeconds() < 60
            ) {
                return
            }

            val as = avgSize()
            if (
              positionMgr.currentPosition.isEmpty &&                      // 无持仓
              maDirection != 0 &&                                         // 均线有方向
              macd.macdDirection == maDirection &&                        // macd方向一致
              (k.close - maSeq.data(0).value) * maDirection < as * 0.2 && // 现价不正偏离均线太多(成本优势)
              (k.close - k.open) * maDirection > 0 &&                     // 阳线
              (k.close - k.open) * maDirection > avgSize() * 0.1          // 有效K线， 过滤了开盘即平仓的尴尬, 以及下跌中的无限抄底
            ) {
                // 0.5倍波动止损
                val sl = k.close - as * 0.5 * maDirection
                positionMgr.open(k, k.close, maDirection, Some(sl), None, false)
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
