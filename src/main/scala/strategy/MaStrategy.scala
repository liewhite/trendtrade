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

// 加上远离均线时的浮动止盈
// 顺均线方向开仓
// 止损为2.5size
class MaStrategy(
    symbol:          String,
    interval:        String,
    shortMaInterval: Int, // 短均线
    longMaInterval:  Int, // 长均线
    maxHold:         Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines      = KlineMetric()
    val shortMa     = MaMetric(klines, shortMaInterval)
    val longMa      = MaMetric(klines, longMaInterval)
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
        shortMa.tick(k)
        longMa.tick(k)
        macd.tick(k)
    }

    // 更新止损位
    def updateSl(): Unit = {
        if (!positionMgr.hasPosition) {
            return
        }

        val k     = klines.data(0)
        val as    = avgSize()
        val p     = positionMgr.currentPosition.get
        // load position 的时候没有止损
        val oldSl = p.stopLoss match {
            case None => p.openAt - p.direction * as * 1.5
            case Some(o) => o
        }

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

        //  正偏离均线的值, 优势为正， 劣势为负
        val offsetFromMaValue = (k.close - longMa.currentValue) * p.direction
        val offsetFromMa      = offsetFromMaValue / as
        val maValue           = longMa.current.value

        // 涨幅远离均线时需要叠加浮动止盈, 可以利用插针行情， 对冲反向插针造成的损失
        val (newSl, reason) = if (offsetFromMa > 20) {
            (maxSl(oldSl, maValue + offsetFromMaValue * 0.9 * p.direction, p.direction), "达到20倍波动")
        } else if (offsetFromMa > 10) {
            (maxSl(oldSl, maValue + offsetFromMaValue * 0.8 * p.direction, p.direction), "达到10倍波动")
        } else if (offsetFromMa > 5) {
            (maxSl(oldSl, maValue + offsetFromMaValue * 0.6 * p.direction, p.direction), "达到5倍波动")
        } else if (offsetFromMa > 3) {
            (maxSl(oldSl, maValue + offsetFromMaValue * 0.4 * p.direction, p.direction), "达到3倍波动")
        } else {
            (oldSl, "无止损调节需求")
        }
        if (newSl != oldSl) {
            ntf.sendNotify(
              s"${symbol} 当前价格: ${k.close} 均线: ${maValue} 平均波动: ${as} 移动止损位: ${oldSl} -> ${newSl}, 原因: ${reason}"
            )
        }
        positionMgr.updateSl(Some(newSl))
    }

    def checkSl(): Boolean = {
        if (positionMgr.hasPosition) {
            val k            = klines.data(0)
            val p            = positionMgr.currentPosition.get
            val currentPrice = k.close
            if ((currentPrice - p.stopLoss.get) * p.direction < 0) {
                positionMgr.closeCurrent(k, "触发移动止盈平仓")
                true
            } else {
                false
            }
        } else {
            false
        }
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

    // 检查平仓
    // 在浮动止盈后进行
    // 难点在平仓后又瞬间开仓， 失去平仓意义。
    // 收盘时， 均线调头或者跌破止损位平仓
    def checkClose() = {
        val maDirection = longMa.maDirection
        val k           = klines.current
        // 有持仓， k线收盘
        if (positionMgr.currentPosition.nonEmpty && k.end) {
            if (maDirection != positionMgr.currentPosition.get.direction) {
                positionMgr.closeCurrent(k, "均线调头")
            }
        }
    }

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20) {
            updateSl()
            checkSl()
            checkClose()

            val maDirection = longMa.maDirection

            if (
              openTime != null && Duration.between(openTime, LocalDateTime.now()).getSeconds() < 60
            ) {
                return
            }

            val as = avgSize()
            if (
              positionMgr.currentPosition.isEmpty && // 无持仓
              maDirection != 0 &&                    // 均线有方向
              macd.macdDirection == maDirection &&   // macd方向一致
              (k.close - longMa.currentValue) * maDirection < as * 0.2 &&  // 现价不正偏离均线太多(成本优势)
              (k.close - k.open) * maDirection > 0.1 * as   // 阳线
            ) {
                // 如果还在下跌， macd应该能体现
                positionMgr.open(k, k.close, maDirection, Some(k.close - (1.5 * as) * maDirection), None, false)
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
