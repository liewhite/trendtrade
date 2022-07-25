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

// 突破所有均线, 且短期均线处于优势侧
// 结合浮动止盈
class MasStrategy(
    symbol:          String,
    interval:        String,
    shortMaInterval: Int, // 短均线
    // midMaInterval:   Int, // 中均线
    longMaInterval:  Int, // 长均线
    maxHold:         Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines      = KlineMetric()
    val shortMa     = MaMetric(klines, shortMaInterval)
    // val midMa       = MaMetric(klines, midMaInterval)
    val longMa      = MaMetric(klines, longMaInterval)
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
        // midMa.tick(k)
        longMa.tick(k)
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
            case None    => p.openAt - p.direction * as * 1
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

        //  todo 远离均线才止盈
        //  利润 / 平均size, >0盈利， <0 亏损
        val profit           = (k.close - p.openAt) * p.direction
        val profitForAvgSize = profit / as

        val (newSl, reason) = if (profitForAvgSize > 20) {
            // 浮盈大于20倍k线size, 跟踪止盈到最大盈利的90%
            (maxSl(oldSl, p.openAt + profit * 0.9 * p.direction, p.direction), "达到20倍波动")
        } else if (profitForAvgSize > 10) {
            // 浮盈大于10倍k线size, 跟踪止盈到最大盈利的80%
            (maxSl(oldSl, p.openAt + profit * 0.8 * p.direction, p.direction), "达到10倍波动")
        } else if (profitForAvgSize > 5) {
            // 浮盈大于5倍k线size, 跟踪止盈到最大盈利的60%
            (maxSl(oldSl, p.openAt + profit * 0.6 * p.direction, p.direction), "达到5倍波动")
        } else if (profitForAvgSize > 3) {
            // 浮盈大于3倍k线size, 跟踪止盈到最大盈利的40%
            (maxSl(oldSl, p.openAt + profit * 0.4 * p.direction, p.direction), "达到3倍波动")
        } else if (profitForAvgSize > 1.5) {
            // 浮盈大于1倍size， 保本出
            (maxSl(oldSl, p.openAt + profit * 0.2 * p.direction, p.direction), "达到1.5倍波动")
        } else if (profitForAvgSize <= 0.5) {
            // 几乎无盈利或浮亏， 0.8倍平均size止损
            // 当波动越来越小， 止损也越来越小
            // 反之， 波动大， 止损就大， 跟随市场
            (maxSl(oldSl, p.openAt - as * 1 * p.direction, p.direction), "无浮盈")
        } else {
            // 应该不会执行到这里
            (oldSl, "无止损调节需求")
        }
        if (newSl != oldSl) {
            ntf.sendNotify(
              s"${symbol} 利润: ${profit} 平均波动: ${as} 移动止损位: ${oldSl} -> ${newSl}, 原因: ${reason}"
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

    var openTime: ZonedDateTime = null
    var lastTick: Kline         = null

    // 收盘跌破两根均线平仓
    def checkClose(): Unit = {
        if (!positionMgr.hasPosition || !klines.current.end) {
            return
        }

        val k = klines.current

        val pDirection = positionMgr.currentPosition.get.direction
        if (positionMgr.currentPosition.nonEmpty) {
            // 跌破两根均线平仓
            if (
              (k.close - shortMa.currentValue) * pDirection <= 0 && // 跌破短期均线
              //   (k.close - midMa.currentValue) * pDirection <= 0,
              (k.close - longMa.currentValue) * pDirection <= 0 &&  // 跌破长期均线
              (k.close - k.open) * pDirection <= 0                  // 阴线
            ) {
                positionMgr.closeCurrent(k, "收盘跌破两根均线")
            }
        }
    }

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 60 && lastTick != null) {
            updateSl()
            checkSl()
            checkClose()

            if (
              openTime != null && Duration.between(openTime, ZonedDateTime.now()).getSeconds() < 60
            ) {
                return
            }

            val direction = (shortMa.currentValue - longMa.currentValue).signum

            val lastTickLongMa = if (lastTick.end) longMa.data(1).value else longMa.currentValue
            val lastTickShorMa = if (lastTick.end) shortMa.data(1).value else shortMa.currentValue

            val as = avgSize()
            // 突破均线
            if (
              //   positionMgr.currentPoition.isEmpty && // 无持仓
              direction != 0 &&                     // 突破所有均线,且本tick刚突破
              (k.close - k.open) * direction > 0 && // 阳线
              (k.close - longMa.currentValue) * direction >= 0 &&
              (k.close - shortMa.currentValue) * direction >= 0 &&
              !((lastTick.close - lastTickShorMa) * direction >= 0 &&
                  (lastTick.close - lastTickLongMa) * direction >= 0)
            ) {
                if (
                  positionMgr.hasPosition && positionMgr.currentPosition.get.direction != direction
                ) {
                    positionMgr.closeCurrent(k, "反手")
                }
                if (!positionMgr.hasPosition) {
                    positionMgr.open(
                      k,
                      k.close,
                      direction,
                      Some(k.close - (1 * as) * direction),
                      None,
                      false,
                      s"lasTick: ${lastTick.close} current: ${k.close} current ma: ${longMa.currentValue}, ${shortMa.currentValue}, last ma: ${lastTickLongMa}, ${lastTickShorMa}"
                    )

                }
                // 休息一分钟
                openTime = ZonedDateTime.now()
            }
        }

        if (!history) {
            lastTick = k
        }
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}
