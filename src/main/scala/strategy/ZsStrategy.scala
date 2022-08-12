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

// 考虑怎么长线持仓
// 

// 基于中枢的策略
// 均线具有吸附作用， 所以当价格远离均线， 出现衰竭时可以反向开仓
// 均线还具有支撑作用， 当价格吸附到均线附近但是再次出现远离,即出现支撑作用，可能要发生单边行情， 顺势开仓
// 有幅度要求， 即吸附原则开仓时， 必须远离一定的距离， 才有盈利空间
// 当支撑原则开仓时， 要求价格一定是从远处吸附过来再形成支撑的
class ZsStrategy(
    symbol:          String,
    interval:        String,
    zsMaInterval:    Int, // 中枢参数
    maxHold:         Int,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines      = KlineMetric()
    val czscK       = CzscKMetric(klines)
    val zs          = MaMetric(klines, zsMaInterval)
    val macd        = MacdMetric(klines)
    val positionMgr = PositionMgr(symbol, trader, maxHold, ntf, exceptionNotify)
    // val slFactor    = 0.3

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
        czscK.tick(k)
        zs.tick(k)
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

    def hasProfit: Boolean = {
        (positionMgr.currentPosition.get.openAt - klines.current.close) > 0
    }

    var lastDi: Option[BigDecimal]   = None
    var lastDing: Option[BigDecimal] = None

    def doTick(k: Kline, history: Boolean = false): Unit = {
        metricTick(k)
        // if(k.end) {
        //     println(s"${k.datetime} ${czscK.fenxing()}")
        // }
        // return
        // 忽略历史数据， 只处理实时数据
        if (!history && klines.data.length >= 20 && k.end) {
            val as            = avgSize()
            // 找到中枢
            val fenxing       = czscK.fenxing()
            // 没有分型， 没有操作空间
            if (fenxing == 0) {
                return
            }
            val macdDirection = macd.macdDirection()
            // 有持仓
            if (positionMgr.hasPosition) {
                if (hasProfit) {
                    // 有利润， 反向分型加 macd 平仓
                    if (
                      positionMgr.currentPosition.get.direction != fenxing && macdDirection == fenxing
                    ) {
                        positionMgr.closeCurrent(k, " 反向分型 + macd 逆势")
                    }
                } else {
                    // 无利润， 反向分型立即平仓
                    if (positionMgr.currentPosition.get.direction != fenxing) {
                        positionMgr.closeCurrent(k, " 反向分型 + 亏损")
                    }
                }

            }

            if (!positionMgr.hasPosition) {
                // 如果没有持仓或刚平仓了， 判断是否需要开仓
                // 偏离中枢的方向
                // 一类买卖点, 分型方向与偏离方向相反
                // 价格偏离达到一定程度， 并且出现反向分型 + macd
                if (
                  (k.close - (zs.currentValue - as * fenxing)) * fenxing < 0 && macdDirection == fenxing
                ) {
                    positionMgr.open(
                      k,
                      k.close,
                      fenxing,
                      None,
                      None,
                      false,
                      s"一类, 平均波动${as}, 距离均线 ${(k.close - zs.currentValue).abs}"
                    )
                } else if (macdDirection == fenxing) {
                    // 三类买卖点， 分型方向与偏离方向相同
                    // 要求分型的最低点不能与中枢有交集
                    val k1 = czscK.data(1)
                    // 底分型收盘大于中枢
                    // 且上一个顶分型收盘价高于当前价
                    if (fenxing == 1) {
                        if (
                          k.close > zs.currentValue && lastDing.nonEmpty && k.close < lastDing.get
                        ) {
                            positionMgr.open(
                              k,
                              k.close,
                              fenxing,
                              None,
                              None,
                              false,
                              "三类"
                            )
                        }
                    } else {
                        if (k.close < zs.currentValue && lastDi.nonEmpty && k.close > lastDi.get) {
                            positionMgr.open(
                              k,
                              k.close,
                              fenxing,
                              None,
                              None,
                              false,
                              "三类"
                            )
                        }

                    }
                }
            }
            if (fenxing == 1) {
                lastDi = Some(k.close)
            } else if (fenxing == -1) {
                lastDing = Some(k.close)
            }
        }
    }

    def tick(k: Kline, history: Boolean = false): Unit = {
        this.synchronized {
            doTick(k, history)
        }
    }
}