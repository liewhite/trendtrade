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

// kdj叉， macd顺势， 价格处于均线劣势方且大于平均波动的5倍
// 以tick为准
// 止盈2倍波动值， 止损1倍
class KdjStrategy(
    symbol:          String,
    interval:        String,
    trader:          BinanceApi,
    ntf:             Notify,
    exceptionNotify: Notify
) {
    val klines = KlineMetric()
    val ma     = MaMetric(klines, 20)
    val macd   = MacdMetric(klines)
    val kdj    = KdjMetric(klines)

    val logger                            = Logger("strategy")
    var currentPosition: Option[Position] = None
    val closed                            = mutable.ListBuffer.empty[Position]

    // 加载历史K线
    def start() = {
        loadHistory()
        // 开始websocket
        trader.subscribeKlines(symbol, interval, k => tick(k))
    }

    def symbolMeta = trader.symbolMeta(symbol)

    // 币安是以k线开始时间为准的
    def loadHistory() = {
        val history = trader.getHistory(symbol, interval)
        // 去掉第一条
        history.dropRight(1).foreach(tick(_, true))
        logger.info(
          s"load history of ${symbol} , last kline: ${klines.data(0)} ma: ${ma.data(0)}"
        )
    }

    def formatQuantity(n: BigDecimal): BigDecimal = {
        BigDecimal((n / symbolMeta.stepSize).intValue) * symbolMeta.stepSize
    }
    def formatPrice(n: BigDecimal): BigDecimal = {
        BigDecimal((n / symbolMeta.priceStep).intValue) * symbolMeta.priceStep
    }

    // 发送订单， 等待成交
    // 止盈止损
    def open(direction: Int, stopLoss: BigDecimal, tp: BigDecimal): Unit = {
        if (currentPosition.nonEmpty) {
            return
        }
        // 查询账户总额， 余额, 如果余额小于总额的10%()， 放弃开仓
        val balances    = trader.getTotalBalance()
        if (balances._2 * 10 < balances._1) {
            // NOTE: 做好合约账户被爆90%的准备,千万不能入金太多, 最多放可投资金的1/4, 这样被爆了还有机会翻
            val msg = s"余额不足10%, 停止开仓 ${balances._2}/${balances._1}"
            logger.warn(msg)
            ntf.sendNotify(msg)
            return
        }
        val k           = klines.data(0)
        val price       = k.close
        // 按精度取近似值
        val rawQuantity = ((balances._1 * 0.1) / price * trader.leverage)
        val quantity    = formatQuantity(rawQuantity)
        val side        = if (direction == 1) TradeSide.BUY else TradeSide.SELL
        val msg         = s"触发开仓 ${symbol}, ${side} ${quantity}, k: ${k}"
        logger.info(msg)
        ntf.sendNotify(msg)
        try {
            trader.sendOrder(symbol, side, quantity, Some(formatPrice(stopLoss)), Some(formatPrice(tp)))
            val msg = s"开仓成功 ${symbol}, ${side} ${quantity} sl: ${stopLoss} tp: ${tp}"
            logger.info(msg)
            ntf.sendNotify(msg)
            currentPosition = Some(
              Position(
                quantity,
                k.datetime,
                direction,
                k.close,
                None,
                None,
                Some(stopLoss),
                None
              )
            )
        } catch {
            case e: TimeoutException => {
                val msg = s" ${symbol} 挂单未成交， 请手动取消开仓挂单, ${k}"
                logger.error(msg)
                exceptionNotify.sendNotify(msg)
            }
            case e: Exception => {
                val msg = s"${symbol} 开仓失败， 请检查账户是否存在不一致"
                logger.warn(msg)
                exceptionNotify.sendNotify(msg)
            }
        }
    }

    // 除当前K外的最近k线平均大小
    def avgSize(): BigDecimal = {
        val entities = klines.data
            .slice(1, 21)
            .map(item => {
                if (item.close == item.open) {
                    BigDecimal(0)
                } else {
                    (item.close - item.open).abs
                }
            })

        val avgEntitySize = entities.sum / entities.length
        avgEntitySize
    }

    def metricTick(k: Kline)                           = {
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
        if (klines.data.length < 20) {
            return
        }
        if(!k.end) {
            return
        }

        // 无持仓才开仓
        if (currentPosition.isEmpty) {
            // logger.info(s"${symbol} ${kdj.data(0).k} ${kdj.data(0).d} ${kdj.data(0).j}")
            // kdj叉， macd顺势， 价格处于均线劣势方且大于平均波动的5倍
            // 以tick为准
            // 止盈4倍波动值， 止损2倍
            val kdjDir  = kdj.kdjDirection
            val macdDir = macd.macdDirection
            val maValue = ma.data(0).value

            if (kdjDir != 0) {
                // logger.info(s"${symbol} kdj ${if(kdjDir > 0) "金叉" else "死叉"}")
                val az = avgSize()
                if (
                  macdDir == kdjDir &&
                  ((k.close - maValue) * macdDir < 0 || (k.close - maValue).abs < az * 0.2)
                ) {
                    val positions = trader.getPositions(symbol)
                    if (positions.length != 0) {
                        return
                    }
                    // 1倍止损， 2倍止盈
                    logger.info(
                      s"触发开仓: ${symbol}, price: ${k.close} ma: ${ma.data(1).value},${ma.data(0).value} kdj: ${kdj
                              .data(1)}, ${kdj.data(1)} macd: ${macd.data(1).bar},${macd.data(0).bar}"
                    )
                    open(kdjDir, k.close - (az * 1.25) * kdjDir, k.close + (az * 2.5) * kdjDir)
                    // 每个周期只尝试一次
                    currentPosition = Some(null)
                }
            }
        }

        // k线结束清除开仓记录， 再次出现才能开仓
        if (k.end) {
            currentPosition = None
        }

    }
}
