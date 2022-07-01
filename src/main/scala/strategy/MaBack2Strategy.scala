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

class MaBack2Strategy(symbol: String, interval: String, trader: BinanceApi, ntf: Notify,exceptionNotify: Notify)
    extends BaseStrategy
    with KlineMixin
    with MacdMixin()
    with MaMixin(Vector(20)) {

    val logger                            = Logger("strategy")
    var currentPosition: Option[Position] = None
    val closed                            = mutable.ListBuffer.empty[Position]

    def start() = {
        loadHistory()
        loadPosition()
        // 开始websocket
        trader.subscribeKlines(symbol, interval, k => step(k))
    }

    def symbolMeta = trader.symbolMeta(symbol)

    // 币安是以k线开始时间为准的
    def loadHistory() = {
        val history = trader.getHistory(symbol, interval)
        // 去掉第一条
        history.dropRight(1).foreach(step(_, true))
        logger.info(
          s"load history of ${symbol} , last kline: ${klines(0)} ma: ${mas(20)(0)}"
        )
    }

    // 必须先load历史K线才能加载持仓
    def loadPosition(): Unit = {
        logger.info(s"load positions of ${symbol}")
        // 获取持仓,过滤出symbol
        val positions    = trader.getPositions(symbol)
        if (positions.length == 0) {
            return
        }
        if (positions.length > 1) {
            throw Exception("positions > 1, strategy only support one position")
        }
        val p            = positions(0)
        // 加入持仓
        val direction = p.positionAmt.signum
        currentPosition = Some(
          Position(
            p.positionAmt.abs,
            LocalDateTime.now,
            direction,
            p.entryPrice,
            None,
            None,
            None,
            None
          )
        )
    }

    def closeCurrent(): Unit = {
        val k = klines(0)
        currentPosition match {
            case None       =>
            case Some(item) => {
                val msg = s"触发平仓:${symbol} ${item} 当前k: ${k}"
                logger.info(msg)
                ntf.sendNotify(msg)
                try {
                    trader.sendOrder(
                      symbol,
                      if (item.direction == 1) then TradeSide.SELL else TradeSide.BUY,
                      item.quantity,
                      true
                    )
                    val msg = s"平仓成功: ${symbol} ${item} 当前k: ${k}"
                    logger.info(msg)
                    ntf.sendNotify(msg)
                    closed.prepend(
                      item.copy(
                        closeTime = Some(klines(0).datetime),
                        closeAt = Some(klines(0).close)
                      )
                    )
                } catch {
                    case e: TimeoutException => {
                        val msg = s"挂单未成交， 请手动取消或平仓, ${symbol} ${k} ${e}"
                        logger.error(msg)
                        exceptionNotify.sendNotify(msg)
                    }
                    case e: Exception => {
                        val msg = s"平仓失败， 请检查账户是否存在不一致 ${symbol} ${k} ${e}"
                        logger.error(msg)
                        exceptionNotify.sendNotify(msg)
                    }
                }
                // 无论如何都要删除持仓， 不然容易引起不一致, 币安端可以手动操作平仓
                currentPosition = None
            }
        }

    }
    // 止盈止损
    def checkAndClose(): Unit = {
        currentPosition match {
            case Some(p) => {
                val k     = klines(0)
                val ma    = mas(20)(0)
                val preMa = mas(20)(1)
                // 如果还没突破均线, 则均线方向逆势且亏损状态止损
                if ( (k.open - ma) * p.direction < 0 && (k.close - ma) * p.direction < 0) {
                    if ((ma - preMa).signum != p.direction && (k.close - p.openAt) * p.direction < 0) {
                        closed.prepend(
                          p.copy(
                            closeTime = Some(klines(0).datetime),
                            closeAt = Some(klines(0).close)
                          )
                        )
                        closeCurrent()
                    }
                }else {
                    // 如果已突破均线，则跌破均线平仓
                    if( (k.close - ma) * p.direction <0 ) {
                        closed.prepend(
                          p.copy(
                            closeTime = Some(klines(0).datetime),
                            closeAt = Some(klines(0).close)
                          )
                        )
                        closeCurrent()
                    }
                }
            }
            case None    => 
        }
    }

    // 发送订单， 等待成交
    def open(direction: Int): Unit = {
        if (currentPosition.nonEmpty) {
            return
        }
        // 查询账户总额， 余额, 如果余额小于总额的40%， 放弃开仓
        val balances    = trader.getTotalBalance()
        if (balances._2  < balances._1 * 0.4) {
            // NOTE: 做好合约账户被爆80%的准备,千万不能入金太多, 最多放可投资金的1/4, 这样被爆了还有机会翻
            val msg = s"余额不足40%, 停止开仓 ${balances._2}/${balances._1}"
            logger.warn(msg)
            ntf.sendNotify(msg)
            return
        }
        val k           = klines(0)
        val price       = k.close
        // 按精度取近似值, 开4成仓
        val rawQuantity = ((balances._1 * 0.4) / price * trader.leverage)
        val quantity    =
            BigDecimal((rawQuantity / symbolMeta.stepSize).intValue) * symbolMeta.stepSize
        val side        = if (direction == 1) TradeSide.BUY else TradeSide.SELL
        val msg         = s"触发开仓 ${symbol}, ${side} ${quantity}, k: ${k}"
        logger.info(msg)
        ntf.sendNotify(msg)
        try {
            trader.sendOrder(symbol, side, quantity)
            val msg = s"开仓成功 ${symbol}, ${side} ${quantity}, k: ${k}"
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
                None,
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

    override def step(k: Kline, history: Boolean = false): Unit = {
        super.step(k)
        // 忽略历史数据， 只处理实时数据
        if (history) {
            return
        }
        logger.info(s"kline: ${k} ma: ${mas(20)(0)}")
        // 历史数据不足， 无法参考
        if (klines.length < 60) {
            return
        }
        // 先止盈止损
        checkAndClose()
        // 不重复开仓
        if(currentPosition.nonEmpty) {
            return
        }


        val entitySize = (k.close - k.open).abs
        val entities   = klines
            .slice(1, 11)
            .map(item => {
                if (item.close == item.open) {
                    BigDecimal(0)
                } else {
                    (item.close - item.open).abs
                }
            })

        val avgEntitySize = entities.sum / entities.length
        // k线实体大于过去一段时间的平均的2倍， 视为趋势开始
        if (entitySize <= avgEntitySize) {
            return
        }

        val ma = mas(20)
        // 无波动，不操作
        if (klines(0).close == klines(1).close) {
            return
        }
        if (ma(0) == ma(1)) {
            return
        }

        val maDirection = (ma(0) - ma(1)).signum

        // 开盘价在均线劣势方或者很接近,且涨跌与均线一致
        if (
          (k.open - ma(0)) * maDirection < 0&& (k.close - k.open) * maDirection > 0
        ) {
            try {
                open(maDirection)
            } catch {
                case e: Exception => {
                    val msg = s"开仓失败，请检查账户一致性 ${symbol} ${k.datetime} ${e}"
                    logger.warn(msg)
                    exceptionNotify.sendNotify(msg)
                }
            }
        }
    }
}
