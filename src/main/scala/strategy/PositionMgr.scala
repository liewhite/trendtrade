package strategy

import binance.BinanceApi
import notifier.Notify
import com.typesafe.scalalogging.Logger
import binance.TradeSide
import java.util.concurrent.TimeoutException
import java.time.LocalDateTime

class PositionMgr(
    symbol:          String,
    trader:          BinanceApi,
    maxHolds:        Int,
    ntf:             Notify,
    exceptionNotify: Notify
) {

    val logger = Logger("positionMgr")

    var currentPosition: Option[Position] = None
    def cleanPosition() = {
        currentPosition = None
    }

    def hasPosition: Boolean = currentPosition.nonEmpty

    def symbolMeta = trader.symbolMeta(symbol)

    def formatQuantity(n: BigDecimal): BigDecimal = {
        BigDecimal((n / symbolMeta.stepSize).intValue) * symbolMeta.stepSize
    }

    def formatPrice(n: BigDecimal): BigDecimal = {
        BigDecimal((n / symbolMeta.priceStep).intValue) * symbolMeta.priceStep
    }

    def loadPosition(): Unit = {
        logger.info(s"load positions of ${symbol}")
        // 获取持仓,过滤出symbol
        val positions = trader.getPositions(symbol)
        if (positions.length == 0) {
            return
        }
        if (positions.length > 1) {
            throw Exception("positions > 1, strategy only support one position")
        }
        val p         = positions(0)
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

    // 发送订单， 等待成交
    // 止盈止损
    def open(k: Kline,price: BigDecimal, direction: Int, stopLoss: Option[BigDecimal], tp: Option[BigDecimal]): Unit = {
        if (currentPosition.nonEmpty) {
            return
        }
        // 查询账户总额， 余额, 如果余额小于总额的10%()， 放弃开仓
        val balances    = trader.getTotalBalance()
        if (balances._2 * maxHolds < balances._1) {
            // NOTE: 做好合约账户被爆90%的准备,千万不能入金太多, 最多放可投资金的1/4, 这样被爆了还有机会翻
            val msg = s"余额不足10%, 停止开仓 ${symbol} ${balances._2}/${balances._1}"
            logger.warn(msg)
            // ntf.sendNotify(msg)
            return
        }
        // 按精度取近似值
        val rawQuantity = ((balances._1 / maxHolds) / price * trader.leverage)
        val quantity    = formatQuantity(rawQuantity)
        val side        = if (direction == 1) TradeSide.BUY else TradeSide.SELL
        val msg         = s"开仓 ${symbol}, ${side} ${quantity}"
        logger.info(msg)
        ntf.sendNotify(msg)
        try {
            trader.sendOrder(
              symbol,
              side,
              quantity,
              stopLoss.map(formatPrice(_)),
              tp.map(formatPrice(_)),
            )
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
                stopLoss,
                tp
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

    def closeCurrent(k: Kline): Unit = {
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
                      close = true
                    )
                    val msg = s"平仓成功: ${symbol} ${item} 当前k: ${k}"
                    logger.info(msg)
                    ntf.sendNotify(msg)
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

}
