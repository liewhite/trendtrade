package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
import sttp.client3._
import binance.BinanceApi
import com.typesafe.scalalogging.Logger
import binance.TradeSide

class MaBackStrategy(symbol: String, trader: BinanceApi)
    extends BaseStrategy
    with KlineMixin
    with MacdMixin()
    with MaMixin(Vector(20)) {

  val logger = Logger("strategy")
  var currentPosition: Option[Position] = None
  val closed = mutable.ListBuffer.empty[Position]

  def init() = {
    loadHistory()
    loadPosition()
    // 开始websocket
  }

  def loadHistory() = {
    val history = trader.getHistory(symbol, "1h")
    history.foreach(step(_, true))
  }

  // 必须先load历史K线才能加载持仓
  def loadPosition() = {
    // 获取持仓,过滤出symbol
    val positions = trader.getPositions(symbol)
    if (positions.length > 1) {
      throw Exception("positions > 1, strategy only support one position")
    }
    // 获取当前价格， 计算止损价
    val currentPrice = trader.getSymbolPrice(symbol)
    // 加入持仓
    val p = positions(0)
    val direction = p.positionAmt.signum
    currentPosition = Some(
      Position(
        p.positionAmt,
        LocalDateTime.now,
        direction,
        p.entryPrice,
        None,
        None,
        None,
        None
      )
    )
    modifyStopLoss()
  }

  // 根据盈亏设置止盈止损线
  def modifyStopLoss(): Unit = {
    if (currentPosition.isEmpty) {
      return
    }
    // 盈利超过20根K线平均值的1倍，则止损拉到成本线
    // 盈利超过20根K线平均值的3倍，则50%浮动止盈
    val k = klines(0)
    val ks = klines.slice(0, 10)

    val avgFluctuate =
      ks.map(item => (item.close - item.open).abs).sum / ks.length

    currentPosition = currentPosition.map(p => {
      if ((k.close - p.openAt) * p.direction > 10 * avgFluctuate) {
        p.copy(stopLoss =
          Some(k.close - (k.close - p.openAt) * p.direction * 0.2)
        )
      } else if ((k.close - p.openAt) * p.direction > avgFluctuate) {
        p.copy(stopLoss = Some(p.openAt))
      } else {
        p
      }
    })
  }

  // 止盈止损
  def checkAndClose() = {
    val k = klines(0)
    currentPosition match {
      case None =>
      case Some(item) => {
        if ((klines(0).close - item.stopLoss.get) * item.direction < 0) {
          // 平仓, 需要symbol， quantity，direction
          trader.sendOrder(
            symbol,
            if (item.direction == 1) then TradeSide.SELL else TradeSide.BUY,
            item.quantity
          )

          closed.prepend(
            item.copy(
              closeTime = Some(klines(0).datetime),
              closeAt = Some(klines(0).close)
            )
          )
          currentPosition = None
          println(
            s"close:${item} profit: ${(k.close - item.openAt) * item.direction} ${item.direction} ${k.datetime}, price ${k.close}"
          )
        }
      }
    }
  }

  // 发送订单， 等待成交
  def open(direction: Int, stopLoss: BigDecimal): Unit = {
    if (currentPosition.nonEmpty) {
      return
    }
    // 查询账户总额， 余额, 如果余额小于总额的10%()， 放弃开仓
    // 根据开仓金额计算quantity， 精确到千分位
    // 开仓成功时，记录订单id
    val balances = trader.getTotalBalance()
    if (balances._2 * 10 < balances._1) {
      logger.warn(
        s"failed open position, not enough balance: total: ${balances._1}, availiable: ${balances._2}"
      )
      return
    }
    val k = klines(0)
    val price = k.close
    // 保留3位小数
    val quantity =
      ((balances._1 * 0.1) / price * trader.leverage) setScale (3, BigDecimal.RoundingMode.HALF_UP)
    val side = if (direction == 1) TradeSide.BUY else TradeSide.SELL
    val orderId = trader.sendOrder(symbol, side, quantity)

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
    logger.info(s"open : ${currentPosition}")
  }

  override def step(k: Kline, history: Boolean = false): Unit = {
    super.step(k)
    // 忽略历史数据， 只处理实时数据
    if (history) {
      return
    }
    // 历史数据不足， 无法参考
    if (klines.length < 20) {
      return
    }
    checkAndClose()
    modifyStopLoss()

    val entitySize = (k.close - k.open).abs
    val entities = klines
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
    if (entitySize <= avgEntitySize * 2) {
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
    if (currentPosition.nonEmpty) {
      return
    }

    val maDirection = (ma(0) - ma(1)).signum

    // 开盘价在均线劣势方,且涨跌与均线一致
    if (
      (k.open - ma(0)) * maDirection < 0 && (k.close - k.open) * maDirection > 0
    ) {
      // 已有持仓， 忽略
      if (
        currentPosition.nonEmpty && currentPosition.get.direction == maDirection
      ) {
        return
      }
      val sl = if (maDirection == 1) k.low else k.high
      open(maDirection, stopLoss = sl)
    }

  }

}
