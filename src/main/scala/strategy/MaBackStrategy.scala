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

class MaBackStrategy(symbol: String, interval: String, trader: BinanceApi)
    extends BaseStrategy
    with KlineMixin
    with MacdMixin()
    with MaMixin(Vector(20)) {

  val logger = Logger("strategy")
  var currentPosition: Option[Position] = None
  val closed = mutable.ListBuffer.empty[Position]

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
    val positions = trader.getPositions(symbol)
    if (positions.length == 0) {
      return
    }
    if (positions.length > 1) {
      throw Exception("positions > 1, strategy only support one position")
    }
    // 获取当前价格， 计算止损价
    val currentPrice = trader.getSymbolPrice(symbol)
    // 获取开仓时间， 拿到low，high
    val trades = trader.getTrades(symbol)
    // 没有查询到成交记录， 哪来的持仓， 应该不会发生
    if (trades.isEmpty) {
      return
    }
    val lastTrade = trades.last
    val tradeTime = LocalDateTime.ofInstant(
      Instant.ofEpochMilli(lastTrade.time),
      ZoneId.systemDefault
    )
    logger.info(s"${symbol} 开仓时间: ${tradeTime}")
    val p = positions(0)
    // 遍历k线， 找到小于开仓时间 + interval 的那一条
    val tradeK = klines.find(item => !item.datetime.isAfter(tradeTime.minus(Duration.parse("PT" + interval))))
    // 找到K线则使用正确的止损， 找不到则开仓价止损
    val sl = tradeK match {
      case None    => {
        logger.info(s"找不到开仓k线, 使用成本价止损, ${p}")
        p.entryPrice
      }
      case Some(k) => {
        val v = if(p.positionAmt.signum == 1) k.low else k.high
        logger.info(s"找到开仓k线 ${k}, 设置止损 ${v} ${p}")
        v
      } 
    }

    // 加入持仓
    val direction = p.positionAmt.signum
    currentPosition = Some(
      Position(
        p.positionAmt,
        LocalDateTime.now,
        direction,
        p.entryPrice,
        None,
        None,
        Some(sl),
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
        logger.info(
          s"${symbol} 最近平均波动率 ${avgFluctuate}, 跟踪止盈到 ${k.close - (k.close - p.openAt) * p.direction * 0.2}"
        )
        p.copy(stopLoss =
          Some(k.close - (k.close - p.openAt) * p.direction * 0.2)
        )
      } else if ((k.close - p.openAt) * p.direction > avgFluctuate) {
        logger.info(s"${symbol} 止损拉到成本线 ${p.openAt}")
        p.copy(stopLoss = Some(p.openAt))
      } else {
        p
      }
    })
  }

  // 止盈止损
  def checkAndClose(): Unit = {
    val k = klines(0)
    currentPosition match {
      case None =>
      case Some(item) => {
        if ((klines(0).close - item.stopLoss.get) * item.direction < 0) {
          logger.info(s"触发止损, ${item} 当前k：${k}")
          // 平仓, 需要symbol， quantity，direction
          try {
            trader.sendOrder(
              symbol,
              if (item.direction == 1) then TradeSide.SELL else TradeSide.BUY,
              item.quantity
            )
            logger.info(s"止损成功, ${item} 当前k: ${k}")
            closed.prepend(
              item.copy(
                closeTime = Some(klines(0).datetime),
                closeAt = Some(klines(0).close)
              )
            )
            currentPosition = None
          } catch {
            case e: TimeoutException => {
              logger.error(s"挂单未成交， 请手动取消或平仓, ${symbol} ${k}")
            }
            case e: Exception => {
              logger.error(s"平仓失败， 请检查账户是否存在不一致")
            }
          }
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
    val rawQuantity = ((balances._1 * 0.1) / price * trader.leverage)
    val quantity = BigDecimal(
      (rawQuantity / symbolMeta.stepSize).intValue
    ) * symbolMeta.stepSize
    // val quantity =
    //   ((balances._1 * 0.1) / price * trader.leverage) setScale (3, BigDecimal.RoundingMode.HALF_UP)
    val side = if (direction == 1) TradeSide.BUY else TradeSide.SELL
    logger.info(s"触发开仓 ${symbol}, ${side} ${quantity}, k: ${k}")
    try {
      trader.sendOrder(symbol, side, quantity)
      logger.info(s"开仓成功 ${symbol}, ${side} ${quantity}, k: ${k}")
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
        logger.error(s"挂单未成交， 请手动取消或平仓, ${symbol} ${k}")
      }
      case e: Exception => {
        logger.warn(s"开仓失败， 请检查账户是否存在不一致")
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
      try {
        open(maDirection, stopLoss = sl)
      } catch {
        case e: Exception => {
          logger.warn(s"开仓失败，请检查账户一致性 ${symbol} ${k.datetime} ${e}")
        }
      }
    }

  }

}
