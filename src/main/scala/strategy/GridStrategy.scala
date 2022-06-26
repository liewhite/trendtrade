package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
// 代码设计

case class Position(
    kline: Kline,
    openTime: LocalDateTime,
    closeTime: Option[LocalDateTime],
    direction: Int,
    openAt: BigDecimal,
    closeAt: Option[BigDecimal],
    stopLoss: BigDecimal
)

// 网格交易策略
// 确定平均持仓时间 historyLength, 作为划分网格的价格区间, 且k线数据先进先出,最大只保留固定长度
// 因为成交活跃的品种平均持仓时间会很短
// 阻力值计算，pressure = f(vol, pos), 即离现在越近阻力越大， 成交量越大阻力越大。
// 计算突破K线的整体阻力值， 以及前进方向的阻力值， 变小即可开仓
// 开始交易
class GridStrategy(historyLength: Int)
    extends BaseStrategy
    with KlineMixin
    with VolMaMixin(Vector(5)) {
  val position = mutable.ListBuffer.empty[Position]
  val closed = mutable.ListBuffer.empty[Position]

  // 计算某个价位的阻力值
  // 某个价位的阻力= sum(成交量 * 0.99^距离) / k线数量
  // 原理是某个价位容易放量， 则大概率是阻力位
  def pressureAtPrice(price: BigDecimal): BigDecimal = {
    val ks =
      klines.drop(1).filter(k => k.high >= price && k.low <= price)
    if (ks.length == 0) {
      0
    } else {
      val df = ks.map(_.vol).sum
      df / ks.length
    }
  }

  // 跟价格区间有交集的所有K线的平均压力
  def pressureBetweenPrice(p1: BigDecimal, p2: BigDecimal): BigDecimal = {
    val low = Vector(p1, p2).min
    val high = Vector(p1, p2).max
    val ks =
      klines.drop(1).filter(k => k.high >= low && k.low <= high)
    if (ks.length == 0) {
      0
    } else {
      val df = ks.map(_.vol).sum
      df / ks.length
    }
  }

  def closeCurrent() = {
    val current = position(0)
    val k = klines(0)
    position.remove(0)
    closed.prepend(
      current.copy(
        closeTime = Some(klines(0).datetime),
        closeAt = Some(klines(0).close)
      )
    )
    println(
      s"close: profit: ${(k.close - current.openAt) * current.direction} ${current.direction} ${k.datetime}, price ${k.close}"
    )

  }
  def open(direction: Int, stopLoss: BigDecimal) = {
    val k = klines(0)
    position.prepend(
      Position(k, k.datetime, None, direction, k.close, None, stopLoss)
    )
    println(s"open : ${direction} ${k.datetime}, price ${k.close}")
  }

  // 回撤超过
  def stopLoss() = {
    if (position.nonEmpty) {
      val p = position(0)
      val slPrice = p.stopLoss

      if (
        (klines(0).close - slPrice) * position(
          0
        ).direction < 0
      ) {
        println(
          s"触发止损(${klines(0).datetime}): 止损价: ${position(0).stopLoss} 当前收盘价${klines(0).close}"
        )
        closeCurrent()
      }

    }

  }

  override def step(k: Kline, history: Boolean = false): Unit = {
    super.step(k)
    // 没有达到历史数据长度要求， 不划分网格，不进行交易
    if (klines.length < historyLength) {
      return
    }

    // 超出所需历史长度，保持历史数据长度不变
    if (klines.length > historyLength) {
      klines.dropRightInPlace(1)
    }

    // 忽略历史数据， 只处理实时数据
    if (history) {
      return
    }

    if (klines(0).close == klines(1).close) {
      return
    }
    val entitySize = ((k.close - k.open) / k.open).abs
    val entities = klines
      .slice(1, 21)
      .map(item => {
        (item.close - item.open).abs / item.open
      })

    val avgEntitySize = entities.sum / entities.length
    // k线实体大于过去一段时间的平均的3倍， 视为趋势开始
    if (entitySize <= avgEntitySize * 3) {
      return
    }

    val lowPressure = pressureAtPrice(k.low)
    val highPressure = pressureAtPrice(k.high)

    val direction = ((klines(0).close - klines(1).close).abs / (klines(
      0
    ).close - klines(1).close)).intValue

    val currentKPressure = pressureBetweenPrice(
      k.open,
      k.close
    )

    val forwardPressure1 = pressureBetweenPrice(
      k.close,
      k.close * 2 - k.open
    )
    val forwardPressure2 = pressureBetweenPrice(
      k.close + (k.close - k.open),
      k.close + 2 * (k.close - k.open)
    )

    val backPressure1 = pressureBetweenPrice(
      k.open,
      k.open * 2 - k.close
    )
    val backPressure2 = pressureBetweenPrice(
      k.open - (k.close - k.open),
      k.open - 2 * (k.close - k.open)
    )

    if (
      // backPressure2 > currentKPressure &&
      backPressure1 >= currentKPressure * 1.2 &&
      // currentKPressure >= forwardPressure1 &&
      currentKPressure >= forwardPressure1
    ) {
      if (position.nonEmpty) {
        if (position(0).direction != direction) {
          closeCurrent()
          open(direction, 0)
        } else {}
      } else {
        // 无持仓直接开仓
        open(direction, 0)
      }
    }
  }
}
