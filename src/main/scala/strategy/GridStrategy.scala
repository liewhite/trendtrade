package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
// 代码设计

case class Position(
    kline: Kline,
    openTime: LocalDateTime,
    direction: Int,
    openAt: BigDecimal,
    closeTime: Option[LocalDateTime] = None,
    closeAt: Option[BigDecimal] = None,
    stopLoss: Option[BigDecimal] = None,
    targetProfit: Option[BigDecimal] = None,
)

case class Tick(
    low: BigDecimal,
    high: BigDecimal,
    pressure: BigDecimal
)
// 网格交易策略
// 确定平均持仓时间 historyLength, 作为划分网格的价格区间, 且k线数据先进先出,最大只保留固定长度
// 因为成交活跃的品种平均持仓时间会很短
// 阻力值计算，pressure = f(vol, pos), 即离现在越近阻力越大， 成交量越大阻力越大。
// 计算突破K线的整体阻力值， 以及前进方向的阻力值， 变小即可开仓
// 开始交易
class GridStrategy(historyLength: Int) extends BaseStrategy with KlineMixin {
  val position = mutable.ListBuffer.empty[Position]
  val closed = mutable.ListBuffer.empty[Position]

  // 根据盈亏设置止盈止损线
  def modifyStopLoss() = {
    // 盈利超过20根K线平均值的2倍，则止损拉到成本线
    // 盈利超过20根K线平均值的5倍，则70%浮动止盈
    val k = klines(0)
    val ks = klines.slice(0,20)
    val avgFluctuate = ks.map(item => (item.close - item.open).abs).sum / ks.length
    position.mapInPlace(p => {
      if((k.close - p.openAt) * p.direction / p.openAt > 3 * avgFluctuate) {
        p.copy(stopLoss = Some(k.close - (k.close - p.openAt) * p.direction * 0.3))
      }else if((k.close - p.openAt) * p.direction / p.openAt > avgFluctuate) {
        p.copy(stopLoss = Some(p.openAt))
      }else{
        p
      }
    })
  }

  // 止盈止损
  def checkAndClose() = {
    val k = klines(0)
    val toClose = position.filter(item =>
      (klines(0).close - item.stopLoss.get) * item.direction < 0
    )
    position.filterInPlace(item =>
      (klines(0).close - item.stopLoss.get) * item.direction >= 0
    )

    toClose.foreach(item => {
      closed.prepend(
        item.copy(
          closeTime = Some(klines(0).datetime),
          closeAt = Some(klines(0).close)
        )
      )
      println(
        s"close:${item} profit: ${(k.close - item.openAt) * item.direction} ${item.direction} ${k.datetime}, price ${k.close}"
      )
    })
  }
  // 计算某个价位的阻力值
  // 原理是某个价位容易放量， 则大概率是阻力位
  def pressureAtPrice(price: BigDecimal): BigDecimal = {
    val ks =
      klines.drop(1).filter(k => k.high >= price && k.low <= price)
    if (ks.length == 0) {
      0
    } else {
      ks.map(_.vol).max
      // df / ks.length
    }
  }

  // 跟价格区间有交集的所有K线的平均压力
  def pressureBetweenPrice(p1: BigDecimal, p2: BigDecimal): BigDecimal = {
    val low = Vector(p1, p2).min
    val high = Vector(p1, p2).max
    val ks =
      klines.drop(1).filter(k => k.high > low && k.low < high)
    if (ks.length == 0) {
      0
    } else {
      ks.map(_.vol).max
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
    println()
  }
  def open(direction: Int, stopLoss: BigDecimal) = {
    val k = klines(0)
    position.prepend(
      Position(k, k.datetime, direction, k.close, None,None, Some(stopLoss),None)
    )
    println(s"open : ${direction} ${k.datetime}, price ${k.close}")
  }

  override def step(k: Kline, history: Boolean = false): Unit = {
    super.step(k)
    // 历史数据不足， 无法参考
    if (klines.length < historyLength) {
      return
    }

    // 历史数据达到上限， 先进先出
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
    checkAndClose()
    modifyStopLoss()

    val direction = ((klines(0).close - klines(1).close).abs / (klines(
      0
    ).close - klines(1).close)).intValue

    val entitySize = ((k.close - k.open) / k.open).abs
    val entities = klines
      .slice(1, 11)
      .map(item => {
        if (item.close == item.open) {
          BigDecimal(0)
        } else {
          (item.close - item.open).abs / item.open
        }
      })

    val avgEntitySize = entities.sum / entities.length
    // k线实体大于过去一段时间的平均的1.5倍， 视为趋势开始
    if (entitySize <= avgEntitySize * 1.5) {
      return
    }

    val openPressure = pressureAtPrice(
      k.open
    )

    val closePressure = pressureAtPrice(
      k.close
    )

    // 将来阻力变小或者突破了
    if (
      closePressure == 0 ||
      openPressure > closePressure
    ) {
      val sl = if(direction == 1) k.low else k.high
      if (position.nonEmpty) {
        if (position(0).direction != direction) {
          closeCurrent()
          open(direction, sl)
        } else {}
      } else {
        // 无持仓直接开仓
        open(direction, sl)
      }
    }
  }

}
