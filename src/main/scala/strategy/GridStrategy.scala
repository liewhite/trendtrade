package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
// 代码设计

case class Position(
    openTime: LocalDateTime,
    closeTime: Option[LocalDateTime],
    direction: Int,
    openAt: BigDecimal,
    closeAt: Option[BigDecimal]
)

// 网格交易策略
// 确定平均持仓时间 historyLength, 作为划分网格的价格区间, 且k线数据先进先出,最大只保留固定长度
// 因为成交活跃的品种平均持仓时间会很短
// 阻力值计算，pressure = f(vol, pos), 即离现在越近阻力越大， 成交量越大阻力越大。
// 开始交易
// K线到来时检查上一跟K线的阻力值， 和当前的阻力值，如果当前阻力变小， 则往后迭代几个价位， 如果整体为变小趋势， 则是开仓点
// 无持仓或者持有反向仓位时， 往上突破阻力带开多，往下突破阻力带反手开空
// tickPercent: 模拟压力时的步长
// tickThreshold: 模拟压力时的步数
class GridStrategy(historyLength: Int)
    extends BaseStrategy
    with KlineMixin with VolMaMixin(Vector(5)) {
  val position = mutable.ListBuffer.empty[Position]
  val closed = mutable.ListBuffer.empty[Position]

  // 计算某个价位的阻力值
  // 某个价位的阻力= sum(成交量 * 0.99^距离) / k线数量
  // 原理是某个价位容易放量， 则大概率是阻力位
  def pressureAtPrice(price: BigDecimal): BigDecimal = {
    val ks =
      klines.zipWithIndex.filter((k, i) => k.high > price && k.low < price)
    if (ks.length == 0) {
      0
    } else {
      val df = ks.map { case (k, i) =>
        // k.vol * BigDecimal(0.99).pow(i)
        k.vol
      }.sum
      // println(s"price pressure: ${price} .  ${df}")
      df / ks.length
    }
  }

  def closeCurrent() = {
    val current = position(0)
    val k = klines(0)
    position.dropInPlace(0)
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
  def open(direction: Int) = {
    val k = klines(0)
    position.prepend(Position(k.datetime, None, direction, k.close, None))
    println(s"open : ${direction} ${k.datetime}, price ${k.close}")
  }

  override def step(k: Kline, history: Boolean = false): Unit = {
    super.step(k)
    // println(k)
    // 没有达到历史数据长度要求， 不划分网格，不进行交易
    if (klines.length < historyLength) {
      return
    }

    // 超出所需历史长度，保持历史数据长度不变
    if (klines.length > historyLength) {
      klines.dropRight(1)
    }

    // 忽略历史数据， 只处理实时数据
    if (history) {
      return
    }

    if (klines(0).close == klines(1).close) {
      return
    }

    val entitySize = ((k.close - k.open) / k.open).abs

    val entities = klines.slice(1,21).map(item => {
      (item.close - item.open).abs / item.open
    })

    val avgEntitySize = entities.sum / entities.length
    // k线实体大于过去一段时间的平均的3倍， 视为趋势开始
    if(entitySize <= avgEntitySize * 2) {
      return
    }

    val lowPressure = pressureAtPrice(k.low)
    val highPressure = pressureAtPrice(k.high)
    val direction = ((klines(0).close - klines(1).close).abs / (klines(
      0
    ).close - klines(1).close)).intValue

    if( (direction == 1 && highPressure > lowPressure * 1.2) || (direction == -1 && lowPressure > highPressure * 1.2)) {
      if (position.nonEmpty) {
        if (position(0).direction != direction) {
          closeCurrent()
          open(direction)
        }
        // 有持仓且方向一致， 继续持有
      } else {
        // 无持仓直接开仓
        open(direction)
      }
    }
  }
}
