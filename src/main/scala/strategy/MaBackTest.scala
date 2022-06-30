package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
// 代码设计

// 做趋势回调
// 处于均线劣势方， 出现顺势K线开仓
// 止损为K线最低点
// 移动止盈
class MaBackTest() extends BaseStrategy with KlineMixin with MacdMixin() with MaMixin(Vector(20)) {
  val position = mutable.ListBuffer.empty[Position]
  val closed = mutable.ListBuffer.empty[Position]

  // 根据盈亏设置止盈止损线
  def modifyStopLoss() = {
    // 盈利超过20根K线平均值的1倍，则止损拉到成本线
    // 盈利超过20根K线平均值的3倍，则50%浮动止盈
    val k = klines(0)
    val ks = klines.slice(0,10)

    val avgFluctuate = ks.map(item => (item.close - item.open).abs).sum / ks.length

    position.mapInPlace(p => {
      if((k.close - p.openAt) * p.direction > 10 * avgFluctuate) {
        p.copy(stopLoss = Some(k.close - (k.close - p.openAt) * p.direction * 0.2))
      }else if((k.close - p.openAt) * p.direction > avgFluctuate) {
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

  def open(direction: Int, stopLoss: BigDecimal) = {
    val k = klines(0)
    position.prepend(
      Position(1,k.datetime, direction, k.close, None,None, Some(stopLoss), None)
    )
    println(s"open : ${position}")
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
    if(ma(0) == ma(1)) {
      return
    }
    if(position.nonEmpty) {
      return
    }

    val maDirection = (ma(0) - ma(1)).signum

    // 开盘价在均线劣势方,且涨跌与均线一致
    if( ((k.open - ma(0) ) * maDirection < 0 || (k.open - ma(0)) .abs < avgEntitySize * 0.3) && (k.close - k.open) * maDirection > 0 ) {
      // 已有持仓， 忽略
      if(position.nonEmpty && position(0).direction == maDirection) {
        return
      }
      val sl = if(maDirection == 1) k.low else k.high
      open(maDirection, stopLoss = sl)
    }

  }

}