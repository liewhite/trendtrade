package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
// 代码设计

// 做趋势回调
// 处于均线劣势方， 出现顺势K线开仓
// 止损为K线最低点
// 移动止盈
class MaBackStrategy() extends BaseStrategy with KlineMixin with MacdMixin() with MaMixin(Vector(20)) {
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

  def open(direction: Int, stopLoss: BigDecimal) = {
    val k = klines(0)
    position.prepend(
      Position(k, k.datetime, direction, k.close, None, stopLoss = Some(stopLoss))
    )
    println(s"open : ${direction} ${k.datetime}, price ${k.close}")
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
    // 无波动，不操作
    if (klines(0).close == klines(1).close) {
      return
    }
    if(mas(20)(0) == mas(20)(1)) {
      return
    }

    val maDirection = ((macd(0).dea - macd(1).dea).abs / (macd(0).dea - macd(1).dea)).intValue

    if( (k.open - mas(20)(0) ) * maDirection <0 && (k.close - k.open) * maDirection > 0 ) {
      if(position.nonEmpty && position(0).direction == maDirection) {
        return
      }
      println(s"macd ${macd(0)} k: ${k}")
      val sl = if(maDirection == 1) k.low else k.high
      open(maDirection, stopLoss = k.close)
    }

  }

}
