package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
import scala.annotation.targetName
// 代码设计

// 神奇九转， 第九根或者第十根出现反转线开仓
class Magic9Strategy() extends BaseStrategy with KlineMixin{
  val position = mutable.ListBuffer.empty[Position]
  val closed = mutable.ListBuffer.empty[Position]

  // 止盈止损
  def checkAndClose() = {
    val k = klines(0)
    val toClose = position.filter(item =>
      (klines(0).close - item.stopLoss.get) * item.direction < 0 ||
      (klines(0).close - item.targetProfit.get) * item.direction > 0 )
    position.filterInPlace(item =>
      !((klines(0).close - item.stopLoss.get) * item.direction < 0 ||
      (klines(0).close - item.targetProfit.get) * item.direction > 0 ))

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

  def open(direction: Int, stopLoss: BigDecimal, targetProfit: BigDecimal) = {
    val k = klines(0)
    position.prepend(
      Position(k, k.datetime, direction, k.close, None, stopLoss = Some(stopLoss), targetProfit = Some(targetProfit))
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
    if (klines.length < 15) {
      return
    }
    checkAndClose()
    if(position.nonEmpty) {
      return
    }
    // 如果出现连续9根同向， 反向开仓
    val ks9Dir = Range(0,9).map(i => {
      klines(i).close > klines(i+4).close
    })

    // 前一日开始的9根K线
    val ks10Dir = Range(1,10).map(i => {
      klines(i).close > klines(i+4).close
    })

    // 连涨9k
    if(ks9Dir.forall(identity) && (klines(0).close - klines(0).open) < 0) {
      open(-1, k.high, k.close - (k.high - k.close))
    }else if(ks9Dir.forall(!identity(_)) && (klines(0).close - klines(0).open) > 0) {
      open(1, k.low, k.close + (k.close - k.low))
    } else if(ks10Dir.forall(identity) && (klines(0).close - klines(0).open) < 0) {
      open(-1, k.high, k.close - (k.high - k.close))
    }else if(ks10Dir.forall(!identity(_)) && (klines(0).close - klines(0).open) > 0) {
      open(1, k.low, k.close + (k.close - k.low))
    }else{

    }

  }

}
