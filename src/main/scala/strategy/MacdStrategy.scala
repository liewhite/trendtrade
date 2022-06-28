package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
import scala.languageFeature.postfixOps
import scala.annotation.targetName

// macd综合策略
class MacdStrategy()
    extends BaseStrategy
    with KlineMixin()
    with MacdMixin() {
  var position: Option[Position] = None
  val closed = mutable.ListBuffer.empty[Position]


  // 0止损 1止盈
  def close(t: Int) = {
    val current = position.get
    if(t == 0) {
      closed.prepend(
        current.copy(
          closeTime = Some(klines(0).datetime),
          closeAt = Some(current.stopLoss.get)
        )
      )
    }else {
      closed.prepend(
        current.copy(
          closeTime = Some(klines(0).datetime),
          closeAt = Some(current.targetProfit.get)
        )
      )
    }
    position = None
    val k = klines(0)
    println(
      s"close: profit: ${(k.close - current.openAt) * current.direction} ${current.direction} ${k.datetime}, price ${k.close}"
    )
    println()
  }

  // 达到止盈止损标准的要平仓
  def checkClose(): Unit = {
    if(position.isEmpty) {
      return
    }
    val k = klines(0)
    val current = position.get

    if(current.direction == 1) {
      if(k.low <= current.stopLoss.get ) {
        close(0)
      }else if(k.high >= current.targetProfit.get) {
        close(1)
      }
    }else {
      if(k.high >= current.stopLoss.get ) {
        close(0)
      }else if(k.low <= current.targetProfit.get) {
        close(1)
      }
    }
  }

  def open(direction: Int, stopLoss: BigDecimal, tp: BigDecimal) = {
    val k = klines(0)
    position = Some(Position(k, k.datetime, direction, k.close, None, stopLoss = Some(stopLoss), targetProfit=Some(tp)))
    println(s"open :${position.get}")
  }

  override def step(k: Kline, history: Boolean = false): Unit = {
    super.step(k)
    if(klines.length < 26) {
      return
    }

    checkClose()

    if(macd(0).bar * macd(1).bar > 0) {
      return
    }

    if(position.nonEmpty) {
      return
    }

    val direction = macd(0).bar.signum
    if(direction == 0) {
      return
    }
    // 至少10根bar后出现金叉死叉
    val d10 = Range(1,10).map(i => {
      macd(i).bar.signum == -direction
    })
    if(!d10.forall(identity)){
      return
    }

    // 过去20K平均波动
    val entities = klines
      .slice(1, 20)
      .map(item => {
        (item.close - item.open).abs
      })

    val avgEntitySize = entities.sum / entities.length
    // 开仓， 设置止盈止损
    open(direction, k.close - (avgEntitySize * 1.2) * direction, k.close + (avgEntitySize * 1.5) * direction)
  }

}
