package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
import scala.languageFeature.postfixOps
import scala.annotation.targetName

// macd综合策略
class KdjStrategy() extends BaseStrategy with KlineMixin() with KdjMixin() with MacdMixin() {
  var position: Option[Position] = None
  val closed = mutable.ListBuffer.empty[Position]

  // 0止损 1止盈
  def close(t: Int) = {
    val current = position.get
    if (t == 0) {
      closed.prepend(
        current.copy(
          closeTime = Some(klines(0).datetime),
          closeAt = Some(current.stopLoss.get)
        )
      )
    } else {
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
    if (position.isEmpty) {
      return
    }
    val k = klines(0)
    val current = position.get

    if (current.direction == 1) {
      if (k.low <= current.stopLoss.get) {
        close(0)
      } else if (k.high >= current.targetProfit.get) {
        close(1)
      }
    } else {
      if (k.high >= current.stopLoss.get) {
        close(0)
      } else if (k.low <= current.targetProfit.get) {
        close(1)
      }
    }
  }

  def open(direction: Int, stopLoss: BigDecimal, tp: BigDecimal) = {
    val k = klines(0)
    position = Some(
      Position(
        k,
        k.datetime,
        direction,
        k.close,
        None,
        stopLoss = Some(stopLoss),
        targetProfit = Some(tp)
      )
    )
    println(s"open :${position.get}")
  }

  override def step(k: Kline, history: Boolean = false): Unit = {
    super.step(k)
    if (klines.length < 26) {
      return
    }

    checkClose()
    if (position.nonEmpty) {
      return
    }
    // 过去20K平均波动
    val entities = klines
      .slice(1, 20)
      .map(item => {
        if (item.close == item.open) {
          BigDecimal(0)
        } else {
          (item.close - item.open).abs
        }
      })

    val avgEntitySize = entities.sum / entities.length

    val kdjDirection =
      if (
        kdj(1).j > 80 &&
        kdj(1).j > kdj(1).d &&
        kdj(2).j > kdj(2).d &&
        kdj(3).j > kdj(3).d &&
        kdj(0).j < kdj(0).d
      ) {
        -1
      } else if (
        kdj(1).j < 20 &&
        kdj(1).j < kdj(1).d &&
        kdj(2).j < kdj(2).d &&
        kdj(3).j < kdj(3).d &&
        kdj(0).j > kdj(0).d
      ) {
        1
      } else {
        0
      }
    val macdDirection = (macd(0).bar - macd(1).bar).signum
    if (kdjDirection == macdDirection && kdjDirection != 0) {
      open(
        kdjDirection,
        k.close - (avgEntitySize * 1) * kdjDirection,
        k.close + (avgEntitySize * 1.5) * kdjDirection
      )
    }

  }

}
