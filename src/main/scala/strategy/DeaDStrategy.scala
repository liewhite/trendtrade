package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
import scala.languageFeature.postfixOps
import scala.annotation.targetName

// macd, kdj
class DeaDStrategy() extends BaseStrategy with KlineMixin() with MacdMixin() with KdjMixin() with MaMixin(Vector(20)){
  var position: Option[Position] = None
  val closed = mutable.ListBuffer.empty[Position]

  // 0止损 1止盈
  def closeCurrent(): Unit = {
    if(position.isEmpty) {
      return
    }
    val current = position.get
    val k = klines(0)
    position = None
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

  def open(direction: Int) = {
    val k = klines(0)
    position = Some(
      Position(
        k,
        k.datetime,
        direction,
        k.close,
        None,
      )
    )
    println(s"open :${position.get}")
  }

  override def step(k: Kline, history: Boolean = false): Unit = {
    super.step(k)
    if (klines.length < 26) {
      return
    }
    if(macd(0).dea == macd(1).dea) {
      return
    }
    if(kdj(0).d == kdj(1).d) {
      return
    }

    val maDirection = ((mas(20)(0) - mas(20)(1)) - (mas(20)(1) - mas(20)(2))).signum
    val preMaDirection = ((mas(20)(1) - mas(20)(2)) - (mas(20)(2) - mas(20)(3))).signum

    val deaDirection = ((macd(0).dea - macd(1).dea) - (macd(1).dea - macd(2).dea)).signum
    val preDeaDirection = ((macd(1).dea - macd(2).dea) - (macd(2).dea - macd(3).dea)).signum

    val dDirection = ((kdj(0).d - kdj(1).d) - (kdj(1).d - kdj(2).d)).signum
    val preDDirection = ((kdj(1).d - kdj(2).d) - (kdj(2).d - kdj(3).d)).signum

    val currentDirection = position match {
      case None => 0
      case Some(o) => o.direction
    }

    // // 有两个指标背离， 清仓
    // if(currentDirection !=0 && Vector(deaDirection, maDirection, dDirection).filter(_ == currentDirection).length < 3) {
    //   println("指标出现背离， 清仓")
    //   closeCurrent()
    // }

    // println(((preDDirection + preDeaDirection + preMaDirection).abs,(dDirection + deaDirection + maDirection).abs ))
    if((preDDirection + preDeaDirection + preMaDirection).abs < 3 && (dDirection + deaDirection + maDirection).abs == 3) {
      position match {
        case None => {
          open(deaDirection)
        }
        case Some(p) => {
          if(p.direction != deaDirection) {
            closeCurrent()
            open(deaDirection)
          }else {
          }
        }
      }
    }



  }

}
