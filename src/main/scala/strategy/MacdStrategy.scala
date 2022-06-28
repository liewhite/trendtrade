package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
import scala.languageFeature.postfixOps

// macd综合策略
class MacdStrategy()
    extends BaseStrategy
    with KlineMixin
    with MaMixin(Vector(20))
    with KdjMixin()
    with MacdMixin() {
  var position: Option[Position] = None
  val closed = mutable.ListBuffer.empty[Position]

  def closeCurrent() = {
    val k = klines(0)
    val current = position.get
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
  }

  def open(direction: Int, stopLoss: BigDecimal) = {
    val k = klines(0)
    position = Some(Position(k, k.datetime, direction, k.close, None, stopLoss = Some(stopLoss)))
    println(s"open : ${direction} ${k.datetime}, price ${k.close}")
  }

  def maDirection: Int = {
    if(mas(20)(0) == mas(20)(1)) {
      0
    }else if(mas(20)(0) > mas(20)(1)) {
      1
    }else {
      -1
    }
  }
  // 均线加速情况
  def maDirection2: Int = {
    if(mas(20)(0) - mas(20)(1) == mas(20)(1) - mas(20)(2)) {
      0
    }else if(mas(20)(0) - mas(20)(1) > mas(20)(1) - mas(20)(2)) {
      1
    }else {
      -1
    }
  }

  def diffDirection: Int = {
    if(macd(0).diff == macd(1).diff) {
      0
    }else if(macd(0).diff > macd(1).diff) {
      1
    }else {
      -1
    }
  }
  def diffDirection2: Int = {
    if(macd(0).diff - macd(1).diff == macd(1).diff - macd(2).diff) {
      0
    }else if(macd(0).diff - macd(1).diff > macd(1).diff - macd(2).diff) {
      1
    }else {
      -1
    }
  }
  def macdDirection: Int = {
    if(macd(0).bar == macd(1).bar) {
      0
    }else if(macd(0).bar > macd(1).bar) {
      1
    }else {
      -1
    }
  }

  def priceDirection: Int = {
    if(klines(0).close == mas(20)(0)) {
      0
    }else if(klines(0).close > mas(20)(0)) {
      1
    }else{
      -1
    }
  }

  def kdjDirection: Int = {
    if(kdj(0).d - kdj(0).j == kdj(1).d - kdj(1).j) {
      0
    }else if(kdj(0).d - kdj(0).j < kdj(1).d - kdj(1).j) {
      1
    }else {
      -1
    }
  }

  override def step(k: Kline, history: Boolean = false): Unit = {
    super.step(k)
    if(klines.length < 26) {
      return
    }
    val weight: Vector[Double] = Vector(1,1,1,1,1,1,1)
    val metrics: Vector[Double] = Vector(maDirection, maDirection2, macdDirection, diffDirection, diffDirection2, priceDirection, kdjDirection)

    val weightSum = weight.sum

    val score = metrics.zip(weight).map(_ * _).sum
    if(score == 0 && position.nonEmpty) {
      closeCurrent()
      return
    }

    val direction = (score.abs / score).intValue
    val scoreAbs = score.abs
    // 头寸逆势， 平仓
    if(position.nonEmpty && position.get.direction != direction) {
      closeCurrent()
    }

    // 得分超过总分的80%直接强制平反向仓位再开仓
    if(score > weightSum * 0.618) {
      // println(s"price: ${priceDirection} ma: ${maDirection}, ma2: ${maDirection2} diff: ${diffDirection} diff2: ${diffDirection2}, kdj:${kdjDirection} macd: ${macdDirection}")
      // println(s"${macd(0).diff} ${macd(1).diff} ${macd(2).diff}")
      // 平反向仓位
      if(position.nonEmpty && position.get.direction != direction) {
        closeCurrent()
      }
      // 开仓
      if(position.isEmpty){
        open(direction,0)
      }
    }else if(score < 0){
      if(position.nonEmpty) {
        closeCurrent()
      }
    }
  }

}
