package bots

import java.time.Duration

// ma20导数方向
// 价格在均线附近或处于负偏离状态
// macd bar 一致
// kdj j值小于60
// kdj j值向上
// 4个条件中，满足3个条件开仓， 低于2个条件清仓
class MaMacdKdjBot() extends Bot {

  // 价格与均线的位置是否满足开仓方向的要求
  def isPriceDirection(expectDirection: Int): Boolean = {
    if (
      ((klines(0).close - mas(20)(
        0
      )) / mas(20)(0)).abs.doubleValue < 0.01
    ) {
      return true
    }
    if (expectDirection == 1) {
      return klines(0).close < mas(20)(0)
    }
    if (expectDirection == -1) {
      return klines(0).close > mas(20)(0)
    }
    return false
  }

  def isJValueExpected(d: Int): Boolean = {
    if (kdj.length < 9) {
      return false
    }
    if (d == 1) {
      kdj(1).j < 50
    } else if (d == -1) {
      kdj(1).j > 70
    } else {
      false
    }
  }

  def isJDirectionExpected(d: Int): Boolean = {
    if (kdj.length < 9) {
      return false
    }
    if (d == 1) {
      kdj(0).j > kdj(1).j
    } else if (d == -1) {
      kdj(0).j < kdj(1).j
    } else {
      false
    }
  }

  def macdDirection: Int = {
    if (macd.length < 3) {
      return 0
    }
    val m0 = macd(0).bar
    val m1 = macd(1).bar
    val m2 = macd(2).bar
    // val m3 = macd(3).bar
    if (m0 > m1 && m1 > m2) {
      return 1
    }
    if (m0 < m1 && m1 < m2) {
      return -1
    }
    return 0
  }

  // 均线方向
  def maL1Direction: Int = {
    val ma = mas(20)
    if (ma.length < 3) {
      return 0
    }
    if (ma(0) == ma(1)) {
      return 0
    }
    return ((ma(0) - ma(1)).abs / (ma(0) - ma(1))).intValue
  }

  def ma = mas(20)

  // 均线导数, 连续3根加速
  def maDirection: Int = {
    val ma = mas(20)
    if (ma.length < 3) {
      return 0
    }
    // 向上加速
    if (ma(0) - ma(1) > ma(1) - ma(2) && ma(1) - ma(2) > ma(2) - ma(3) && ma(2) - ma(3) > ma(3) - ma(4) ) {
      return 1
    }
    // 向下加速
    if (ma(0) - ma(1) < ma(1) - ma(2) && ma(1) - ma(2) < ma(2) - ma(3) && ma(2) - ma(3) < ma(3) - ma(4) ) {
      return -1
    }
    return 0
  }

  def isHoldingExpectJ(d: Int): Boolean = {
    if (d == 1) {
      kdj(0).j < 100
    } else if (d == -1) {
      kdj(0).j > 0
    } else {
      return false
    }

  }
  def closeHolding(k: Kline): Unit = {
    if (holding.isEmpty) {
      return
    }
    val filterF = (item: Hold) => {
      val d = item.trend
      val conds = Vector(
        maL1Direction == d, // 均线还未调头
        maDirection == d, // 均线还未放缓
        (k.close - mas(20)(0)) * item.trend > 0, // 收盘价还在均线优势方
        (k.close - item.tradePrice) * item.trend > 0, // 收盘价还浮盈
        macdDirection == d,
        isHoldingExpectJ(d),
        isJDirectionExpected(d)
      )
      conds.count(item => item) >= 4
    }

    val newHolding = holding.filter(filterF)

    val price = k.close
    val closeds = holding
      .filter(item => !filterF(item))
      .map(hold => {
        val closed = Closed(hold, price, (price - hold.tradePrice) * hold.trend)
        println(
          s"close: ${hold}, at: ${price} profit: ${closed.profit}, time: ${k.datetime}"
        )
        closed
      })
    closed = closed.appendedAll(closeds)
    holding = newHolding
  }

  def open(k: Kline): Unit = {
    // 开仓方向
    val direction = maDirection
    if (direction == 0) {
      return
    }
    if(maDirection != direction) {
      return
    }
    val conds = Vector(
      // maDirection == direction,
      isPriceDirection(direction),
      macdDirection == direction,
      isJValueExpected(direction),
      isJDirectionExpected(direction)
    )
    if (conds.count(item => item) >= 3) {
      val open = Hold(k.datetime, direction, k.close, 0, k)
      println(s"open: ${open}")
      holding = holding.appended(open)
    }
  }

  def doStep(k: Kline): Unit = {
    if (klines.length < 27) {
      return
    }
    // 平仓
    closeHolding(k)
    // 开仓
    if (holding.isEmpty) {
      open(k)
    }
  }
}
