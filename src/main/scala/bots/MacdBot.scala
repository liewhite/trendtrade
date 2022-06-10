package bots

import java.time.Duration

class MacdBot() extends Bot {
  def closeHolding(k: Kline): Unit = {
    if(holding.isEmpty){
      return
    }
    val direction = holding(0).trend
    val m0 = macd(0)
    val m1 = macd(1)
    val m2 = macd(2)
    if (
      ((m0.bar > m1.bar) && (m1.bar > m2.bar) && direction < 0) || ((m0.bar < m1.bar) && (m1.bar < m2.bar) && direction > 0)
    ) {
      closeAll(k)
    }
  }

  def open(k: Kline): Unit = {
    // dea 大于0 做多， 小于0做空
    // bar 连续两根变长开仓， 连续两根缩短清仓
    val m0 = macd(0)
    val m1 = macd(1)
    val m2 = macd(2)
    val direction = m0.dea.abs / m0.dea
    if (
      ((m0.bar > m1.bar) && (m1.bar > m2.bar) && direction > 0 && m2.bar > 0) || ((m0.bar < m1.bar) && (m1.bar < m2.bar) && direction < 0 && m2.bar < 0)
    ) {
      val open = Hold(k.datetime, direction.intValue, k.close, 0, k)
      println(s"open: ${open}, ${m2.bar},${m1.bar},${m0.bar}, dea: ${m0.dea}")
      holding = holding.appended(open)
    }
  }

  def doStep(k: Kline): Unit = {
    if (macd.length < 26) {
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
