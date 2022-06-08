package bots

class MaBot() extends Bot {
  def maIntervals: Vector[Int] = Vector(20)

  // 平掉不符合持仓规则的仓位
  def closeHolding(k: Kline) = {
    // 1. 亏损超过3%的单子要强制平仓
    // 2. 均线连续两天逆势
    // 3. 出现反向开仓信号（一定逆势了， 所以可以不考虑）
    val price = k.close
    val ma = mas(20)
    def filterF = (hold: Hold) => {
      // 止损
      if ((price - hold.tradePrice) * hold.trend / hold.tradePrice < -0.03) {
        false
      } else {
        // 逆势
        if (
          (ma(0) - ma(1)) * hold.trend < 0 && (ma(1) - ma(2)) * hold.trend < 0
        ) {
          false
        } else {
          true
        }
      }
    }

    val newHolding = holding.filter(filterF)
    holding
      .filter(i => !filterF(i))
      .foreach(item => {
        val loss =
          Closed(item, k.close, (k.close - item.tradePrice) * item.trend)
        println(
          s"stop lose close: ${item}, at: ${k.close} profit: ${loss.profit}, time: ${k.datetime}"
        )
      })

    holding = newHolding
  }

  def ma = mas(20)
  // 最近均线连续两次同向
  def trend: Int = {
    if (ma(0) - ma(1) == 0 || ma(1) - ma(2) == 0) {
      0
    } else {
      val trend1 = (ma(0) - ma(1)).abs / (ma(0) - ma(1))
      val trend2 = (ma(1) - ma(2)).abs / (ma(1) - ma(2))
      if (trend1 == 1 && trend2 == 1) {
        1
      } else if (trend1 == -1 && trend2 == -1) {
        -1
      } else {
        0
      }

    }
  }

  def open(k: Kline): Unit = {
    val currentTrend = trend
    if (currentTrend == 0) {
      return
    }
    if (k.close == k.open || k.open == ma(0)) {
      return
    }
    // 1 上涨， -1 下跌
    val kDirection = (k.close - k.open).abs / (k.close - k.open)
    // 均线方向和K线涨跌不一致
    if (currentTrend != kDirection) {
      return
    }

    // 开盘价在均线优势侧， 不做
    if ((k.open - ma(0)).abs / (k.open - ma(0)) == kDirection) {
      return
    }
    // 开仓
    println(klines)
    println(ma)
    val open = Hold(k.datetime, currentTrend, k.close, 0)
    println(s"open: ${open}")
    holding = holding.appended(open)
  }

  def doStep(k: Kline): Unit = {
    if (ma.length < 20) {
      return
    }
    // 平仓
    closeHolding(k)
    // 开仓
    open(k)
  }
}
