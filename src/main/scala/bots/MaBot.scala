package bots

import java.time.Duration

class MaBot() extends Bot {

  // 平掉不符合持仓规则的仓位
  def closeHolding(k: Kline) = {
    // 1. 亏损超过3%的单子要强制平仓
    // 2. 均线连续两天逆势
    // 3. 出现反向开仓信号（一定逆势了， 所以可以不考虑）
    // 4. 连续两天逆势K线
    holding = holding.map(item => {
      item.copy(
        maxProfit =
          Vector((k.high - item.tradePrice) * item.trend, item.maxProfit).max,
        holdDays = item.holdDays + 1
      )
    })
    val price = k.close
    def filterF = (hold: Hold) => {
      // 第二天不赚钱就走
      // if (
      //   hold.holdDays == 1 && (k.close - hold.tradePrice) * trend < 0
      // ) {
      //   false
      // } else
      if (hold.maxProfit > hold.tradePrice * 0.05) {
        // 浮盈回撤止盈
        if ((k.close - hold.tradePrice) * hold.trend < hold.maxProfit * 0.7) {
          println(s"利润回撤止盈： max: ${hold.maxProfit}, current: ${(k.close - hold.tradePrice) * hold.trend}")
          false
        } else {
          true
        }
      } else if (hold.maxProfit > hold.tradePrice * 0.02) {
        // 保本出
        if ((k.close - hold.tradePrice) * hold.trend < 0) {
          false
        } else {
          true
        }
      } else {
        if((price - hold.kline.low) * hold.trend < 0) {
          false
        }else {
          if (
            // ((ma(0) - ma(1)) * hold.trend < 0 && (ma(1) - ma(2)) * hold.trend < 0 && (price - hold.tradePrice) * hold.trend > 0) ||
            ((ma(0) - ma(
              1
            )) * hold.trend < 0 && (price - ma(0)) * hold.trend < 0)
          ) {
            false
          } else {
            true
          }
        }
        // // 止损
        // if ((price - hold.tradePrice) * hold.trend / hold.tradePrice < -0.03) {
        //   false
        // } else {
          // 回到均线优势侧，但是出现连续两天均线调头
          // 或者没回到均线优势侧，均线就调头了
          // if (
          //   // ((ma(0) - ma(1)) * hold.trend < 0 && (ma(1) - ma(2)) * hold.trend < 0 && (price - hold.tradePrice) * hold.trend > 0) ||
          //   ((ma(0) - ma(
          //     1
          //   )) * hold.trend < 0 && (price - hold.tradePrice) * hold.trend < 0)
          // ) {
          //   false
          // } else {
          //   true
          // }
        // }
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
        closed = closed.prepended(loss)
      })

    holding = newHolding
  }

  def ma = mas(20)
  // 最近均线连续两次同向
  def trend: Int = {
    if (ma(0) - ma(1) == 0 || ma(1) - ma(2) == 0) {
      0
    } else {
      // val trend1 = (ma(0) - ma(1)).abs / (ma(0) - ma(1))
      // val trend2 = (ma(1) - ma(2)).abs / (ma(1) - ma(2))
      // if (trend1 == 1 && trend2 == 1) {
      //   1
      // } else if (trend1 == -1 && trend2 == -1) {
      //   -1
      // } else {
      //   0
      // }
      // trend1
      ((ma(0) - ma(1)).abs / (ma(0) - ma(1))).intValue
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
    // 减仓行情不做
    if(klines(0).hold < klines(1).hold) {
      return
    }
    // 开仓
    val open = Hold(k.datetime, currentTrend, k.close, 0, k)
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
    if (holding.isEmpty) {
      open(k)
    }
  }
}
