package bots


class TrendBot() extends Bot {
  // 盈利超过2%则止损拉到成本线
  // 超过5%则回撤30%止盈
  def updateMustNotLoss(k15: Kline) = {
    this.holding = holding.map(item => {
      if((k15.close  - item.tradePrice) / item.tradePrice * item.trend >= 0.03 &&  (k15.close  - item.tradePrice) / item.tradePrice * item.trend < 0.06) {
        // 如果已有止盈策略， 则忽略
        if((item.stopLossPrice - item.tradePrice) * item.trend < 0 ) {
          val newItem = item.copy(mustNotLose = true, stopLossPrice = item.tradePrice)
          println(s"浮盈超过3%， 止损拉到成本线:${newItem},现价: ${k15}")
          newItem
        }else{
          item
        }
      }else if((k15.close  - item.tradePrice) / item.tradePrice * item.trend > 0.06 ){
        val newItem = item.copy(mustNotLose = true, stopLossPrice = item.tradePrice + (k15.close - item.tradePrice) * 0.7 )
        println(s"浮盈超过6%， 移动止损,${newItem}, 现价：${k15}")
        newItem
      }else{
        item
      }
    })
  }
  // 止盈止损
  // 亏损或打到止损线出局
  def stopLoss(k15: Kline) = {
    val losses = holding
      .filter(item =>
        (k15.close - item.stopLossPrice) * item.trend < 0 
      )
      .map(item => {
        val loss =
          Closed(item, k15.close, (k15.close - item.tradePrice) * item.trend)
        println(s"stop lose close: ${item}, at: ${k15.close} profit: ${loss.profit}, time: ${k15.datetime}")
        loss
      })
    closed = closed.appendedAll(losses)
    holding = holding.filter(item =>
      !((k15.close - item.stopLossPrice) * item.trend < 0)
    )
  }

  // 传入15min k, 自行计算日K
  def doStep(k15: Kline): Unit = {
    updateMustNotLoss(k15)
    this.stopLoss(k15)

    val date = k15.datetime.toLocalDate.atStartOfDay.toString

    if(mas(20).length < 2) {
      println("ma length < 2")
      return
    }
    if(mas(20)(0) - mas(20)(1) == 0) {
      println("ma has no direction")
      return
    }
    val ma20Trend = (mas(20)(0) - mas(20)(1)).abs / (mas(20)(0) - mas(20)(1))

    val backtraceResult = backtrace(klines, ma20Trend.intValue)
    val trend = backtraceResult._2
    if (trend == 0) {
      return
    }
    // 均线逆势,不做
    if ((k15.close - mas(20).head) * trend < 0) {
      println(s"trend ${trend}, price: ${k15.close}, ma20: ${mas(20).head}")
      return
    }

    if (holding.nonEmpty && holding.head.trend * trend < 0) {
      // 趋势反转， 清仓
      closeAll(k15)
    }
    val ks = backtraceResult._1
    // 顺势开仓
    // 统一止损线
    // 做多则以结构最低点,或者固定回撤止损
    val stopLoss = if (trend == 1) {
      List(ks.map(_.low).min).appended(k15.close * 0.98).max
    } else {
    // 做多则以结构最高点,或者固定回撤止损
      List(ks.map(_.high).max).appended(k15.close * 1.02).min
    }
    val open = Hold(k15.datetime, trend, k15.close, stopLoss,k15)
    println(s"open: ${open}")
    holding = holding.appended(open)
  }

}