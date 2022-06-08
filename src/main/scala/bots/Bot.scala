package bots

abstract class Bot() {
  var klines = List.empty[Kline]
  var mas = Map.empty[Int, List[BigDecimal]]

  var holding = List.empty[Hold]
  var closed = List.empty[Closed]

  def maIntervals: Vector[Int]

  def updateMas(k: Kline) = {
    maIntervals.map(i => {
      val m =
        klines.slice(0, i).map(_.close).sum / klines.slice(0, i).length
      mas = mas.updated(i, mas.getOrElse(i, List.empty).prepended(m))
    })
  }

  def updateK(k15: Kline) = {
    klines = klines.prepended(k15)
  }

  def closeAll(closeK: Kline) = {
    val price = closeK.close
    val closeds = holding.map(hold => {
      val closed = Closed(hold, price, (price - hold.tradePrice) * hold.trend)
      println(
        s"close all  close ${hold}, at: ${price} profit: ${closed.profit}, time: ${closeK.datetime}"
      )
      closed
    })
    closed = closed.appendedAll(closeds)
    holding = List.empty
  }

  // 传入15min k, 自行计算日K
  def step(k: Kline): Unit = {
    updateK(k)
    updateMas(k)
    doStep(k)
  }

  def doStep(k: Kline): Unit

  def holdingProfit = {
    val currentPrice = klines.head.close
    holding.map(item => (currentPrice - item.tradePrice) * item.trend - 1).sum
  }

  def closedProfit = {
    closed.map(item => item.profit - 1).sum

  }
}
