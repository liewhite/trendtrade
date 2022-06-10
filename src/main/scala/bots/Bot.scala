package bots

case class Macd(
  ema12: BigDecimal,
  ema26: BigDecimal,
  diff: BigDecimal,
  dea: BigDecimal,
  bar: BigDecimal
){
  def next(price: BigDecimal, emaa: Int,emab: Int): Macd = {
    val e12 = ema12 * (emaa - 1) / (emaa + 1) + price * 2 / (emaa + 1)
    val e26 = ema26 * (emab - 1) / (emab + 1) + price * 2 / (emab +1)
    val newDif = e12 - e26
    val newDea = this.dea * 8 / 10 + newDif * 2 / 10
    val b = 2 *(newDif - newDea)
    Macd(e12,e26,newDif,newDea,b)
  }
}
case class Kdj(
  kline: Kline,
  rsv: BigDecimal,
  k: BigDecimal,
  d: BigDecimal,
  j: BigDecimal,
)

abstract class Bot() {
  var klines = List.empty[Kline]
  var mas = Map.empty[Int, List[BigDecimal]]
  var macd = List.empty[Macd]
  var kdj = List.empty[Kdj]

  var holding = List.empty[Hold]
  var closed = List.empty[Closed]

  def maIntervals: Vector[Int] = Vector(5,10,20,30,60)
  def kdjParams: (Int,Int,Int) = (9,3,3)
  def macdParams: (Int,Int) = (12,26)

  def updateKdj():Unit = {
    if(klines.length < 10) {
      return
    }
    val params = kdjParams
    val k = klines(0)
    val low9 = klines.slice(0,9).map(_.low).min
    val high9 = klines.slice(0,9).map(_.high).max
    val rsv = (klines(0).close - low9 ) / (high9 - low9) * 100

    val newKdj = if(kdj.isEmpty) {
      val newK = 50 * 2 / 3 + rsv / 3
      val newD = 50 * 2 / 3 + newK / 3
      val newJ = 3 * newK - 2 * newD
      Kdj(k, rsv, newK,newD,newJ)
    }else{
      val preKdj = kdj(0)
      val newK = preKdj.k * 2 / 3 +  rsv / 3
      val newD = preKdj.d * 2 / 3 + newK / 3
      val newJ = 3 * newK - 2 * newD
      Kdj(k, rsv, newK,newD,newJ)
    }
    kdj = kdj.prepended(newKdj)
  }

  def updateMacd(): Unit = {
    val k = klines(0)
    if(klines.length == 1) {
      macd = macd.prepended(Macd(k.close, k.close, 0, 0, 0))
    }else{
      macd = macd.prepended(macd(0).next(k.close, macdParams._1,macdParams._2))
    }
  }

  def updateMas() = {
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
    updateMas()
    updateMacd()
    updateKdj()
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
