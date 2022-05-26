import trend.*
import scala.collection.mutable
import sttp.client3.okhttp.quick._
import io.github.liewhite.json.{*, given}
import java.time.LocalDateTime
import java.time.LocalDate

def getSymbol15minK(symbol: String): List[Kline] = {
  val response = quickRequest
    .get(
      uri"http://127.0.0.1:8080/api/public/futures_zh_minute_sina?symbol=${symbol}&period=15min"
    )
    .send(backend)
  response.body
    .fromJsonMust[List[HttpKline]]
    .map(item =>
      Kline(
        LocalDateTime.parse(item.datetime.replace(" ", "T")),
        item.open,
        item.high,
        item.low,
        item.close
      )
    )
}
def getSymbolDayK(symbol: String): List[Kline] = {
  val response = quickRequest
    .get(
      uri"http://127.0.0.1:8080/api/public/futures_zh_daily_sina?symbol=${symbol}"
    )
    .send(backend)
  val dk = response.body.fromJsonMust[List[HttpDayKline]]
  dk.map(item =>
    Kline(
      LocalDate.parse(item.date).atStartOfDay,
      item.open,
      item.high,
      item.low,
      item.close
    )
  )
}

case class Hold(
    datetime: LocalDateTime,
    trend: Int,
    openPrice: BigDecimal,
    stopLossPrice: BigDecimal
)

case class Closed(
    hold: Hold,
    closedAt: BigDecimal,
    profit: BigDecimal
)

abstract class Bot() {
  var klines15 = List.empty[Kline]
  var klinesDay = List.empty[Kline]

  var holding = List.empty[Hold]
  var closed = List.empty[Closed]

  def onOpen(open: Hold): Unit
  def onStopLoss(loss: Closed):Unit
  def onClose(close: Closed):Unit

  def updateDayK(k15: Kline) = {
    val date = if (k15.datetime.getHour > 15) {
      // 夜盘
      k15.datetime.plusDays(1).toLocalDate.atStartOfDay
    } else {
      k15.datetime.toLocalDate.atStartOfDay
    }
    // 更新日线
    if (
      klinesDay.nonEmpty && klinesDay.head.datetime.toString == date.toString
    ) {
      val old = klinesDay.head
      klinesDay = klinesDay.updated(
        0,
        Kline(
          date,
          old.open,
          Vector(old.high, k15.high).max,
          Vector(old.low, k15.low).min,
          k15.close
        )
      )
    } else {
      klinesDay = klinesDay.prepended(
        Kline(date, k15.open, k15.high, k15.low, k15.close)
      )
    }
  }
  def update15K(k15: Kline) = {
    klines15 = klines15.prepended(k15)
  }

  def closeAll(price: BigDecimal) = {
    val closeds = holding.map(hold => {
      val closed = Closed(hold, price, (price - hold.openPrice) * hold.trend)
      println(s"close ${hold}, at: ${price} profit: ${closed.profit}" )
      onClose(closed)
      closed
    })
    closed = closed.appendedAll(closeds)
    holding = List.empty
  }

  def stopLoss(k15: Kline) = {
    // 止损
    val losses = holding
      .filter(item => (k15.close - item.stopLossPrice) * item.trend < 0)
      .map(item => {
        val loss =
          Closed(item, k15.close, (k15.close - item.openPrice) * item.trend)
        onStopLoss(loss)
        println(s"close: ${item}, at: ${k15.close} profit: ${loss.profit}")
        loss
      })
    closed = closed.appendedAll(losses)
    holding =
      holding.filter(item => (k15.close - item.stopLossPrice) * item.trend >= 0)
  }

  // 传入15min k, 自行计算日K
  def step(k15: Kline): Unit = {
    update15K(k15)
    updateDayK(k15)
    this.stopLoss(k15)
    val date = k15.datetime.toLocalDate.atStartOfDay.toString
    // 判断趋势
    if (klinesDay.length < 5) {
      return
    }
    val dayDirection = if (klinesDay(0).close - klinesDay(4).close > 0) {
      1
    } else if (klinesDay(0).close - klinesDay(4).close < 0) {
      -1
    } else {
      0
    }
    val backtraceResult = backtrace(klines15, dayDirection)
    val trend = backtraceResult._2
    if (trend == 0) {
      return
    }
    val ks = backtraceResult._1
    if (holding.nonEmpty && holding.head.trend * trend < 0) {
      // 趋势反转， 清仓
      closeAll(k15.close)
    }
    // 顺势开仓
    val stopLoss = if (trend == 1) {
      ks.map(_.low).min
    } else {
      ks.map(_.high).max
    }
    val open = Hold(k15.datetime, trend, k15.close, stopLoss)
    println(s"open: ${open}")
    holding = holding.appended(open)
  }

  def holdingProfit = {
    val currentPrice = klines15.head.close
    holding.map(item => (currentPrice - item.openPrice) * item.trend ).sum
  }

  def closedProfit = {
    closed.map(item => item.profit).sum

  }
}

def realtimeMarket(symbol:String): List[HttpRealtimePrice] = {
  val response = quickRequest
    .get(
      uri"http://127.0.0.1:8080/api/public/futures_zh_spot?symbol=${symbol}&market=CF&adjust=0"
    )
    .send(backend)
  println(response.body)
  response.body
    .fromJsonMust[List[HttpRealtimePrice]]
    // .map(item =>
    //   RealtimePrice(
    //     LocalDateTime.parse(item.datetime.replace(" ", "T")),
    //     item.open,
    //     item.high,
    //     item.low,
    //     item.close
    //   )
    // )

}

class AlertBot extends Bot {
  def onOpen(hold: Hold) = {

  }

  def onClose(close: Closed) = {

  }
  def onStopLoss(close: Closed) = {

  }

}
@main def hello: Unit = {
  // val rt = realtimeMarket("MA2209")
  val ks15 = getSymbol15minK("M2209")

  val bot = AlertBot()
  ks15.foreach(k => {
    bot.step(k)
  })
  println(bot.holdingProfit)
  println(bot.closedProfit)
}
