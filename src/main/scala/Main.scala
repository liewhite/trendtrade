// import bots.*

import scala.collection.mutable
import sttp.client3.okhttp.quick._
import io.github.liewhite.json.{*, given}
import java.time.LocalDateTime
import java.time.LocalDate
import strategy.*

case class HttpKline(
    datetime: String,
    open: BigDecimal,
    high: BigDecimal,
    low: BigDecimal,
    close: BigDecimal,
    hold: BigDecimal
)

def getSymbol5minK(symbol: String): List[Kline] = {
  val response = quickRequest
    .get(
      uri"http://127.0.0.1:8090/api/public/futures_zh_minute_sina?symbol=${symbol}&period=5min"
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
        item.close,
        item.hold
      )
    )
}

class Bot extends Strategy with MaMixin(Vector(5,10,20)) with MacdMixin(12,26) with KdjMixin(9,3,3) {
  override def step(k: strategy.Kline): Unit = {
    super.step(k)
    println(mas)
    println(macd)
    println(kdj)
  }
}

@main def main: Unit = {
  val ks15 = getSymbol5minK("CF2209")
  val bot = Bot()
  ks15.foreach(k => {
    bot.step(k)
  })

  // println("holding profit: " + bot.holdingProfit)

  // println("closed positions: " + bot.closed.length)
  // println("closed profit: " + bot.closedProfit)
  // bot.closed.sortBy(_.profit.doubleValue).foreach(println)
}

