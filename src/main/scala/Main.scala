import bots.*

import scala.collection.mutable
import sttp.client3.okhttp.quick._
import io.github.liewhite.json.{*, given}
import java.time.LocalDateTime
import java.time.LocalDate

def getSymbol5minK(symbol: String): List[Kline] = {
  val response = quickRequest
    .get(
      uri"http://127.0.0.1:8080/api/public/futures_zh_minute_sina?symbol=${symbol}&period=5min"
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
        item.hold,
      )
    )
}
def getSymbolDayK(symbol: String): List[Kline] = {
  val response = quickRequest
    .get(
      uri"http://127.0.0.1:8080/api/public/futures_zh_daily_sina?symbol=${symbol}"
    )
    .send(backend)
  // println(response.body)
  val dk = response.body.fromJsonMust[List[HttpDayKline]]
  dk.map(item =>
    Kline(
      LocalDate.parse(item.date).atStartOfDay,
      item.open,
      item.high,
      item.low,
      item.close,
      item.hold,
    )
  )
}


@main def main: Unit = {
  val ks15 = getSymbol5minK("CF2209")
  val bot = MaMacdKdjBot()
  ks15.foreach(k => {
    bot.step(k)
  })
  
  println("holding profit: " + bot.holdingProfit)

  println("closed positions: " + bot.closed.length)
  println("closed profit: " + bot.closedProfit)
  // bot.closed.sortBy(_.profit.doubleValue).foreach(println)
}