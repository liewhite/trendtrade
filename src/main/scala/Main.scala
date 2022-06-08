import bots.*

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


@main def hello: Unit = {
  val ks15 = getSymbol15minK("CF2209")
  val bot = MaBot()
  ks15.foreach(k => {
    bot.step(k)
  })
  println("holding profit: " + bot.holdingProfit)
  
  println("closed profit: " + bot.closedProfit)
}
