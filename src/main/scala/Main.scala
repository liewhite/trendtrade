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
    hold: BigDecimal,
    volume: BigDecimal
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
        item.volume
      )
    )
}

@main def main: Unit = {
  val ks15 = getSymbolK("GALUSDT","5m")
  // tickPercent 设置为interval波动的大概幅度即可
  val bot = GridStrategy(200)
  ks15.foreach(k => {
    bot.step(k)
  })
  val profit = bot.closed.map(item => (item.closeAt.get - item.openAt) * item.direction).sum
  println(s"tx count: ${bot.closed.length} , total profit:" + profit)
}

