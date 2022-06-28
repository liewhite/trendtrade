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
  // val ks15 = getSymbol5minK("CF2209")
  val ks15 = getSymbolK("DOTBUSD","15m")
  val bot = GridStrategy(150)
  ks15.foreach(k => {
    bot.step(k)
  })
  val profit = bot.closed.map(item => (item.closeAt.get - item.openAt) * item.direction - item.openAt * 0.00036 - item.closeAt.get * 0.00036).sum
  val fee = bot.closed.map(item => item.openAt * 0.00036 + item.closeAt.get * 0.00036).sum
  
  println(s"tx count: ${bot.closed.length} ,fee: ${fee} total profit: ${profit} precent: ${profit/bot.klines(0).close * 100}"  )
}

