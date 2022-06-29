package data

import sttp.client3.okhttp.quick._
import io.github.liewhite.json.{*, given}
import java.time.LocalDateTime
import java.time.LocalDate
import strategy.Kline

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