import sttp.client3.okhttp.quick._
import io.github.liewhite.json.{*, given}
import strategy.Kline
import java.time.LocalDateTime
import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime

val rootUrl = "https://fapi.binance.com"

def getSymbolK(symbol: String, interval: String) = {
  val endTime = ZonedDateTime.now()
  val startTime = endTime.minusDays(15)

  val startTs = startTime.toInstant().toEpochMilli
  val endTs = endTime.toInstant().toEpochMilli

  val response = quickRequest
    .get(
      uri"$rootUrl/fapi/v1/continuousKlines?pair=${symbol}&startTime=${startTs}&endTime=${endTs}&limit=1500&contractType=PERPETUAL&interval=${interval}"
    )
    .header(
      "X-MBX-APIKEY",
      "HOPEvPkEmARRPJ2Vw8MQvXGDnA9nclxRqbGWODC8NzVD79wSOdHsn6YwX7nSjYG6"
    )
    .send(backend)

  response.body
    .fromJsonMust[List[
      (
          Long,
          String,
          String,
          String,
          String,
          String,
          Long,
          String,
          Long,
          String,
          String,
          String
      )
    ]]
    .map(item =>
      Kline(
        LocalDateTime
          .ofInstant(Instant.ofEpochMilli(item._1), ZoneId.systemDefault),
        BigDecimal(item._2),
        BigDecimal(item._3),
        BigDecimal(item._4),
        BigDecimal(item._5),
        BigDecimal(item._6)
      )
    )

}
