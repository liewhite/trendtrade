package data

import sttp.client3.okhttp.quick._
import strategy.Kline
import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime
import zio.json.*

val rootUrl = "https://fapi.binance.com"

def getLatest500K(symbol: String, interval: String): List[Kline] = {
    val response = quickRequest
        .get(
          uri"$rootUrl/fapi/v1/continuousKlines?pair=${symbol}&contractType=PERPETUAL&interval=${interval}"
        )
        .header(
          "X-MBX-APIKEY",
          "HOPEvPkEmARRPJ2Vw8MQvXGDnA9nclxRqbGWODC8NzVD79wSOdHsn6YwX7nSjYG6"
        )
        .send(backend)

    response.body
        .fromJson[List[
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
        ]].toOption.get
        .map(item =>
            Kline(
              ZonedDateTime
                  .ofInstant(Instant.ofEpochMilli(item._1), ZoneId.systemDefault),
              BigDecimal(item._2),
              BigDecimal(item._3),
              BigDecimal(item._4),
              BigDecimal(item._5),
              BigDecimal(item._6)
            )
        )
}

def getSymbolK(symbol: String, interval: String, limit: Int = 1500): List[Kline] = {
    val endTime   = ZonedDateTime.now().minusDays(0)
    val startTime = endTime.minusDays(60)
    val startTs   = startTime.toInstant().toEpochMilli
    val endTs     = endTime.toInstant().toEpochMilli
    val response  = quickRequest
        .get(
          uri"$rootUrl/fapi/v1/continuousKlines?limit=${limit}&pair=${symbol}&startTime=${startTs}&endTime=${endTs}&contractType=PERPETUAL&interval=${interval}"
        )
        .header(
          "X-MBX-APIKEY",
          "HOPEvPkEmARRPJ2Vw8MQvXGDnA9nclxRqbGWODC8NzVD79wSOdHsn6YwX7nSjYG6"
        )
        .send(backend)

    response.body
        .fromJson[List[
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
        ]].toOption.get
        .map(item =>
            Kline(
              ZonedDateTime
                  .ofInstant(Instant.ofEpochMilli(item._1), ZoneId.systemDefault),
              BigDecimal(item._2),
              BigDecimal(item._4),
              BigDecimal(item._3),
              BigDecimal(item._5),
              BigDecimal(item._6)
            )
        )

}
