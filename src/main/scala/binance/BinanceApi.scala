package binance

import sttp.client3.okhttp.quick._
import common.ExecutionPool.e
import scala.concurrent.duration.*
import io.github.liewhite.json.{*, given}
import scala.collection.concurrent.{TrieMap => CMap}
import sttp.model.Uri
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.time.ZonedDateTime
import java.security.MessageDigest
import java.math.BigInteger
import org.apache.commons.codec.binary.Hex
import scala.collection.concurrent
import java.time.Duration
import okhttp3.Request
import okhttp3.OkHttpClient
import okhttp3.WebSocketListener
import okhttp3.WebSocket
import okhttp3.Response
import scala.concurrent.*
import com.typesafe.scalalogging.Logger
import strategy.Kline
import java.time.Instant
import java.time.ZoneId
import notifier.Notify
import cats.syntax.validated

// import com.typesafe.lo

enum TradeSide    {
    case BUY
    case SELL
}
enum PositionSide {
    case LONG
    case SHORT
}
enum MarginType   {
    case ISOLATED
    case CROSSED
}
case class OpenOrder(
    symbol:   String,
    `type`:   String,
    quantity: String,
    side:     TradeSide
)

case class StopOrder(
    symbol:      String,
    `type`:      String,
    quantity:    String,
    side:        TradeSide,
    stopPrice:   String,
    timeInForce: String,
    reduceOnly:  String
)

case class BalanceResponse(
    asset:            String,
    balance:          String,
    availableBalance: String
)
case class OrderResponse(
    orderId: Long
)
case class ListenKeyResponse(
    listenKey: String
)

case class OrderFillDetail(
    i: Long,
    X: String
)

case class OrderFillResponse(
    e: String,
    o: OrderFillDetail
)
case class PriceResponse(
    price: String
)
case class AccountInfoResponse(
    positions: Vector[AccountInfoResponsePosition]
)
case class AccountInfoResponsePosition(
    symbol:      String,
    entryPrice:  String,
    // 负数表示空单?
    positionAmt: String
)
case class PositionFromBinance(
    symbol:      String,
    entryPrice:  BigDecimal,
    // 负数表示空单
    positionAmt: BigDecimal
)
case class StreamKlineResponseDetail(
    t: Long, // 以开始时间为准
    // T: Long,
    o: String,
    l: String,
    h: String,
    c: String,
    v: String,
    x: Boolean
)

case class StreamKlineResponse(
    k: StreamKlineResponseDetail
)
case class SymbolMetaFilter(
    filterType: String,
    tickSize:   Option[String],
    stepSize:   Option[String]
)

case class SymbolMetaResponseItem(
    symbol:       String,
    contractType: String,
    filters:      Vector[SymbolMetaFilter]
)

case class SymbolMetaResponse(
    symbols: Vector[SymbolMetaResponseItem]
)
case class OpenInterestResponse(
    sumOpenInterestValue: String
)

case class SymbolMeta(
    symbol:    String,
    stepSize:  BigDecimal,
    priceStep: BigDecimal
)
case class TradeResponse(
    symbol: String,
    price:  String,
    side:   String,
    time:   Long
)

trait BinanceApi(
    val apiKey:    String,
    val apiSecret: String,
    val leverage:  Int,
    ntf:           Notify,
    quoteSymbol:   String,
    totalSupply:   BigDecimal
) {
    val logger = Logger("trader")

    val binanceHttpBaseUrl  = "https://fapi.binance.com"
    val binanceHttpBaseUrl2 = "https://api.binance.com"
    val streamUrl           = "wss://fstream.binance.com/ws"

    val readySymbol                          = CMap.empty[String, Boolean]
    val orders                               = concurrent.TrieMap.empty[Long, Boolean]
    var listenKey: String                    = ""
    var streamReady: Promise[Boolean]        = Promise()
    var symbolMetas: Map[String, SymbolMeta] = Map.empty
    val heartBeat                            = CMap.empty[String, ZonedDateTime]

    def allSymbol(): Vector[SymbolMeta]        = {
        this.synchronized {
            if (symbolMetas.isEmpty) {
                val response = quickRequest
                    .get(
                      uri"${binanceHttpBaseUrl}/fapi/v1/exchangeInfo"
                    )
                    .header(
                      "X-MBX-APIKEY",
                      apiKey
                    )
                    .send(backend)

                val res = response.body.fromJsonMust[SymbolMetaResponse]
                symbolMetas = res.symbols
                    .filter(_.contractType == "PERPETUAL")
                    .map(item => {
                        val stepSize  = BigDecimal(
                          item.filters
                              .filter(_.filterType == "MARKET_LOT_SIZE")(0)
                              .stepSize
                              .get
                        )
                        val priceStep = BigDecimal(
                          item.filters
                              .filter(_.filterType == "PRICE_FILTER")(0)
                              .tickSize
                              .get
                        )
                        SymbolMeta(item.symbol, stepSize, priceStep)
                    })
                    .map(item => (item.symbol, item))
                    .toMap

            }
        }
        symbolMetas.map(_._2).toVector
    }
    def symbolMeta(symbol: String): SymbolMeta = {
        this.synchronized {
            if (symbolMetas.isEmpty) {
                val response = quickRequest
                    .get(
                      uri"${binanceHttpBaseUrl}/fapi/v1/exchangeInfo"
                    )
                    .header(
                      "X-MBX-APIKEY",
                      apiKey
                    )
                    .send(backend)

                val res = response.body.fromJsonMust[SymbolMetaResponse]
                symbolMetas = res.symbols
                    .filter(_.contractType == "PERPETUAL")
                    .map(item => {
                        val stepSize  = BigDecimal(
                          item.filters
                              .filter(_.filterType == "MARKET_LOT_SIZE")(0)
                              .stepSize
                              .get
                        )
                        val priceStep = BigDecimal(
                          item.filters
                              .filter(_.filterType == "PRICE_FILTER")(0)
                              .tickSize
                              .get
                        )
                        SymbolMeta(item.symbol, stepSize, priceStep)
                    })
                    .map(item => (item.symbol, item))
                    .toMap

            }
        }
        symbolMetas(symbol)
    }

    def subscribeKlines(
        symbol: String,
        interval: String,
        klineCallback: Kline => Unit
    ): Unit = {
        val client = new OkHttpClient()
        val req    = new Request.Builder()
            .url(
              streamUrl + s"/${symbol.toLowerCase}_perpetual@continuousKline_${interval}"
            )
            .build()
        val ws     = client.newWebSocket(
          req,
          new WebSocketListener {
              override def onOpen(x: WebSocket, y: Response): Unit  = {
                  logger.info("market stream connected")
              }
              override def onMessage(s: WebSocket, x: String): Unit = {
                  val res = x.fromJsonMust[StreamKlineResponse]
                  // 记录心跳时间
                  heartBeat.update(symbol, ZonedDateTime.now)
                  // logger.info(s"market info: ${res}")
                  //   logger.info(s"${symbol}: ${res.k}")
                  klineCallback(
                    Kline(
                      ZonedDateTime.ofInstant(
                        // Instant.ofEpochMilli(((res.k.t / 1000).longValue + 1) * 1000),
                        Instant.ofEpochMilli(((res.k.t / 1000).longValue) * 1000),
                        ZoneId.systemDefault
                      ),
                      BigDecimal(res.k.o),
                      BigDecimal(res.k.l),
                      BigDecimal(res.k.h),
                      BigDecimal(res.k.c),
                      BigDecimal(res.k.v),
                      res.k.x
                    )
                  )
              }

              override def onClosing(x: WebSocket, y: Int, z: String): Unit = {
                  logger.info(s"market websocket closing: ${symbol}")
              }

              override def onFailure(
                  s: WebSocket,
                  e: Throwable,
                  x: Response
              ): Unit = {
                  logger.error(e.toString)
                  e.printStackTrace
                  logger.info("market websocket closed, reloading")
                  s.cancel()
                  Thread.sleep(3000)
                  subscribeKlines(symbol, interval, klineCallback)
              }
          }
        )
    }

    def getHistory(symbol: String, interval: String, limit: Int = 250): Vector[Kline] = {
        val response = quickRequest
            .get(
              uri"${binanceHttpBaseUrl}/fapi/v1/continuousKlines?limit=${limit}&pair=${symbol}&contractType=PERPETUAL&interval=${interval}"
            )
            .header(
              "X-MBX-APIKEY",
              apiKey
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
                  ZonedDateTime
                      .ofInstant(Instant.ofEpochMilli(item._1), ZoneId.systemDefault),
                  BigDecimal(item._2),
                  BigDecimal(item._4), // low
                  BigDecimal(item._3), // high
                  BigDecimal(item._5),
                  BigDecimal(item._6)
                )
            )
            .toVector
    }

    def getPositions(symbol: String): Vector[PositionFromBinance] = {
        val infoUrl   = uri"${binanceHttpBaseUrl}/fapi/v2/account"
        // 更改逐仓， 杠杆倍数
        val signedReq = signReq(infoUrl)
        val response  = quickRequest
            .get(signedReq)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)

        // logger.info(response.body)

        response.body
            .fromJsonMust[AccountInfoResponse]
            .positions
            .map(item => {
                PositionFromBinance(
                  item.symbol,
                  BigDecimal(item.entryPrice),
                  BigDecimal(item.positionAmt)
                )
            })
            .toVector
            .filter(item => item.positionAmt != 0 && item.symbol == symbol)
    }

    def getTrades(symbol: String): Vector[TradeResponse] = {
        val url       = uri"${binanceHttpBaseUrl}/fapi/v1/userTrades"
        // 更改逐仓， 杠杆倍数
        val signedReq = signReq(url)
        val response  = quickRequest
            .get(signedReq)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)

        response.body
            .fromJsonMust[Vector[TradeResponse]]
            .filter(_.symbol == symbol)
    }
    def getSymbolPrice(symbol: String): BigDecimal       = {
        val req  = uri"${binanceHttpBaseUrl}/fapi/v1/ticker/price?symbol=${symbol}"
        val lres = quickRequest
            .get(req)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)
        val res  = lres.body.fromJsonMust[PriceResponse]
        return BigDecimal(res.price)
    }

    // 调整pair杠杆和逐仓
    def prepareSymbol(symbol: String) = {
        val leverageUrl   = uri"${binanceHttpBaseUrl}/fapi/v1/leverage"
        val marginTypeUrl = uri"${binanceHttpBaseUrl}/fapi/v1/marginType"
        // 更改逐仓， 杠杆倍数
        val req           = marginTypeUrl
            .addParam("symbol", symbol)
            .addParam("marginType", MarginType.ISOLATED.toString)
        val signedReq     = signReq(req)
        val response      = quickRequest
            .post(signedReq)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)
        logger.info(s"init margin result: ${response.body}")

        val leverageReq       = leverageUrl
            .addParam("symbol", symbol)
            .addParam("leverage", leverage.toString)
        val signedLeverageReq = signReq(leverageReq)
        val lres              = quickRequest
            .post(signedLeverageReq)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)

        logger.info(s"init leverage result: ${lres.body}")

        readySymbol.addOne(symbol, true)
    }

    def getOpenInterest(symbol: String): BigDecimal = {
        val response = quickRequest
            .get(
              uri"${binanceHttpBaseUrl}/futures/data/openInterestHist"
                  .addParam("symbol", symbol)
                  .addParam("period", "5m")
                  .addParam("limit", "1")
            )
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)

        // logger.info(s"openInterest for ${symbol}: ${response.body}")
        val res = response.body.fromJsonMust[Vector[OpenInterestResponse]]
        if (res.isEmpty) {
            0
        } else {
            BigDecimal(res(0).sumOpenInterestValue)
        }
    }

    // (total, available)
    def getTotalBalance(): (BigDecimal, BigDecimal) = {

        val req     = uri"${binanceHttpBaseUrl}/fapi/v2/balance"
        val signed  = signReq(req)
        val lres    = quickRequest
            .get(signed)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)
        val res     = lres.body.fromJsonMust[Vector[BalanceResponse]]
        val busdRes = res.filter(_.asset == quoteSymbol)
        if (busdRes.isEmpty) {
            (0, 0)
        } else {
            (BigDecimal(busdRes(0).balance), BigDecimal(busdRes(0).availableBalance))
        }
    }

    // 发送交易并等待成交
    def sendOrder(
        symbol: String,
        side: TradeSide,
        quantity: BigDecimal,
        sl: Option[BigDecimal] = None,
        tp: Option[BigDecimal] = None,
        close: Boolean = false
    ): Unit = {
        if (!readySymbol.contains(symbol)) {
            prepareSymbol(symbol)
        }

        val order = Map(
          "symbol"           -> symbol,
          "type"             -> "MARKET",
          "quantity"         -> quantity.toString(),
          "side"             -> side.toString,
          "newOrderRespType" -> "RESULT",
          "reduceOnly"       -> close.toString()
        )

        val batchOrderUrl   = uri"${binanceHttpBaseUrl}/fapi/v1/batchOrders"
        val signedOpenOrder = signReq(uri"${batchOrderUrl}?batchOrders=${Vector(order).toJson}")
        logger.info(s"send order: ${signedOpenOrder}")
        val lres            = quickRequest
            .post(signedOpenOrder)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)
        logger.info(s"send order response: ${lres.body}")
        val orderRes        = lres.body.fromJsonMust[Vector[OrderResponse]]
        orders.addOne(orderRes(0).orderId, false)
        var maxTry          = 25
        while (orders.get(orderRes(0).orderId).isEmpty && maxTry > 0) {
            Thread.sleep(200)
            maxTry -= 1
        }
        orders.remove(orderRes(0).orderId)
        if (maxTry > 0) {} else {
            throw Exception("timeout")
        }

        var batchOrders = Vector.empty[Map[String, String]]
        val stopSide    = if (side == TradeSide.BUY) TradeSide.SELL else TradeSide.BUY
        // 应该挂LIMIT单
        if (tp.nonEmpty) {
            batchOrders = batchOrders.appended(
              Map(
                "symbol"           -> symbol,
                "type"             -> "TAKE_PROFIT",
                "side"             -> stopSide.toString(),
                "reduceOnly"       -> "true",
                "stopPrice"        -> tp.get.toString,
                "price"            -> tp.get.toString,
                "quantity"         -> quantity.toString(),
                "timeInForce"      -> "GTE_GTC",
                "newOrderRespType" -> "RESULT",
                "workingType"      -> "CONTRACT_PRICE"
              )
            )
        }

        if (sl.nonEmpty) {
            batchOrders = batchOrders.appended(
              Map(
                "symbol"           -> symbol,
                "type"             -> "STOP_MARKET",
                "side"             -> stopSide.toString(),
                "closePosition"    -> "true",
                "stopPrice"        -> sl.get.toString,
                "timeInForce"      -> "GTE_GTC",
                "newOrderRespType" -> "RESULT",
                "workingType"      -> "MARK_PRICE"
              )
            )
        }
        if (batchOrders.nonEmpty) {
            val paramsJson = batchOrders.toJson
            var req        = uri"${batchOrderUrl}?batchOrders=${paramsJson}"

            val signedReq = signReq(req)
            logger.info(s"send sl tp order: ${signedReq}")
            val slTpRes   = quickRequest
                .post(signedReq)
                .header(
                  "X-MBX-APIKEY",
                  apiKey
                )
                .send(backend)
            logger.info(s"send order response: ${lres.body}")
            val orderRes  = lres.body.fromJsonMust[Vector[OrderResponse]]
            logger.info("sl tp order response: " + orderRes.toJson)
        }
    }

    def updateListenKey() = {
        logger.info("update listen key")
        val req  = uri"${binanceHttpBaseUrl}/fapi/v1/listenKey"
        val lres = quickRequest
            .post(req)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)
        try {
            val key = lres.body.fromJsonMust[ListenKeyResponse].listenKey
            listenKey = key
        } catch {
            case e: Exception => {
                logger.warn("failed update listen key" + e)
                e.printStackTrace()
            }
        }
    }

    def listenOrder(updateKey: Boolean = true): Unit = {
        // 获取listen key
        if (updateKey) {
            updateListenKey()
            Future {
                while (true) {
                    Thread.sleep(1000 * 60 * 30)
                    updateListenKey()
                }
            }
        }

        val client = new OkHttpClient()
        val req    = new Request.Builder().url(streamUrl + s"/${listenKey}").build()
        val ws     = client.newWebSocket(
          req,
          new WebSocketListener {
              override def onOpen(x: WebSocket, y: Response): Unit = {
                  logger.info("trade stream connected")
                  if (updateKey) {
                      streamReady.success(true)
                  }
              }

              override def onMessage(s: WebSocket, x: String): Unit         = {
                  try {
                      val res = x.fromJsonMust[OrderFillResponse]
                      if (res.e == "ORDER_TRADE_UPDATE" && res.o.X == "FILLED") {
                          logger.info(s"order filled: ${res.o.i}")
                          orders.update(res.o.i, true)
                      } else {
                          logger.info(s"ignore trade msg: ${x}")
                      }
                  } catch {
                      case e: Exception => {
                          logger.info(s"ignore other msg: ${x}")
                      }
                  }
              }
              override def onClosing(x: WebSocket, y: Int, z: String): Unit = {
                  logger.info(s"account websocket closing")
              }
              override def onFailure(
                  s: WebSocket,
                  e: Throwable,
                  x: Response
              ): Unit = {
                  logger.error(e.toString)
                  e.printStackTrace
                  logger.info("account websocket closed, reloading")
                  s.cancel()
                  Thread.sleep(3000)
                  listenOrder(false)
              }
          }
        )
    }

    def signReq(s: Uri): Uri = {
        val now    = ZonedDateTime.now().toInstant.toEpochMilli
        val withTs = s.addParam("timestamp", now.toString)

        val qs = withTs.params.toString

        val secret     = new SecretKeySpec(apiSecret.getBytes, "SHA256")
        val hmacSha256 = Mac.getInstance("HmacSHA256")
        val secKey     = new SecretKeySpec(apiSecret.getBytes(), "2")
        hmacSha256.init(secKey)
        val sign       = new String(Hex.encodeHex(hmacSha256.doFinal(qs.getBytes())));
        withTs.addParam("signature", sign)
    }

    def supply(amount: BigDecimal) = {
        val url       = uri"${binanceHttpBaseUrl2}/sapi/v1/futures/transfer"
            .addParam("asset", quoteSymbol)
            .addParam("amount", amount.toString())
            .addParam("type", "1")
        val signedReq = signReq(url)
        val response  = quickRequest
            .post(signedReq)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)
        val res       = response.body
        logger.info(s"补充保证金: ${response.code}, ${res}")
        ntf.sendNotify(s"补充保证金: ${response.code}, ${res}")
    }

    // 补充合约账户余额
    def autoSupply() = {
        val (total, _) = getTotalBalance()
        ntf.sendNotify(s"当前保证金: ${total},所需最低保证金: ${totalSupply}")

        // 如果保证金低于最低要求,补足
        if (total < totalSupply * 0.99) {
            // 取整
            supply((totalSupply - total).longValue + 1)
        }
    }

    def startHeartBeatLoop() = {
        Future {
            while (true) {
                val now = ZonedDateTime.now
                ntf.sendNotify(
                  heartBeat
                      .map(item => {
                          val dt =
                              if (Duration.between(item._2, now).getSeconds > 60) s"*${item._2}*"
                              else item._2
                          s"${item._1}: ${dt}"
                      })
                      .mkString("\n")
                )
                autoSupply()
                Thread.sleep(30 * 1000)
            }
        }
    }
    def start()              = {
        startHeartBeatLoop()
        listenOrder()
        Await.result(streamReady.future, 10.seconds)
    }
}
