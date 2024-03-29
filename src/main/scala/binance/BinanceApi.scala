package binance

import sttp.client3.okhttp.quick._
import common.ExecutionPool.e
import scala.concurrent.duration.*
import scala.collection.concurrent.{TrieMap => CMap}
import sttp.model.Uri
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.time.ZonedDateTime
import java.security.MessageDigest
import java.math.BigInteger
import org.apache.commons.codec.binary.Hex
import scala.collection.concurrent
import okhttp3.*
import scala.concurrent.*
import com.typesafe.scalalogging.Logger
import strategy.Kline
import notifier.Notify
import zio.json.*
import java.time.*
import java.time.Duration

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

// case class OpenOrder(
//     symbol:   String,
//     `type`:   String,
//     quantity: String,
//     side:     TradeSide
// ) derives JsonDecoder

// case class StopOrder(
//     symbol:      String,
//     `type`:      String,
//     quantity:    String,
//     side:        TradeSide,
//     stopPrice:   String,
//     timeInForce: String,
//     reduceOnly:  String
// ) derives JsonDecoder

case class BalanceResponse(
    asset:            String,
    balance:          String,
    availableBalance: String
) derives JsonDecoder
case class OrderResponse(
    orderId: Long
) derives JsonDecoder,JsonEncoder
case class ListenKeyResponse(
    listenKey: String
) derives JsonDecoder

case class OrderFillDetail(
    i: Long,
    X: String
) derives JsonDecoder

case class OrderFillResponse(
    e: String,
    o: OrderFillDetail
) derives JsonDecoder
case class PriceResponse(
    price: String
) derives JsonDecoder
case class AccountInfoResponse(
    positions: Vector[AccountInfoResponsePosition]
) derives JsonDecoder
case class AccountInfoResponsePosition(
    symbol:      String,
    entryPrice:  String,
    // 负数表示空单?
    positionAmt: String
) derives JsonDecoder
case class PositionFromBinance(
    symbol:      String,
    entryPrice:  BigDecimal,
    // 负数表示空单
    positionAmt: BigDecimal
) derives JsonDecoder
case class StreamKlineResponseDetail(
    t: Long, // 以开始时间为准
    // T: Long,
    o: String,
    l: String,
    h: String,
    c: String,
    v: String,
    x: Boolean
) derives JsonDecoder

case class StreamKlineResponse(
    k: StreamKlineResponseDetail
) derives JsonDecoder
case class SymbolMetaFilter(
    filterType: String,
    tickSize:   Option[String],
    stepSize:   Option[String]
) derives JsonDecoder

case class SymbolMetaResponseItem(
    symbol:       String,
    contractType: String,
    filters:      Vector[SymbolMetaFilter]
) derives JsonDecoder

case class SymbolMetaResponse(
    symbols: Vector[SymbolMetaResponseItem]
) derives JsonDecoder

case class OpenInterestResponse(
    sumOpenInterestValue: String
) derives JsonDecoder

case class SymbolMeta(
    symbol:    String,
    stepSize:  BigDecimal,
    priceStep: BigDecimal
) derives JsonDecoder
case class TradeResponse(
    symbol: String,
    price:  String,
    side:   String,
    time:   Long
) derives JsonDecoder

trait BinanceApi(
    val apiKey:    String,
    val apiSecret: String,
    val leverage:  Int,
    importNtf:     Notify,
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

    def allSymbol(): Vector[SymbolMeta] = {
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

                val res = response.body.fromJson[SymbolMetaResponse].toOption.get
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

                val res = response.body.fromJson[SymbolMetaResponse].toOption.get
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
                  val res = x.fromJson[StreamKlineResponse].toOption.get
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

    def getHistory(symbol: String, interval: String, limit: Int = 499): Vector[Kline] = {
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
                  BigDecimal(item._4), // low
                  BigDecimal(item._3), // high
                  BigDecimal(item._5),
                  BigDecimal(item._6)
                )
            )
            .toVector
    }

    def getPositions(): Vector[PositionFromBinance] = {
        val infoUrl   = uri"${binanceHttpBaseUrl}/fapi/v2/account"
        val signedReq = signReq(infoUrl)
        val response  = quickRequest
            .get(signedReq)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)
        response.body
            .fromJson[AccountInfoResponse].toOption.get
            .positions
            .map(item => {
                PositionFromBinance(
                  item.symbol,
                  BigDecimal(item.entryPrice),
                  BigDecimal(item.positionAmt)
                )
            })
            .toVector
    }

    def getPosition(symbol: String): Vector[PositionFromBinance] = {
        getPositions().filter(item => item.positionAmt != 0 && item.symbol == symbol)
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
            .fromJson[Vector[TradeResponse]].toOption.get
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
        val res  = lres.body.fromJson[PriceResponse].toOption.get
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
        val res = response.body.fromJson[Vector[OpenInterestResponse]].toOption.get
        if (res.isEmpty) {
            0
        } else {
            BigDecimal(res(0).sumOpenInterestValue)
        }
    }

    def doGetTotalBalance(): (BigDecimal, BigDecimal) = {
        val req     = uri"${binanceHttpBaseUrl}/fapi/v2/balance"
        val signed  = signReq(req)
        val lres    = quickRequest
            .get(signed)
            .header(
              "X-MBX-APIKEY",
              apiKey
            )
            .send(backend)
        val res     = lres.body.fromJson[Vector[BalanceResponse]].toOption.get
        val busdRes = res.filter(_.asset == quoteSymbol)
        if (busdRes.isEmpty) {
            (0, 0)
        } else {
            (BigDecimal(busdRes(0).balance), BigDecimal(busdRes(0).availableBalance))
        }

    }

    var balance: (BigDecimal, BigDecimal)    = (0, 0)
    var balanceLastUpdateTime: ZonedDateTime = ZonedDateTime.now().minusHours(1)
    val balanceLock                          = Object()

    // (total, available)
    // 缓存5s，开仓失败就失败吧
    def getTotalBalance(): (BigDecimal, BigDecimal) = {
        balanceLock.synchronized {
            val now = ZonedDateTime.now()
            if (Duration.between(balanceLastUpdateTime, now).getSeconds() > 5) {
                val (total, current) = doGetTotalBalance()
                balance = (total, current)
                balanceLastUpdateTime = now
                (total, current)
            } else {
                balance
            }
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
        val orderRes        = lres.body.fromJson[Vector[OrderResponse]].toOption.get
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
            val orderRes  = lres.body.fromJson[Vector[OrderResponse]].toOption.get
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
            val key = lres.body.fromJson[ListenKeyResponse].toOption.get.listenKey
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
            (Future {
                while (true) {
                    Thread.sleep(1000 * 60 * 30)
                    updateListenKey()
                }
            })
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
                      val res = x.fromJson[OrderFillResponse].toOption.get
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

    def transfer(amount: BigDecimal, direction: String) = {
        val url       = uri"${binanceHttpBaseUrl2}/sapi/v1/futures/transfer"
            .addParam("asset", quoteSymbol)
            .addParam("amount", amount.toString())
            .addParam("type", direction)
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
        val action    = if (direction == "1") { "入金" }
        else { "出金" }
        importNtf.sendNotify(s"${action}: ${amount}, 结果: ${response.code}, ${res}")
    }

    // 补充合约账户余额
    def autoSupply() = {
        val (total, _) = getTotalBalance()
        ntf.sendNotify(s"当前保证金: ${total},所需保证金: ${totalSupply}")

        if (total < totalSupply * 0.99) {
            transfer((totalSupply - total).longValue + 1, "1")
        } else if (total > totalSupply * 1.05) {
            if ((total - totalSupply).longValue - 1 > 0) {
                transfer((total - totalSupply).longValue - 1, "2")
            }
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
