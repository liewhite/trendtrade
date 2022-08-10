import scala.collection.mutable
import sttp.client3.okhttp.quick._
import io.github.liewhite.json.{*, given}
import strategy.*
import binance.*
import io.github.liewhite.config.loadConfig
import io.github.liewhite.json.given
import java.time.Duration
import notifier.FeishuNotify
import com.typesafe.scalalogging.Logger
import cats.syntax.validated

case class AppConfig(
    interval:         String,
    shortMa:          Int,
    midMa:            Int,
    longMa:           Int,
    quoteSymbol:      String,
    leverage:         Int,
    maxHolds:         Int,
    apiKey:           String,
    apiSecret:        String,
    notifyWebhook:    String,
    heartBeatWebhook: String,
    exceptionWebhook: String,
    leastSupply:      BigDecimal,
    leastOpenedValue: BigDecimal
)

val logger = Logger("main")

def loadCfg(): AppConfig = {
    logger.info("start binance bot...")
    logger.info("load config")
    val cfg = loadConfig[AppConfig]("config.yaml")
    logger.info("create notify bot")
    cfg
}
def start()              = {
    val cfg = loadCfg()

    val notifyBot    = FeishuNotify(cfg.notifyWebhook)
    val heartBeatBot = FeishuNotify(cfg.heartBeatWebhook)
    val exceptionBot = FeishuNotify(cfg.exceptionWebhook)

    logger.info("init binance api")
    val binanceApi = new BinanceApi(
      cfg.apiKey,
      cfg.apiSecret,
      cfg.leverage,
      heartBeatBot,
      cfg.quoteSymbol,
      cfg.leastSupply
    ) {}
    binanceApi.start()

    logger.info("get all busd symbols")
    val interval   = cfg.interval
    val allSymbols = binanceApi
        .allSymbol()
        .filter(_.symbol.endsWith(cfg.quoteSymbol))
    // .filter(item => cfg.symbolBl.contains(item.symbol))
    logger.info("create strategies for symbols")
    // binanceApi.sendOrder("BTCBUSD", TradeSide.SELL, 0.001, Some(21000), Some(18000))
    // val positionMgr = PositionMgr("BTCBUSD", binanceApi, 10, notifyBot, exceptionBot)
    // positionMgr.closeManually(-1, 0.001)
    // println(binanceApi.getOpenInterest("ONEUSDT"))
    // println(binanceApi.supply(1))

    // 持仓小于1000w的忽略
    // val validSymbols = allSymbols.filter(item => {
    //     val value = binanceApi.getOpenInterest(item.symbol)
    //     value > cfg.leastOpenedValue
    // })
    // validSymbols.foreach(println)
    val validSymbols = Vector(SymbolMeta("BTCUSDT", 0.01, 0.01))

    val strategies = validSymbols.map(s => {
        val bot = ZsStrategy(
          s.symbol,
          interval,
          // cfg.shortMa,
          cfg.midMa,
        //   cfg.longMa,
          cfg.maxHolds,
          binanceApi,
          notifyBot,
          exceptionBot
        )
        bot.start()
        bot
    })
    // strategies(0).czscK.data.foreach(println)
}

@main def main: Unit = {
    start()
}
