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
      exceptionBot,
      heartBeatBot,
      cfg.quoteSymbol,
      cfg.leastSupply
    ) {}
    binanceApi.start()

    val interval   = cfg.interval

    val allSymbols = binanceApi
        .allSymbol()
        .filter(_.symbol.endsWith(cfg.quoteSymbol))

    val symbolsAndOpened = allSymbols.map(item => {
        val value = binanceApi.getOpenInterest(item.symbol)
        (item, value)
    })

    val validSymbols = symbolsAndOpened.sortBy(_._2).reverse.take(cfg.maxHolds).map(_._1.symbol).toSet
    val currentHoldingSymbol = binanceApi.getPositions().filter(_.positionAmt != 0).map(_.symbol).toSet
    // 当前持有但是跌出持仓排名的
    logger.info("load current holding but not in target symbols:")
    (currentHoldingSymbol -- validSymbols).foreach(println)
    logger.info("all symbols: ")
    validSymbols.zipWithIndex.foreach(println)

    // 市值前N加上当前持有symbol
    val watchSymbols = validSymbols ++ currentHoldingSymbol

    val strategies = watchSymbols.map(s => {
        val bot = MaMacdKdjStrategy(
          s,
          interval,
          cfg.shortMa,
          cfg.maxHolds,
          binanceApi,
          notifyBot,
          exceptionBot
        )
        bot.start()
        bot
    })
}

@main def main: Unit = {
    start()
}
