import scala.collection.mutable
import sttp.client3.okhttp.quick._
import io.github.liewhite.json.{*, given}
import java.time.LocalDateTime
import java.time.LocalDate
import strategy.*
import binance.*
import io.github.liewhite.config.loadConfig
import io.github.liewhite.json.given
import java.time.Duration
import notifier.FeishuNotify
import com.typesafe.scalalogging.Logger

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
    symbolBl:         Option[Vector[String]]
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
      heartBeatBot
    ) {}
    binanceApi.start()

    logger.info("get all busd symbols")
    val interval = cfg.interval
    val symbols  = binanceApi
        .allSymbol()
        .filter(_.symbol.endsWith(cfg.quoteSymbol))
        // .filter(item => cfg.symbolBl.contains(item.symbol))
    logger.info("create strategies for symbols")
    symbols.foreach(println)

    val strategies = symbols.map(s => {
        val bot = MasStrategy(
          s.symbol,
          interval,
          cfg.shortMa,
          cfg.midMa,
          cfg.longMa,
          cfg.maxHolds,
          binanceApi,
          notifyBot,
          exceptionBot
        )
        bot.start()
        bot
    })
    // binanceApi.sendOrder("BTCBUSD", TradeSide.SELL, 0.001, Some(21000), Some(18000))
    // val positionMgr = PositionMgr("BTCBUSD", binanceApi, 10, notifyBot, exceptionBot)
    // positionMgr.closeManually(-1, 0.001)
}

@main def main: Unit = {
    start()
}
