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
    maSize:           Int,
    quoteSymbol:      String,
    leverage:         Int,
    maxHolds:         Int,
    apiKey:           String,
    apiSecret:        String,
    notifyWebhook:    String,
    heartBeatWebhook: String,
    exceptionWebhook: String
)

val logger  = Logger("main")
def start() = {
    logger.info("start binance bot...")
    logger.info("load config")
    val cfg          = loadConfig[AppConfig]("config.yaml")
    logger.info("create notify bot")
    val notifyBot    = FeishuNotify(cfg.notifyWebhook)
    val heartBeatBot = FeishuNotify(cfg.heartBeatWebhook)
    val exceptionBot = FeishuNotify(cfg.exceptionWebhook)
    logger.info("init binance api")
    val binanceApi   = new BinanceApi(
      cfg.apiKey,
      cfg.apiSecret,
      cfg.leverage,
      heartBeatBot
    ) {}
    binanceApi.start()
    logger.info("get all busd symbols")
    // binanceApi.sendOrder("BTCBUSD", TradeSide.BUY, 0.001, Some(10000), Some(30000))
    val interval     = cfg.interval
    val symbols      = binanceApi.allSymbol().filter(_.symbol.endsWith(cfg.quoteSymbol))
    logger.info("create strategies for symbols")
    val strategies   = symbols.map(s => {
        val bot = MaStrategy(s.symbol, interval, cfg.maSize, cfg.maxHolds, binanceApi, notifyBot, exceptionBot)
        bot.start()
        bot
    })
}

@main def main: Unit = {
    // backtest()
    start()
}
