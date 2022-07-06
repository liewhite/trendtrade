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
      5,
      heartBeatBot
    ) {}
    binanceApi.start()
    logger.info("get all busd symbols")
    // binanceApi.sendOrder("BTCBUSD", TradeSide.BUY, 0.001, Some(10000), Some(20000))
    // val symbols      = binanceApi
    //     .allSymbol()
    //     .filter(_.symbol.endsWith("BUSD"))
    //     .filter(!_.symbol.contains("DODO"))
    //     .map(_.symbol)
    val interval     = cfg.interval
    val symbols = Vector("BTCBUSD")
    logger.info("create strategies for symbols")
    val strategies   = symbols.map(s => {
        val bot = KdjStrategy(s, interval, binanceApi, notifyBot, exceptionBot)
        bot.start()
        bot
    })
}

@main def main: Unit = {
    // backtest()
    start()
}
