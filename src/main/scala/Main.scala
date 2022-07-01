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
    exceptionWebhook: String,
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
    logger.info("get all busd symbols")
    val symbols      = binanceApi.allSymbol().filter(_.symbol.endsWith("BUSD")).map(_.symbol)
    val interval     = cfg.interval
    logger.info("create strategies for symbols")
    val strategies   = symbols.map(s => {
        val bot = MaBack2Strategy(s, interval, binanceApi, notifyBot, exceptionBot)
        bot.start()
        bot
    })
}

@main def main: Unit = {
    // backtest()
    start()
}

def backtest() = {
    // Vector("BTCBUSD","ETHBUSD","BNBUSD","WAVESUSD","BUSD","BTCBUSD",)
    val ks15   = data.getSymbolK("1000LUNCBUSD", "1h")
    val bot    = MaBackTest2()
    ks15.foreach(k => {
        bot.step(k)
    })
    val profit = bot.closed
        .map(item =>
            (item.closeAt.get - item.openAt) * item.direction - item.openAt * 0.00036 - item.closeAt.get * 0.00036
        )
        .sum
    val fee    = bot.closed.map(item => item.openAt * 0.00036 + item.closeAt.get * 0.00036).sum

    println(
      s"tx count: ${bot.closed.length} ,fee: ${fee} profit: ${profit} precent: ${(profit / bot.klines(0).close * 100).intValue}%"
    )
}
