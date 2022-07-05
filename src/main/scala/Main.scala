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
    val symbols      = binanceApi.allSymbol().filter(_.symbol.endsWith("BUSD")).filter(!_.symbol.contains("DODO")).map(_.symbol)
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
    val cfg          = loadConfig[AppConfig]("config.yaml")
    val binanceApi   = new BinanceApi(
      cfg.apiKey,
      cfg.apiSecret,
      5,
      null
    ) {}
    val symbols      = binanceApi.allSymbol().filter(_.symbol.endsWith("BUSD")).map(_.symbol)
    val total = symbols.map(item => {
        val ks15   = data.getSymbolK(item, "1h")
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
          s"${item} tx count: ${bot.closed.length} ,fee: ${fee} profit: ${profit} precent: ${(profit / bot.klines(0).close * 100).intValue}%"
        )
        (profit / bot.klines(0).close * 100).intValue
    }).sum
    println("total profit: " + total)

}
