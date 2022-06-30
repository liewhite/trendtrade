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

case class AppConfig(
    apiKey:           String,
    apiSecret:        String,
    notifyWebhook:    String,
    heartBeatWebhook: String
)

def start() = {
    val cfg          = loadConfig[AppConfig]("config.yaml")
    val notifyBot    = FeishuNotify(cfg.notifyWebhook)
    val heartBeatBot = FeishuNotify(cfg.heartBeatWebhook)
    val binanceApi   = new BinanceApi(
      cfg.apiKey,
      cfg.apiSecret,
      5,
      heartBeatBot
    ) {}
    val symbols      = binanceApi.allSymbol().filter(_.symbol.endsWith("BUSD")).map(_.symbol)
    val interval     = "1h"
    val strategies   = symbols.map(s => {
        val bot = MaBackStrategy(s, interval, binanceApi, notifyBot)
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
    val ks15   = data.getSymbolK("LINKBUSD", "1h")
    val bot    = MaBackTest()
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
