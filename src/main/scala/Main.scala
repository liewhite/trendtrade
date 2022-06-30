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
    apiKey: String,
    apiSecret: String,
    feishu: String
)

@main def main: Unit = {
  val cfg = loadConfig[AppConfig]("config.yaml")
  val binanceApi = new BinanceApi(
    cfg.apiKey,
    cfg.apiSecret,
    5
  ) {}
  val fs = FeishuNotify(cfg.feishu)

  val strategy1 = MaBackStrategy("WAVESBUSD", "15m", binanceApi, fs)
  strategy1.start()
  val strategy2 = MaBackStrategy("BTCBUSD", "15m", binanceApi, fs)
  strategy2.start()
  val strategy3 = MaBackStrategy("FTMBUSD", "15m", binanceApi, fs)
  strategy3.start()
  val strategy4 = MaBackStrategy("APEBUSD", "15m", binanceApi, fs)
  strategy4.start()
  val strategy5 = MaBackStrategy("GMTBUSD", "15m", binanceApi, fs)
  strategy5.start()
  val strategy6 = MaBackStrategy("XRPBUSD", "15m", binanceApi, fs)
  strategy6.start()
}

def backtest() = {
  // val ks15 = getSymbol5minK("CF2209")
  // val ks15 = getSymbolK("BTCBUSD","1h")
  // val bot = MaBackStrategy()
  // ks15.foreach(k => {
  //   bot.step(k)
  // })
  // val profit = bot.closed.map(item => (item.closeAt.get - item.openAt) * item.direction - item.openAt * 0.00036 - item.closeAt.get * 0.00036).sum
  // val fee = bot.closed.map(item => item.openAt * 0.00036 + item.closeAt.get * 0.00036).sum

  // println(s"tx count: ${bot.closed.length} ,fee: ${fee} profit: ${profit} precent: ${(profit/bot.klines(0).close * 100).intValue}%"  )
}
