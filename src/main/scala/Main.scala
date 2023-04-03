import scala.collection.mutable
import sttp.client3.okhttp.quick._
import strategy.*
import binance.*
import java.time.Duration
import notifier.FeishuNotify
import com.typesafe.scalalogging.Logger
import common.loadConfig
import zio.ZIO
import zio.Config
import zio.ZIOAppDefault
import zio.Scope
import zio.ZIOAppArgs

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
    // blackList:        Vector[String]
)

val logger = Logger("main")

def loadCfg(): ZIO[Any, Config.Error, AppConfig] = {
    logger.info("start binance bot...")
    logger.info("load config")
    val cfg = loadConfig[AppConfig]("config.yaml")
    // val cfg: AppConfig = null
    logger.info("create notify bot")
    cfg
    // ???
}

def start(cfg: AppConfig)              = {

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

    val interval = cfg.interval

    // logger.info(s"blackList: ${cfg.blackList}")

    val allSymbols = binanceApi
        .allSymbol()
        .filter(_.symbol.endsWith(cfg.quoteSymbol))
        // .filter(item => !cfg.blackList.contains(item.symbol))

    val symbolsAndOpened = allSymbols.map(item => {
        val value = binanceApi.getOpenInterest(item.symbol)
        (item, value)
    })

    val validSymbols         =
        symbolsAndOpened.sortBy(_._2).reverse.take(cfg.maxHolds).map(_._1.symbol).toSet
    val currentHoldingSymbol =
        binanceApi.getPositions().filter(_.positionAmt != 0).map(_.symbol).toSet
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

object Main extends ZIOAppDefault {
    def run: ZIO[Any & (ZIOAppArgs & Scope), Any, Any] = {
        for {
            cfg <- loadCfg()
        } yield {
            start(cfg)
        }
    }
}
