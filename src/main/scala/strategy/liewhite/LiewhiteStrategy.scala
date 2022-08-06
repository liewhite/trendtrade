package strategy.liewhite

import strategy.Kline
import strategy.KlineMetric


// 接收tick， 负责生成k线， 同步各周期
// 以及发现买卖点进行操作
class LiewhiteStrategy {
    val klines5 = KlineMetric()
    val klines30 = KlineMetric()

    def tick(k: Kline): Unit = {
        

    }
}
