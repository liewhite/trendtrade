package strategy.czsc

import strategy.Kline

// 所有缠中说禅构造的共同特征： 接收tick和完成k线
// 默认传入1m线， 自动构造出5m和30m
trait Czsc {

    // 每个tick调用一次, 用于打提前量的需求
    def tick(k: Kline): Unit

    // 每个k结束时调用一次, 用于处理稳定结构
    def k(k: Kline): Unit

}
