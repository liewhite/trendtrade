package strategy.czsc

import strategy.Kline
import scala.collection.mutable


class CzscKlines extends Czsc {
    val klines                        = mutable.ListBuffer.empty[Kline]
    override def tick(k: Kline): Unit = {}

    override def k(k: Kline): Unit = {
        klines.prepend(k)
        mergeK()
    }

    // 处理新k和最近k的合并关系
    def mergeK(): Unit = {
        // 至少2k才有可能存在包含关系
        if (klines.length > 1) {
            val k0         = klines(0)
            val k1         = klines(1)
            // 高低点方向不同， 则存在包含关系
            val ifContains = (k0.high - k1.high).signum != (k0.low - k1.low).signum

            // 两根K是包含关系
            if (ifContains) {
                // 判断当前是上升还是下降
                // 没有前面2k做参考， 则比较当前收盘价
                val direction = if (klines.length == 2) {
                   // 可能存在收盘价相等， 视为上升
                    (k0.close - k1.close).signum
                } else {
                  // 大于2根k， 说明前面处理过了， 不可能存在高点相等的相邻K
                    val k2 = klines(2)
                    // 比较最高点和最低点都一样
                    (k1.high - k2.high).signum
                }
                // 上升走势， 取高点的最高和低点的最高
                val (high, low) = if(direction == 1 || direction == 0) {
                  val high = Vector(k0.high, k1.high).max
                  val low = Vector(k0.low, k1.low).max
                  (high,low)
                }else {
                  val high = Vector(k0.high, k1.high).min
                  val low = Vector(k0.low, k1.low).min
                  (high,low)
                }
                klines(0) = klines(0).copy(high=high,low=low)
            } else {
              // 如果高低点完全一样, 只保留后一根
              klines.remove(1)
            }
        }
    }

}