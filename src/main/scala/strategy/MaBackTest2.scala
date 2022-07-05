package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
// 代码设计

// 做趋势回调
// 处于均线劣势方，出现顺势K线开仓
// 止损为K线最低点
// 突破均线前均线拐头止损
// 突破均线后跌破均线止损
class MaBackTest2(maInterval: Int = 20)
    extends BaseStrategy
    with KlineMixin
    with MacdMixin()
    with MaMixin(Vector(maInterval)) {
    var position: Option[Position] = None
    val closed                     = mutable.ListBuffer.empty[Position]

    def closeCurrent()  = {
        val p = position.get
        closed.prepend(
          p.copy(
            closeTime = Some(klines(0).datetime),
            closeAt = Some(klines(0).close)
          )
        )
        position = None
        // println(s"close: ${klines(0).datetime} ${(klines(0).close - p.openAt) * p.direction}")
    }
    // 止盈止损
    // 跌破均线时有盈利， 则止盈了结
    // 如果跌破时无盈利或者盈利较少， 可能是震荡行情， 适当放宽止损
    def checkAndClose() = {
        position match {
            case Some(p) => {
                val k = klines(0)
                // 出现反向开仓条件要止损
                if (direction == -p.direction) {
                    closeCurrent()
                    open(direction,0)
                } else {
                    val prek     = klines(1)
                    val ma       = mas(maInterval)(0)
                    val preMa    = mas(maInterval)(1)
                    val avgKSize = avgSize()
                    // 收于均线劣势侧
                    if ((k.close - ma) * p.direction < 0) {
                        // 收亏损K
                        if ((k.close - k.open) * p.direction < 0) {
                            // 盈利或亏损较少
                            val slSize =
                                if (
                                  (k.close - p.openAt) * p.direction < avgKSize * 2 && (k.close - p.openAt) * p.direction > -2 * avgKSize
                                ) {
                                    avgKSize * 2.5
                                } else {
                                    avgKSize * 0.8
                                }
                            // 跌破均线大于 slSize止损
                            if ((k.close - ma) * p.direction < -slSize) {
                                closeCurrent()
                            }
                        }
                    }
                }
            }
            case None    =>
        }
    }

    def open(direction: Int, stopLoss: BigDecimal) = {
        val k = klines(0)
        position = Some(
          Position(1, k.datetime, direction, k.close, None, None, Some(stopLoss), None)
        )
        // println(s"open : ${k.datetime}")
    }
    def avgSize(): BigDecimal                      = {
        val k          = klines(0)
        val entitySize = (k.close - k.open).abs
        val entities   = klines
            .slice(1, 11)
            .map(item => {
                if (item.close == item.open) {
                    BigDecimal(0)
                } else {
                    (item.close - item.open).abs
                }
            })

        val avgEntitySize = entities.sum / entities.length
        avgEntitySize
    }

    def direction = {
        val ma          = mas(maInterval)
        val maDirection = (ma(0) - ma(1)).signum
        val k           = klines(0)

        if (
          ((k.close - ma(0)) * maDirection < 0 || (k.close - ma(
            0
          )).abs < avgSize() * 0.5) && (k.close - k.open) * maDirection > 0
        ) {
            maDirection
        } else {
            0
        }
    }

    override def tick(k: Kline, history: Boolean = false): Unit = {
        super.tick(k)
        // 忽略历史数据， 只处理实时数据
        if (history) {
            return
        }
        // 历史数据不足， 无法参考
        if (klines.length < maInterval) {
            return
        }
        checkAndClose()
        if (position.nonEmpty) {
            return
        }
        // 无波动，不操作
        if (klines(0).close == klines(1).close) {
            return
        }

        val ma = mas(maInterval)
        if (ma(0) == ma(1)) {
            return
        }

        val maDirection = (ma(0) - ma(1)).signum

        // 开盘价在均线劣势方,且涨跌与均线一致
        if (direction == maDirection && maDirection != 0) {
            open(maDirection, 0)
        }
    }

}
