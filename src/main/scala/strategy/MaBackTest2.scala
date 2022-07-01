package strategy

import scala.math
import scala.collection.mutable
import java.time.LocalDateTime
// 代码设计

// ma20
// 做趋势回调
// 处于均线劣势方，出现顺势K线开仓
// 止损为K线最低点
// 突破均线前均线拐头止损
// 突破均线后跌破均线止损
class MaBackTest2() extends BaseStrategy with KlineMixin with MacdMixin() with MaMixin(Vector(20)) {
    var position: Option[Position] = None
    val closed                     = mutable.ListBuffer.empty[Position]

    // 止盈止损
    def checkAndClose() = {
        position match {
            case Some(p) => {
                val k     = klines(0)
                val ma    = mas(20)(0)
                val preMa = mas(20)(1)
                // 如果还没突破均线, 则均线方向逆势且亏损状态止损
                if ( (k.open - ma) * p.direction < 0 && (k.close - ma) * p.direction < 0) {
                    if ((ma - preMa).signum != p.direction && (k.close - p.openAt) * p.direction < 0) {
                        closed.prepend(
                          p.copy(
                            closeTime = Some(klines(0).datetime),
                            closeAt = Some(klines(0).close)
                          )
                        )
                        position = None
                        println(
                          s"close:${p} profit: ${(k.close - p.openAt) * p.direction} ${p.direction} ${k.datetime}, price ${k.close}"
                        )
                    }
                }else {
                    // 如果已突破均线，则跌破均线平仓
                    if( (k.close - ma) * p.direction <0 ) {
                        closed.prepend(
                          p.copy(
                            closeTime = Some(klines(0).datetime),
                            closeAt = Some(klines(0).close)
                          )
                        )
                        position = None
                        println(
                          s"close:${p} profit: ${(k.close - p.openAt) * p.direction} ${p.direction} ${k.datetime}, price ${k.close}"
                        )
                    }
                }
            }
            case None    => 
        }
    }

    def open(direction: Int, stopLoss: BigDecimal) = {
        val k = klines(0)
        position =Some(
          Position(1, k.datetime, direction, k.close, None, None, Some(stopLoss), None)
        )
        println(s"open : ${position}")
    }

    override def step(k: Kline, history: Boolean = false): Unit = {
        super.step(k)
        // 忽略历史数据， 只处理实时数据
        if (history) {
            return
        }
        // 历史数据不足， 无法参考
        if (klines.length < 60) {
            return
        }
        checkAndClose()
        if(position.nonEmpty) {
            return
        }

        val entitySize = (k.close - k.open).abs
        val entities   = klines
            .slice(1, 20)
            .map(item => {
                if (item.close == item.open) {
                    BigDecimal(0)
                } else {
                    (item.close - item.open).abs
                }
            })

        val avgEntitySize = entities.sum / entities.length
        // k线实体大于过去一段时间的平均波动幅度， 视为趋势开始
        if (entitySize <= avgEntitySize) {
            return
        }

        val ma = mas(20)
        // 无波动，不操作
        if (klines(0).close == klines(1).close) {
            return
        }
        if (ma(0) == ma(1)) {
            return
        }

        val maDirection = (ma(0) - ma(1)).signum

        // 开盘价在均线劣势方,且涨跌与均线一致
        if (
          (k.open - ma(0)) * maDirection < 0&& (k.close - k.open) * maDirection > 0
        ) {
            val sl = if (maDirection == 1) k.low else k.high
            open(maDirection, stopLoss = sl)
        }

    }

}
