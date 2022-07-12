package strategy

import java.time.LocalDateTime
import scala.collection.mutable
import cats.instances.int

trait IsEnd {
    def isEnd: Boolean
}

case class Kline(
    datetime: LocalDateTime,
    open:     BigDecimal,
    low:      BigDecimal,
    high:     BigDecimal,
    close:    BigDecimal,
    vol:      BigDecimal,
    end:      Boolean = true
) extends IsEnd {
    def isEnd: Boolean = end
    def direction: Int = (close - open).signum
}

case class Position(
    quantity:     BigDecimal,
    openTime:     LocalDateTime,
    direction:    Int,
    openAt:       BigDecimal,
    closeTime:    Option[LocalDateTime] = None,
    closeAt:      Option[BigDecimal] = None,
    stopLoss:     Option[BigDecimal] = None,
    targetProfit: Option[BigDecimal] = None
)

abstract class KBasedMetric[T <: IsEnd] {
    val data = mutable.ListBuffer.empty[T]

    def next(k: Kline): Option[T]

    def maxLength = 1000

    def tick(k: Kline): Unit = {
        val n = next(k)
        n match {
            case None    =>
            case Some(v) => {
                if (data.isEmpty) {
                    data.prepend(v)
                } else {
                    if (data(0).isEnd) {
                        data.prepend(v)
                    } else {
                        data(0) = v
                    }
                }
                if (data.length > maxLength) {
                    data.dropRightInPlace(data.length - maxLength)
                }
            }
        }
    }
}

class KlineMetric extends KBasedMetric[Kline] {
    def next(k: Kline): Option[Kline] = Some(k)
}

case class Ma(
    value: BigDecimal,
    ifEnd: Boolean
) extends IsEnd {
    def isEnd = ifEnd
}

class MaMetric(klines: KlineMetric, interval: Int) extends KBasedMetric[Ma] {
    def next(k: Kline): Option[Ma]           = {
        val ks = klines.data.slice(0, interval)
        // println(ks.map(_.close).mkString(","))
        if (ks.length != 0) {
            val v = ks.map(_.close).sum / ks.length
            Some(Ma(v, k.isEnd))
        } else {
            None
        }
    }
    def maDirection                          = {
        (data(0).value - data(1).value).signum
    }
    def historyMaDirection(offset: Int): Int = {
        (data(offset).value - data(offset + 1).value).signum
    }
}

case class Macd(
    datetime: LocalDateTime,
    ema12:    BigDecimal,
    ema26:    BigDecimal,
    diff:     BigDecimal,
    dea:      BigDecimal,
    bar:      BigDecimal,
    end:      Boolean
) extends IsEnd {
    def next(k: Kline, price: BigDecimal, short: Int = 12, long: Int = 26, mid: Int = 9): Macd = {
        val e12    = ema12 * (short - 1) / (short + 1) + price * 2 / (short + 1)
        val e26    = ema26 * (long - 1) / (long + 1) + price * 2 / (long + 1)
        val newDif = e12 - e26
        val newDea = this.dea * (mid - 1) / (mid + 1) + newDif * 2 / (mid + 1)
        val b      = (newDif - newDea)
        Macd(k.datetime, e12, e26, newDif, newDea, b, k.end)
    }

    def isEnd: Boolean = end
}

class MacdMetric(klines: KlineMetric, fast: Int = 12, slow: Int = 26, mid: Int = 9)
    extends KBasedMetric[Macd] {

    def next(k: Kline): Option[Macd] = {
        // 如果只有一根K线，则直接生成MACD
        // 否则根据最新K线和上一根确定的macd 生成新的macd
        val v = if (data.isEmpty) {
            Macd(k.datetime, k.close, k.close, 0, 0, 0, k.end)
        } else {
            if (data.length == 1) {
                if (data(0).isEnd) {
                    data(0).next(k, k.close, fast, slow, mid)
                } else {
                    Macd(k.datetime, k.close, k.close, 0, 0, 0, k.end)
                }
            } else {
                if (data(0).isEnd) {
                    data(0).next(k, k.close, fast, slow, mid)
                } else {
                    data(1).next(k, k.close, fast, slow, mid)
                }
            }
        }
        Some(v)
    }

    def macdDirection: Int              = {
        if ((data(0).bar - data(1).bar).signum == 1 && data(1).bar < 0) {
            1
        } else if ((data(0).bar - data(1).bar).signum == -1 && data(1).bar > 0) {
            -1
        } else {
            0
        }
    }

    def macdCross(offset: Int = 0): Int = {
        if (data(offset).bar > 0 && data(offset + 1).bar < 0) {
            1
        } else if (data(offset).bar < 0 && data(offset + 1).bar > 0) {
            -1
        } else {
            0
        }
    }

    def macdHistoryDirection(offset: Int = 0): Int = {
        if ((data(offset).bar - data(offset + 1).bar).signum == 1 && data(offset + 1).bar < 0) {
            1
        } else if (
          (data(offset).bar - data(offset + 1).bar).signum == -1 && data(offset + 1).bar > 0
        ) {
            -1
        } else {
            0
        }
    }

    def deaDirection: Int = {
        (data(0).dea - data(1).dea).signum
    }
}

case class Kdj(
    kline: Kline,
    rsv:   BigDecimal,
    k:     BigDecimal,
    d:     BigDecimal,
    j:     BigDecimal,
    end:   Boolean
) extends IsEnd {
    def isEnd: Boolean = end
}

class KdjMetric(klines: KlineMetric, arg1: Int = 9, arg2: Int = 3, arg3: Int = 3)
    extends KBasedMetric[Kdj] {

    def genNext(preKdj: Kdj, ks: Option[Seq[Kline]] = None) = {
        val kls   = ks match {
            case Some(o) => o
            case None    => klines.data.slice(0, 9)
        }
        val headK = ks match {
            case Some(o) => o.head
            case None    => klines.data(0)
        }

        val low9  = kls.map(_.low).min
        val high9 = kls.map(_.high).max
        val rsv   = (headK.close - low9) / (high9 - low9) * 100
        val newK  = preKdj.k * 2 / 3 + rsv / 3
        val newD  = preKdj.d * 2 / 3 + newK / 3
        val newJ  = 3 * newK - 2 * newD
        val k     = headK
        Kdj(k, rsv, newK, newD, newJ, k.end)
    }

    def first() = {
        val k     = klines.data(0)
        val low9  = klines.data.slice(0, 9).map(_.low).min
        val high9 = klines.data.slice(0, 9).map(_.high).max
        val rsv   = (klines.data(0).close - low9) / (high9 - low9) * 100
        val newK  = 50 * 2 / 3 + rsv / 3
        val newD  = 50 * 2 / 3 + newK / 3
        val newJ  = 3 * newK - 2 * newD
        Kdj(k, rsv, newK, newD, newJ, k.end)
    }

    def next(k: Kline): Option[Kdj] = {
        if (klines.data.length < 10) {
            None
        } else {
            val v = if (data.isEmpty) {
                first()
            } else {
                if (data.length == 1) {
                    if (data(0).isEnd) {
                        genNext(data(0))
                    } else {
                        first()
                    }
                } else {
                    if (data(0).isEnd) {
                        genNext(data(0))
                    } else {
                        genNext(data(1))
                    }
                }
            }
            Some(v)
        }
    }

    // kdj金叉死叉
    def kdjCrossDirection(offset: Int = 0): Int = {
        val a = data(offset)
        val b = data(offset + 1)
        if(b.j < b.d && a.j >= a.d && b.j < 20) {
            1
        }else if(b.j > b.d && a.j <= a.d && b.j > 80) {
            -1
        }else{
            0
        }
    }

    // kdj 收敛至少两个周期
    def kdjDirection: Int = {
        val a = data(0)
        val b = data(1)
        if (b.j < b.d && b.k < b.d && a.j >= a.d && a.k >= a.d) {
            1
        } else if (b.j > b.d && b.k > b.d && a.j <= a.d && a.k <= a.d) {
            -1
        } else {
            0
        }
    }

    def dDirection: Int = {
        (data(0).d - data(1).d).signum
    }
}
