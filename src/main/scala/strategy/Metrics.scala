package strategy

import scala.collection.mutable
import cats.instances.int
import java.time.ZonedDateTime
import java.time.Instant
import java.time.ZoneId

trait IsEnd {
    def isEnd: Boolean
}

case class Kline(
    datetime: ZonedDateTime,
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
    openTime:     ZonedDateTime,
    direction:    Int,
    openAt:       BigDecimal,
    closeTime:    Option[ZonedDateTime] = None,
    closeAt:      Option[BigDecimal] = None,
    stopLoss:     Option[BigDecimal] = None,
    targetProfit: Option[BigDecimal] = None
)

abstract class KBasedMetric[T <: IsEnd] {
    val data = mutable.ListBuffer.empty[T]

    def next(k: Kline): Option[T]

    def current: T = data(0)

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
    def next(k: Kline): Option[Ma] = {
        val ks = klines.data.slice(0, interval)
        // println(ks.map(_.close).mkString(","))
        if (ks.length != 0) {
            val v = ks.map(_.close).sum / ks.length
            Some(Ma(v, k.isEnd))
        } else {
            None
        }
    }

    def currentValue = {
        data(0).value
    }

    def maDirection(offset: Int = 0) = {
        (data(offset).value - data(offset + 1).value).signum
    }

    def maTrend(span: Int = 4, offset: Int = 0): Int = {
        val sigs = data
            .slice(offset, offset + span + 1)
            .sliding(2)
            .map(item => {
                (item(0).value - item(1).value).signum
            })
        if (sigs.forall(_ == 1)) {
            1
        } else if (sigs.forall(_ == -1)) {
            -1
        } else {
            0
        }
    }
}

case class Ema(
    value: BigDecimal,
    ifEnd: Boolean
) extends IsEnd {

    def isEnd = ifEnd

    def next(k: Kline, interval: Int): Ema = {
        val e = value * (interval - 1) / (interval + 1) + k.close * 2 / (interval + 1)
        Ema(e, k.end)
    }
}

class EmaMetric(klines: KlineMetric, interval: Int) extends KBasedMetric[Ema] {
    def next(k: Kline): Option[Ema] = {
        val v = if (data.isEmpty) {
            Ema(k.close, k.end)
        } else {
            if (data.length == 1) {
                if (data(0).isEnd) {
                    data(0).next(k, interval)
                } else {
                    Ema(k.close, k.end)
                }
            } else {
                if (data(0).isEnd) {
                    data(0).next(k, interval)
                } else {
                    data(1).next(k, interval)
                }
            }
        }
        Some(v)
    }

    def emaDirection(offset: Int = 0) = {
        (data(offset).value - data(offset + 1).value).signum
    }

    def emaTrend(span: Int = 4, offset: Int = 0): Int = {
        val sigs = data
            .slice(offset, offset + span + 1)
            .sliding(2)
            .map(item => {
                (item(0).value - item(1).value).signum
            })
        if (sigs.forall(_ == 1)) {
            1
        } else if (sigs.forall(_ == -1)) {
            -1
        } else {
            0
        }
    }
}

case class Macd(
    datetime: ZonedDateTime,
    ema12:    BigDecimal,
    ema26:    BigDecimal,
    diff:     BigDecimal,
    dea:      BigDecimal,
    bar:      BigDecimal,
    end:      Boolean,
    cross:    Int = 0 // 1金叉， 2死叉
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
        val v     = if (data.isEmpty) {
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
        val cross = if (data.length >= 1) {
            if (data(0).bar > 0 && v.bar <= 0) {
                -1
            } else if (data(0).bar < 0 && v.bar >= 0) {
                1
            } else {
                0
            }
        } else {
            0
        }
        Some(v.copy(cross = cross))
    }

    def barDirection(offset: Int = 0): Int = {
        data(offset).bar.signum
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

    def macdBarTrend(offset: Int = 0, count: Int = 2): Int = {
        val ds = Range(0, count).map(i => {
            (data(offset + i).bar - data(offset + i + 1).bar).signum
        })
        if (ds.forall(_ == 1)) {
            1
        } else if (ds.forall(_ == -1)) {
            -1
        } else {
            0
        }
    }

    def macdDirection(offset: Int = 0): Int = {
        (data(offset).bar - data(offset + 1).bar).signum
    }

    def deaDirection(offset: Int = 0): Int = {
        (data(offset).dea - data(offset + 1).dea).signum
    }
}

case class Kdj(
    kline: Kline,
    rsv:   BigDecimal,
    k:     BigDecimal,
    d:     BigDecimal,
    j:     BigDecimal,
    end:   Boolean,
    cross: Int = 0 // 1金叉， 2死叉
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
            val v     = if (data.isEmpty) {
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
            val cross = if (data.length >= 1) {
                if (data(0).j > data(0).d && v.j <= v.d) {
                    -1
                } else if (data(0).j < data(0).d && v.j >= v.d) {
                    1
                } else {
                    0
                }
            } else {
                0
            }
            Some(v.copy(cross = cross))
        }
    }

    // kdj金叉死叉
    def kdjCrossDirection(offset: Int = 0, strict: Boolean = false): Int = {
        val a = data(offset)
        val b = data(offset + 1)
        if (b.j < b.d && a.j >= a.d && (!strict || b.j < 25)) {
            // if (b.j < b.d && a.j >= a.d) {
            1
        } else if (b.j > b.d && a.j <= a.d && (!strict || b.j > 75)) {
            // } else if (b.j > b.d && a.j <= a.d) {
            -1
        } else {
            0
        }
    }

    // kdj排列，金叉区间还是死叉区间
    def kdjRange(offset: Int = 0, strict: Boolean = true): Int = {
        val d = (data(offset).j - data(offset).d).signum
        if (d == 1 && data(offset).j < 60) {
            1
        } else if (d == -1 && data(offset).j > 60) {
            -1
        } else {
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

    def dDirection(offset: Int = 0): Int = {
        (data(offset).d - data(offset + 1).d).signum
    }
}

// 缠中说禅k线， 只有高低点， 没有影线、阴阳
case class CzscK(
    datetime: ZonedDateTime,
    low:      BigDecimal,
    high:     BigDecimal,
    close:    BigDecimal,
    end:      Boolean
) extends IsEnd {
    def isEnd: Boolean = isEnd
}

class CzscKMetric(klines: KlineMetric) extends KBasedMetric[CzscK] {

    override def next(k: Kline): Option[CzscK] = ???

    def isContains: Boolean = {
        val highDirection = (klines.data(0).high - data(0).high).signum
        val lowDirection  = (klines.data(0).low - data(0).low).signum

        // if( (highDirection >= 0 && lowDirection <= 0) || (highDirection <= 0 && lowDirection >= 0)) {
        highDirection * lowDirection <= 0
    }

    // 是否处于分型确认， 严格或非严格模式
    // 可能连续两个同向分型， 因为k线合并后再次触发分型
    def fenxing(strict: Boolean = true): Int = {
        if (data.length < 3) {
            0
        } else {
            val k0 = data(0)
            val k1 = data(1)
            val k2 = data(2)
            if (k1.high > Vector(k0.high, k2.high).max && k1.low > Vector(k0.low, k2.low).max) {
                // k1 低点高点都是最高， 顶分型
                if (strict && k0.low > (k2.high + k2.low) / 2) {
                    0
                } else {
                    -1
                }
            } else if (
              k1.high < Vector(k0.high, k2.high).min && k1.low < Vector(k0.low, k2.low).min
            ) {
                // k1 低点高点都是最低， 底分型
                if (strict && k0.high < (k2.high + k2.low) / 2) {
                    0
                } else {
                    1
                }
            } else {
                0
            }
        }
    }

    override def tick(k: Kline): Unit = {
        if (k.end) {
            if (data.isEmpty) {
                data.prepend(CzscK(k.datetime, k.low, k.high, k.close, true))
            } else {
                if (isContains) {
                    val direction = if (data.length == 1) {
                        if (klines.data(0).close > data(0).close) {
                            1
                        } else {
                            -1
                        }
                    } else {
                        if (data(0).high > data(1).high) {
                            1
                        } else {
                            -1
                        }
                    }

                    val newK = if (direction == 1) {
                        CzscK(
                          k.datetime,
                          Vector(klines.data(0).low, data(0).low).max,
                          Vector(klines.data(0).high, data(0).high).max,
                          klines.data(0).close,
                          true
                        )
                    } else {
                        CzscK(
                          k.datetime,
                          Vector(klines.data(0).low, data(0).low).min,
                          Vector(klines.data(0).high, data(0).high).min,
                          klines.data(0).close,
                          true
                        )
                    }
                    data.update(0, newK)
                } else {
                    data.prepend(CzscK(k.datetime, k.low, k.high, k.close, true))
                }
            }
        }
    }
}
