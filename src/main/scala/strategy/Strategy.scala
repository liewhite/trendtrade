package strategy

import java.time.LocalDateTime
import scala.collection.mutable

case class Kline(
    datetime: LocalDateTime,
    open:     BigDecimal,
    low:      BigDecimal,
    high:     BigDecimal,
    close:    BigDecimal,
    vol:      BigDecimal
)

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

abstract class AStrategy {
    def queueList[T](l: mutable.ListBuffer[T], length: Int) = {
        if (l.length > length) {
            l.dropRightInPlace(l.length - length)
        }
    }

    def step(k: Kline, history: Boolean = false): Unit
}

class BaseStrategy extends AStrategy {
    override def step(k: Kline, history: Boolean = false): Unit = {}
}

trait KlineMixin extends AStrategy {
    val klines: mutable.ListBuffer[Kline] = mutable.ListBuffer.empty

    abstract override def step(k: Kline, history: Boolean = false): Unit = {
        super.step(k)
        klines.prepend(k)
        queueList(klines, 1000)
    }
}

trait VolMaMixin(intervals: Vector[Int]) extends AStrategy {
    KL: KlineMixin =>
    val volMas: mutable.Map[Int, mutable.ListBuffer[BigDecimal]] = mutable.Map.from(
      intervals
          .map(i => {
              (i, mutable.ListBuffer.empty[BigDecimal])
          })
    )

    abstract override def step(k: Kline, history: Boolean = false): Unit = {
        super.step(k)
        intervals.foreach(interval => {
            val ks = klines.slice(0, interval)
            if (ks.length != 0) {
                val avg = ks.map(_.vol).sum / ks.length
                volMas(interval).prepend(avg)
            }
        })
        volMas.foreach(item => {
            queueList(item._2, 1000)
        })
    }

    def volMaDirection(interval: Int): Int = {
        if (volMas(interval).length < 2) {
            0
        } else {
            if (volMas(interval)(0) > volMas(interval)(1)) {
                1
            } else if (volMas(interval)(0) < volMas(interval)(1)) {
                -1
            } else {
                0
            }
        }
    }
}

trait MaMixin(intervals: Vector[Int] = Vector(5, 10, 20)) extends AStrategy {
    KL: KlineMixin =>
    val mas: mutable.Map[Int, mutable.ListBuffer[BigDecimal]] = mutable.Map.from(
      intervals
          .map(i => {
              (i, mutable.ListBuffer.empty[BigDecimal])
          })
    )

    abstract override def step(k: Kline, history: Boolean = false): Unit = {
        super.step(k)
        intervals.foreach(interval => {
            val ks = klines.slice(0, interval)
            if (ks.length != 0) {
                val avg = ks.map(_.close).sum / ks.length
                mas(interval).prepend(avg)
            }
            queueList(mas(interval), 1000)
        })
    }
}

case class Macd(
    datetime: LocalDateTime,
    ema12:    BigDecimal,
    ema26:    BigDecimal,
    diff:     BigDecimal,
    dea:      BigDecimal,
    bar:      BigDecimal
) {
    def next(k: Kline, price: BigDecimal, short: Int, long: Int, mid: Int): Macd = {
        val e12    = ema12 * (short - 1) / (short + 1) + price * 2 / (short + 1)
        val e26    = ema26 * (long - 1) / (long + 1) + price * 2 / (long + 1)
        val newDif = e12 - e26
        val newDea = this.dea * (mid - 1) / (mid + 1) + newDif * 2 / (mid + 1)
        val b      = (newDif - newDea)
        Macd(k.datetime, e12, e26, newDif, newDea, b)
    }
}

trait MacdMixin(fast: Int = 12, slow: Int = 26, mid: Int = 9) extends AStrategy {
    KL: KlineMixin =>
    val macd: mutable.ListBuffer[Macd] = mutable.ListBuffer.empty

    abstract override def step(k: Kline, history: Boolean = false): Unit = {
        super.step(k)
        if (klines.length == 1) {
            macd.prepend(Macd(k.datetime, k.close, k.close, 0, 0, 0))
        } else {
            macd.prepend(macd(0).next(k, k.close, fast, slow, mid))
        }
        queueList(macd, 1000)
    }
}

case class Kdj(
    kline: Kline,
    rsv:   BigDecimal,
    k:     BigDecimal,
    d:     BigDecimal,
    j:     BigDecimal
)

trait KdjMixin(arg1: Int = 9, arg2: Int = 3, arg3: Int = 3) extends AStrategy {
    KL: KlineMixin =>
    val kdj: mutable.ListBuffer[Kdj] = mutable.ListBuffer.empty

    abstract override def step(k: Kline, history: Boolean = false): Unit = {
        super.step(k)
        if (klines.length < 10) {
            return
        }
        val low9  = klines.slice(0, 9).map(_.low).min
        val high9 = klines.slice(0, 9).map(_.high).max
        val rsv   = (klines(0).close - low9) / (high9 - low9) * 100

        val newKdj = if (kdj.isEmpty) {
            val newK = 50 * 2 / 3 + rsv / 3
            val newD = 50 * 2 / 3 + newK / 3
            val newJ = 3 * newK - 2 * newD
            Kdj(k, rsv, newK, newD, newJ)
        } else {
            val preKdj = kdj(0)
            val newK   = preKdj.k * 2 / 3 + rsv / 3
            val newD   = preKdj.d * 2 / 3 + newK / 3
            val newJ   = 3 * newK - 2 * newD
            Kdj(k, rsv, newK, newD, newJ)
        }
        kdj.prepend(newKdj)
        queueList(kdj, 1000)
    }
}
