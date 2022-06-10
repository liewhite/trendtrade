package bots

import scala.collection.mutable
import java.time.LocalDate
import java.time.LocalDateTime

case class HttpRealtimePrice(
    time: String,
    open: BigDecimal,
    high: BigDecimal,
    low: BigDecimal,
    current_price: BigDecimal,
)

case class RealtimePrice(
    time: String,
    open: BigDecimal,
    high: BigDecimal,
    low: BigDecimal,
    current_price: BigDecimal,
)

case class HttpKline(
    datetime: String,
    open: BigDecimal,
    high: BigDecimal,
    low: BigDecimal,
    close: BigDecimal,
    hold: BigDecimal
)
case class HttpDayKline(
    date: String,
    open: BigDecimal,
    high: BigDecimal,
    low: BigDecimal,
    close: BigDecimal,
    hold: BigDecimal,
)

case class Kline(
    datetime: LocalDateTime,
    open: BigDecimal,
    high: BigDecimal,
    low: BigDecimal,
    close: BigDecimal,
    hold: BigDecimal,
)

case class DayKline(
    date: LocalDate,
    open: BigDecimal,
    high: BigDecimal,
    low: BigDecimal,
    close: BigDecimal
)

// 1,0,-1, 分别对应上涨， 无法判断方向，下跌
def direction(klines: List[Kline]): Int = {
  if (klines.length < 5) {
    return 0
  }
  if (klines(0).close > klines(4).close) {
    return 1
  }
  if (klines(0).close < klines(4).close) {
    return -1
  }
  return 0
}

// 返回当前点位看多看空以及整个结构
def backtrace(klines15: List[Kline], dayDirection: Int): (List[Kline], Int) = {
  // K线长度不够， 返回中性
  if (klines15.length < 6) {
    return (List.empty, 0)
  }
  val dayTrend = dayDirection
  // 最后一根K趋势
  val trend1 = direction(klines15)
  // 倒数第二根K趋势
  val trend2 = direction(klines15.tail)

  // 存在一个无势则返回中性
  if (trend1 == 0 || trend2 == 0 || dayTrend == 0) {
    return (List.empty, 0)
  }

  // 没有转折， 中性
  if (trend1 == trend2) {
    return (List.empty, 0)
  }

  // 小势逆大势， 中性
  if (trend1 != dayTrend) {
    return (List.empty, 0)
  }

  // 小势顺大势，继续往前回溯结构
  // 往前应有一个逆势结构
  val retraceTrend = -trend1
  // 回调段
  val klinesForRetrace = klines15.tail
  val retraceResult = backtraceTrend(klinesForRetrace, retraceTrend)
  // 应该不可能为0， 前面已经检查过最后两根K方向不一致了。
  if (retraceResult.length == 0) {
    throw Exception("no retrace found: " + klinesForRetrace)
  }
  // if (retraceResult.length < 3) {
  //   return (List.empty, 0)
  // }
  // 保证还有足够的趋势发动阶段, 回调段+反转K + 初始段（3根上涨， 至少需要8根K线）
  if (klines15.length < retraceResult.length + 1 + 8) {
    return (List.empty, 0)
  }

  // 第一段趋势,与trend1一致
  val klinesForInitTrend = klinesForRetrace.drop(retraceResult.length)
  val initTrendResult = backtraceTrend(klinesForInitTrend, trend1)
  // 不满3根的忽略
  if (initTrendResult.length < 3) {
    return (List.empty, 0)
  }
  // 第一波涨幅足够
  if((initTrendResult.head.close - initTrendResult.last.close).abs < initTrendResult.last.close * 0.02) {
    // println("第一波力度不够， 忽略" + initTrendResult.map(_.close).max + " . " + initTrendResult.map(_.close).min)
    return (List.empty, 0)
  }
  // 检查回调极值点是否有超过趋势起点
  if (trend1 == 1) {
    // 回调过深， 不成立
    if (initTrendResult.map(_.low).min > retraceResult(0).close) {
      return (List.empty, 0)
    }
    return ((retraceResult ++ initTrendResult).prepended(klines15.head), 1)
  } else {
    if (initTrendResult.map(_.high).max < retraceResult(0).close) {
      return (List.empty, 0)
    }
    return ((retraceResult ++ initTrendResult).prepended(klines15.head), -1)
  }
}

// 给定K线和方向， 往前回溯， 直到反转， 返回趋势K线段
def backtraceTrend(klines: List[Kline], d: Int): List[Kline] = {
  def trace(klines: List[Kline], klinesInTrend: List[Kline]): List[Kline] = {
    if (klines.length < 5) {
      return klinesInTrend
    }
    if (d == direction(klines)) {
      return trace(klines.drop(1), klinesInTrend.prepended(klines.head))
    } else {
      return klinesInTrend
    }
  }
  trace(klines, List.empty)
}
