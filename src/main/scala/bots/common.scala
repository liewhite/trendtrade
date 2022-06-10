package bots

import java.time.LocalDateTime

case class Hold(
    datetime: LocalDateTime,
    trend: Int, // 1，做多 -1 做空
    tradePrice: BigDecimal,
    stopLossPrice: BigDecimal,
    kline: Kline,
    mustNotLose: Boolean = false,
    maxProfit: BigDecimal = 0,
    holdDays: Int = 0
)

case class Closed(
    hold: Hold,
    closedAt: BigDecimal,
    profit: BigDecimal
)
