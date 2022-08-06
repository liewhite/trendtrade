package strategy.liewhite

// 段的定义, 方向, 低点， 高点， 周期数
case class Segment(
    direction: Int,        // 方向
    open:      BigDecimal, // 第一根K的开盘价
    close:     BigDecimal, // 最后一根K的收盘价, 如果还没结束， 就是最新K的收盘价
    period:    Int,
    isEnd:     Boolean
)

case class Trend(
    direction:       Int,
    trendSegments:   Vector[Segment], // 趋势内包含的段
    confirmSegments: Vector[Segment], // 确认趋势结束的段
    isEnd:           Boolean          // 趋势是否结束
)
