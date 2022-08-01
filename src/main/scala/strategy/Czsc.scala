package strategy

// 底： 前一K收盘价小于往前4k的收盘价, 且当前K大于往前4k的收盘价
// 顶： 前一K收盘价大于于往前4k的收盘价, 且当前K小于于往前4k的收盘价
// 段： 顶 + ks + 底, 可以要求顶的最低点 - 底的最高点 > n倍波动率

// 引力区域： 顶和底

// 买点1: 

// 走势类型
abstract class  TrendType {
    def tick(k: Kline): Unit 
}

// 初始状态
class InitType(k: Kline) extends TrendType {

  override def tick(k: Kline): Unit = ???

}

// 盘整
class Consolidate() extends TrendType {

  override def tick(k: Kline): Unit = ???

}
// 趋势
class Trend(direction: Int) extends TrendType {

  override def tick(k: Kline): Unit = ???

}




// 分型