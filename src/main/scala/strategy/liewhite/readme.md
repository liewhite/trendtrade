改进自缠论的走势分解与买卖策略

> 一下讨论只讨论上涨， 下跌为镜像问题，自行脑补
# 定义
## 基准K线
当前K线往前数4根k即为当前的基准K线

# 分解
某一级别的所有走势都可以分解成 "段"
> 一个段只有等结束以后才能确认, 所以如果基于段的假设开仓， 当段结束， 发现不成立， 需要平仓
> 新段如不成立， 则并入上一段， 方向与上一段保持一致

## 上涨段
### 起点
* 上一k收盘低于上一基准k收盘价
* 当前k收盘价高于基准k收盘价

### 终点
与起点定义相反

### 成立条件
终点一定要高于起点， 否则无从谈起上升
如果段不成立， 则将期间所有波动纳入前一段，以最新价作为收盘价
### 合并
两个连续的上涨段直接合并

## 趋势
上涨段下降段交替出现,  上涨段顶部越来越高, (且下降段底部越来越高？ 可以不要求)

### 趋势破坏
后一个上涨段的高点低于前一个, 则上涨破坏， 且开启一个下降趋势, 趋势起点为最高点后的下降段起点
## 趋势终点
最后一个不破前高的上涨段被破坏的时候认为是趋势终点

> 因为趋势的最后两段满足下降趋势的起点， 所以上涨下跌趋势共同拥有一个下降段和一个上升段, 上升趋势需要确认破坏， 下降趋势去要确认开启，共同需要这两段

# 买卖点
## 一类买卖点
> 即趋势反转处的买卖点， 成本优势极大， 但是胜率较低

趋势被破坏前可能会出现一类买卖点, 也可能不出现， 比如急跌后迅速v型反转, 没有出现背驰，就不会出现一类买点

### 操作点位
上涨趋势中最近连续3段， 上-下-上， 后一段斜率小于第一段, 视为背离, 择机做空

> 小级别出现二类买卖点， 大级别就可以抓一下一类买卖点

## 二类买卖点
上涨趋势背离后， 第二下降段开始前，在之前的上升段出现次级别背离，为第二类卖点。

## 三类买卖点
第三类卖点无需本级别背离
在下跌趋势确认后（两个下降段，一个上升段）， 第二个上升段出现次级别背离， 为第三类卖点

