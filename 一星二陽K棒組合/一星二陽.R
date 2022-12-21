library(quantmod)
library(dplyr)
strategy_test <- function(company,sample){
  #抓取股價
  Stock_price <- get(getSymbols(Symbols = company , src = "yahoo"))
  head(Stock_price,10)
  #整理資料
  #資料轉乘tibble型式
  Stock_price1 <- as_data_frame(Stock_price)
  # 重新命名欄位
  Stock_price1[,6] <- NULL
  colnames(Stock_price1) <- c("Open", "High", "Low", "Close", "Volume") 
  # 將日期成為新的一個欄位
  Stock_price1$Date <- as.Date(index(`Stock_price`)) 
  Stock_price1 <- Stock_price1[,c(6,1:5)]
  Stock_price1 %>%
    select(c(Date,Open:Volume)) %>%
    filter((Volume>0)|(!is.na(Volume))) -> Stock_price2
  #整理判斷條件資料
  Stock_price2  %>%
    mutate(
      # 第t-1天收盤價
      lagClose1=lag(Close,1), 
      # 第t-2天收盤價
      lagClose2=lag(Close,2),
      # 第t-1天開盤價
      lagOpen1=lag(Open,1),
      # 第t-2天開盤價
      lagOpen2=lag(Open,2),
      # 第t天的實體K棒長度
      kbarValue=abs(Close/Open-1), 
      # 第t-1天的實體K棒長度
      lagKbarValue1=lag(kbarValue,1),
      # 第t-2天的實體K棒長度
      lagKbarValue2=lag(kbarValue,2)) -> Stock_price3
  #選擇進場時機點
  Stock_price3 %>%
    filter(
      # 第t天的收盤價 > 第t天的開盤價
      Close>Open,
      # 第t-2天的收盤價 > 第t-2天的開盤價
      lagClose2>lagOpen2, 
      # 第t天的開盤價 > 第t-1天的收盤價*(1-0.01)
      Open>lagClose1*(1-0.01),
      # 第t天的開盤價 < 第t-1天的收盤價*(1+0.01)
      Open<lagClose1*(1+0.01),
      # 第t-2天的收盤價 > 第t-1天的開盤價*(1-0.01)
      lagClose2>lagOpen1*(1-0.01),
      # 第t-2天的收盤價 < 第t-1天的開盤價*(1+0.01)
      lagClose2<lagClose1*(1+0.01),
      # 第t天的實體K棒長度為1%以上
      kbarValue>0.01,
      # 第t-1天的實體K棒長度為0.5%以下
      lagKbarValue1<0.005,
      # 第t-2天的實體K棒長度為1%以上
      lagKbarValue2>0.01) %>%
    # 紀錄一星二陽K棒組合發生日及當日收盤價，並重新命名欄位
    select(inDate=Date, buyPrice=Close) -> purchase
  purchase
  #選擇出場時機點(收盤價摜破10日均線)
  Stock_price3 %>%
    mutate(
      # 計算第t日的5日移動平均線
      MA5=SMA(Close, 5),
      # 計算第t日的60日移動平均線
      MA60=SMA(Close, 60),
      # 計算第t-1日的5日移動平均線
      lagMA5=lag(MA5,1),
      # 計算第t-1日的60日移動平均線
      lagMA60=lag(MA60,1)) %>%
    filter(
      # t日的收盤價<t日的5日移動平均
      Close<MA5,
      # t-1日的收盤價>t-1日的5日移動平均線
      lagClose1>MA5) %>%
    # 紀錄收盤價跌破5日移動平均線的發生日及當日收盤價，並重新命名欄位
    select(outDate=Date, sellPrice=Close) -> sell
  sell
  #建立交易表
  trade <- NULL   
  for(ix in 1:nrow(purchase)){
    # 目前的進場日期
    inDate <- purchase$inDate[ix] 
    # 找尋進場日期往後最近的出場位置
    outDate <- which(sell$outDate>inDate)[1]  
    # 防呆機制，如果進場日期在資料尾端，有可能發生資料不足找不到出場位置的狀況
    if(length(outDate)>0){                            
      # 將該筆進場資訊與對應的出場資訊合併，並儲存至交易表內
      trade <- bind_rows(trade, bind_cols(purchase[ix,], sell[outDate,]))
    }
  }
  trade
  #計算報酬率跟持有時間
  purchaseCost <- 0.0015   # 買入交易成本
  sellCost <- 0.003  # 賣出交易成本
  trade %>%
    mutate(
      # 計算報酬率
      returnR=sellPrice*(1-sellCost)/(buyPrice*(1+purchaseCost))-1,
      # 計算持有日數
      holdDays=as.numeric(outDate-inDate)) -> trade
  trade
  #績效指標
  #平均報酬率
  meanR <- mean(trade$returnR)
  #報酬率標準差
  sdR <- sd(trade$returnR) 
  #交易次數
  tradeNums <- nrow(trade) 
  #勝率
  winRatio <- sum(as.numeric(trade$returnR>0))/tradeNums
  #最大報酬率
  maxR <- max(trade$returnR)
  #最小報酬率
  minR <- min(trade$returnR) 
  #平均持有日數
  avgHoldDays <- mean(trade$holdDays)  
  #績效結果
  cat(paste0("*********策略回測績效*********\n",
             "平均報酬率: ",round(meanR*100,2)," %\n",
             "最大報酬率: ",round(maxR*100,2)," %\n",
             "最小報酬率: ",round(minR*100,2)," %\n",
             "報酬率標準差: ",round(sdR*100,2)," %\n",
             "交易次數: ",tradeNums," 次\n",
             "勝率: ",round(winRatio*100,2)," %\n",
             "平均持有日數: ",round(avgHoldDays,2),"天"))
  #繪圖
Stock_price3 %>%
  mutate(
    MA5=SMA(Close,5),     # 5日移動平均線
    MA20=SMA(Close,20),   # 20日移動平均線
    MA60=SMA(Close,60)) -> Stock_price4 # 60日移動平均線  
  #進出場日期
  inDate <- trade$inDate[sample]
  outDate <- trade$outDate[sample]
  #起始日
  start <- which(Stock_price4$Date==inDate)-30
  plotStartDate <- Stock_price4$Date[ifelse(start<1, 1, start)]                           
  #結束日
  end <- which(Stock_price4$Date==outDate)+30
  plotEndDate <- Stock_price4$Date[ifelse(end>nrow(Stock_price4), nrow(Stock_price4), end)]
  #整理股價資料期間範圍及欄位資料
  plotData <- Stock_price4[which((Stock_price4$Date>=plotStartDate)&(Stock_price4$Date<=plotEndDate)),]
  #取出所需的欄位
  plotData %>% select(Date:Volume, MA5:MA60) -> plotData1
  #加入進場位置欄位資訊
  plotData1$inSite <- rep(NA, nrow(plotData1))
  plotData1$inSite[which(plotData$Date==inDate)] <- plotData1$Open[which(plotData1$Date==inDate)]*0.97
  #加入出場位置欄位資訊
  plotData1$outSite <- rep(NA, nrow(plotData1))
  plotData1$outSite[which(plotData1$Date==outDate)] <- plotData1$Close[which(plotData1$Date==outDate)]*1.03
  #將plotData資料由tibble格式轉為xts格式
  plotData2 <- xts(plotData1[,-1], order.by= plotData1$Date)
  #繪製各交易日的K棒圖形
  chart_Series(x=plotData2[,1:5], name=paste0("2603.TW"," 技術分析圖形")) %>%
    add_TA(x=plotData2$MA5, on=1, type="l", col="red", lwd=2.5) %>%
    add_TA(x=plotData2$MA20, on=1, type="l", col="green", lwd=1.5) %>%
    add_TA(x=plotData2$MA60, on=1, type="l", col="orange", lwd=1.5) %>%
    add_TA(x=plotData2$inSite, on=1, type="p", col="red", pch=1, cex=3, lwd=3) %>%
    add_TA(x=plotData2$outSite, on=1, type="p", col="green", pch=1, cex=3, lwd=3) -> plot
  plot
}
strategy_test("2317.TW",10)


