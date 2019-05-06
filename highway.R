# title : R軟體開放資料應用
# date : 2019.05.08
# author : Ming-Chang Lee
# email : alan9956@gmail.com
# RWEPA : http://rwepa.blogspot.tw/
# Encoding : UTF-8

library(ggplot2)  # 繪圖
library(forecast) # 預測

# 大綱 -----
# 1.資料分析/視覺化/互動式案例
# 2.大數據分析工具與架構
# 3.R/RStudio簡介
# 4.資料物件,套件,輔助說明
# 5.開放資料分析-高速公路


# 1.資料分析/視覺化/互動式案例 -----
# CWB 1,600萬筆資料
# http://rwepa.ddns.net:3838/sample-apps/cwb_vis_qc/

# 3.R/RStudio簡介 -----

# R官網: http://www.r-project.org/

# R 執行畫面 - Windows
plot(runif(100), type="l")
demo(graphics)
demo(persp)

# 參考文獻
citation()

# RStudio官網 http://www.rstudio.com/
# RStudio daily builds https://dailies.rstudio.com/

# 4.資料物件,套件,輔助說明 -----

# Task Views - R套件區分成40個類別
# http://rwepa.blogspot.com/2013/10/packages-list-32.html

# 輔助說明 help -----

help.start() # 線上說明首頁

?plot # 查詢plot函數
help(plot)

help.search("regression") # 查詢所有說明
??regression

# R 高效能計算 -----
x <- c(1:23000000)
x2 <- x^2
head(x)
head(x2)
system.time(x2 <- x^2)

# ls 列出所有物件
ls()

# 刪除特定物件
rm(x)

# getwd 取得工作目錄
getwd()

# setwd 設定工作目錄
setwd("C:/rdata")

# 開放資料分析-高速公路 -----

# 交通部統計查詢網
# https://stat.motc.gov.tw/mocdb/stmain.jsp?sys=100
# 先儲存成 Excel, 再另儲存成 .csv

# 匯入資料
highway <- read.table("交通事故.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# 刪除多餘的最後一筆資料
highway <- highway[-220, ]

names(highway) <- c("時間", "肇事件數", "死亡人數", "受傷人數", "取締違規件數")

head(highway)

highway$時間 <- as.Date(highway$時間)

str(highway)

summary(highway)

#Q1. 哪些年月份肇事件數較多?

# 新增年的資料行
highway$時間.年 <- factor(format(highway$時間, format="%Y"))

# 新增月的資料行
highway$時間.月 <- factor(format(highway$時間, format="%m"))

# case 1 畫點圖, 看不出什麼內容???
ggplot(data=highway, aes(x=時間, y=肇事件數)) +
  geom_point()

# case 2 畫線圖, 好像比較有顯示趨勢變化.
ggplot(data=highway, aes(x=時間, y=肇事件數)) +
  geom_line()

# case 3 客製化x-軸, x軸顯示年月日
ggplot(data=highway, aes(x=時間, y=肇事件數)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year")

# case 4 客製化x-軸,顯示年/月
ggplot(data=highway, aes(x=時間, y=肇事件數)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", 
               date_breaks = "1 year") +
  geom_vline(xintercept = highway$時間[which.max(highway$肇事件數)], color = "blue") +
  annotate("text", x = highway$時間[which.max(highway$肇事件數)], y = 1, 
           label = " 肇事件數最大值=17\n 2005-06", color = "blue", hjust = 0) +
  ggtitle("交通部高速公路肇事件數統計圖(90年1月-108年3月)") +
  theme(plot.title = element_text(hjust = 0.5))

# case 5 群組-各年趨勢圖1
ggplot(data=highway, mapping=aes(x=時間.月 , y=肇事件數, color=時間.年)) +
  geom_line(aes(group = 1), size=2) +
  facet_wrap( ~ 時間.年)
  
# case 6 群組-各年趨勢圖2
ggplot(data=highway, mapping=aes(x=時間.月 , y=肇事件數, color=時間.年)) +
  geom_line(aes(group = 1)) +
  geom_point(aes(group = 1)) +
  facet_grid(facets = 時間.年 ~ ., margins = FALSE) + 
  theme_bw()

# Q2. 繪製各年累計肇事件數統計圖?
mydata <- aggregate(肇事件數 ~ 時間.年, data=highway, FUN=sum)
mydata$時間.年 <- as.numeric(levels(mydata$時間.年))

ggplot(data=mydata, mapping=aes(x=時間.年 , y=肇事件數)) +
  geom_line() +
  geom_point() +
  ggtitle("交通部高速公路肇事件數統計圖(90年~108年)") +
  theme(plot.title = element_text(hjust = 0.5))

# Q3. 取締違規件數與肇事件數關係為何?
ggplot(data=highway, mapping=aes(x=肇事件數 , y=取締違規件數)) +
  geom_point() +
  geom_smooth() # 預設是非線性平滑法

ggplot(data=highway, mapping=aes(x=肇事件數 , y=取締違規件數)) +
  geom_point() +
  geom_smooth(method="lm") # 線性法

ggplot(data=highway, mapping=aes(x=肇事件數 , y=取締違規件數)) +
  geom_point() +
  geom_smooth(span = 0.3)

ggplot(data=highway, mapping=aes(x=肇事件數 , y=取締違規件數, color=時間.年)) +
  geom_point(size=2) +
  geom_smooth(method = "lm")

ggplot(data=highway, mapping=aes(x=肇事件數 , y=取締違規件數, color=時間.年)) +
  geom_point(size=2) +
  geom_smooth(method = "loess")

# Q4. 預測未來?
highway.ts <- ts(data=highway$肇事件數, start=c(2001,1), frequency=12)
highway.ts

fit <- auto.arima(highway.ts)
forecast(fit, h=3) # 未來3個月(1季)

plot(forecast(fit, h=3), main="交通部高速公路肇事件數統計圖2001年~2019年-預測未來3個月\n製表:RWEPA")
grid()

# 參考資料 -----

# RWEPA
# http://rwepa.blogspot.com/ 
  
# R基礎篇
# http://rwepa.blogspot.tw/2013/01/r-201174.html

# R軟體教學影片介紹
# http://rwepa.blogspot.com/2018/09/trainingvideo.html

# R軟體論壇
# https://groups.google.com/forum/#!forum/taiwanruser

# Tibame
# https://www.tibame.com/ --> 線上增能 --> 數據分析系列 --> 程式語言 --> R語言
# https://www.tibame.com/course/41
# end
