#memerikasa apakah library "pacman" suda terinstall, dan melakukan install jika belum
if (!require("pacman"))
  install.packages("pacman")

#me-load semua libraries R yang dibutuhkan dengan lib "pacman",d an auto install jika belum ada
pacman::p_load("dplyr", "tidyverse", "magrittr", "skimr", "dygraphs", 
               "data.table", "lubridate", "anomalize", "seasonal", 
               "TSstudio", "forecast", "tsfeatures", "seastests", "TTR")

#membaca files datasets
mmilk <- fread("monthly_milk.csv")

#melakukan persiapan data
glimpse(mmilk)
range(mmilk$month)
mmilk$month <- ymd(mmilk$month)

#mmilk_ts2 <- xts(mmilk$milk_prod_per_cow_kg, order.by = mmilk$month)
mmilk_ts <- ts(mmilk$milk_prod_per_cow_kg, frequency = 12, start = c(1960,1), end = c(1972,12))
autoplot(mmilk_ts) #forecast::
mmilk_ts

#menggambarkan grafik
plot(mmilk_ts)

ts_plot(mmilk_ts, slider=TRUE) #TStudio::

mmilk_ts %>%  #dygraphs::
  dygraph() %>% 
  dyCrosshair("vertical") %>% 
  dySeries("V1", "Milk Production") %>% 
  dyRangeSelector(height = 50) %>% 
  dyUnzoom()

#Memeriksa Trend
autoplot(SMA(mmilk_ts, n=12)) #TTR::

#Memeriksa seasonality 
ts_seasonal(mmilk_ts, type="all") #TSStudio::
isSeasonal(mmilk_ts) #seastests::

#decompose dengan anomalize untuk persiapan analisa rumus dengan mudah
ds <- decompose_stl(data=mmilk, target=milk_prod_per_cow_kg) #anomalize::

#pembuktian rumus Xt=Tt+St+Et
ds %>% 
  mutate(Xt=season+trend+remainder) %>% 
  kable() #kableExtra

#menggunakan proses decompose klasik
mlk <- decompose(mmilk_ts) #base
autoplot(mlk)

#melakukan pemeriksaan residual untuk proses modeling
checkresiduals(mmilk_ts) #forecast::
tsdisplay(mmilk_ts, lag.max = 50) #forecast::

#meneruskan dengan forecasting sederhana
mmilk_f <- forecast(mmilk_ts, h=48) 
autoplot(mmilk_f)
mmilk_f$mean #forecast data numerik