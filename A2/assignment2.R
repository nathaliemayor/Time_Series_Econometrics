### TSE - Assignment 2 - code

library(xlsx)
library(tidyverse)
library(tseries)
library(lubridate)
library(forecast)
library(slider)
library(ggthemes)
library(broom)
library(sweep)
library(pander)
library(quantmod)
library(urca)
library(broom)
library(urca)
library(broom)
library(magrittr)
library(timetk)
library(recipes)
library(egcm)

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

# 1.a #########################################################################
data<-read.xlsx("MonthlyChocolateSales.xlsx",3,header = TRUE)
data

ts<-ts(data$Sales,frequency = 12,start = c(2015,1))

# Plot the data
autoplot(ts,color="slateblue2")+
  labs(title = "Monthly Sales of Chocolate in Canada",x = "Date",
       y = "Chocolate Sales (mio. CAD)")+
  theme_hc()
  

# 1.b ########################################################################
ggseasonplot(ts,year.labels=TRUE) +
  labs(title = "Monthly Sales of Chocolate in Canada in mio. CAD",y="Chocolate Sales")+
  theme_hc()

ggAcf(ts,lag.max = 36)+
  labs(title = "Autocorrelation function of Monthly Sales of Chocolate in Canada (2015-2020)",y="Autocorrelation")+
  theme_hc()


# 1.c ########################################################################

# 1.d ########################################################################
autoplot(ts,color="black")+
  labs(title = "Monthly Sales of Chocolate in Canada",x = "Date",
       y = "Chocolate Sales (mio. CAD)")+
  geom_line(aes(y = trend_sales), color = "red")+
  theme_hc()

trend_sales = ma(ts, order = 12, centre = TRUE)

plot(ts, col="black",main="Time Series and Trend Component",xlab="Time", ylab="Value")
lines(trend_sales, col="violet") 

plot(as.ts(trend_sales), col="red",main="Trend Component", xlab="Time", ylab="Value")

# detrend the ts
detrend_sales = ts - trend_sales 
plot(as.ts(detrend_sales),col="brown", main="Detrended Series", xlab="Time", ylab="Value")

autoplot(detrend_sales,color="brown")+
  labs(title = "Detrended Series",x = "Date",
       y = "detrended sales")+
  theme_hc()

# 1.e ########################################################################
decompose_data <- decompose(ts, type = "additive") 

plot(decompose_data)


# 1.f ########################################################################
ACFrandom<-decompose_data$random%>%acf(na.action = na.pass,plot=FALSE)

plot(ACFrandom, main="ACF of random component")

# 1.g ########################################################################
arima<-auto.arima(ts)
summary(arima)%>%pander

arima_ord <- auto.arima(data$Sales) 
map_dfr(list(arima, arima_ord), sw_glance) %>% pander

checkresiduals(arima)
checkresiduals(arima_ord)

# 1.h ##################################################
preds <- forecast(arima, h = 24) %>% sweep::sw_sweep(timekit_idx = TRUE, rename_index = "date")

preds %>%
  ggplot(aes(x = date, y = value, color = key)) +
  # Prediction intervals
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#b8daf5", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#478cc4", color = NA, size = 0, alpha = 0.8) +
  # Actual & Forecast
  geom_line(size = 1) + 
  geom_point(size = 2) +
  # Aesthetics
  theme_hc() +
  labs(title = "Sales, 2-Year Forecast", x = "date", y = "value") 


# 2.a ########################################################################
# Load the data from quantmod
getSymbols("EWA",src="yahoo")
getSymbols("EWC",src="yahoo")

# clean up
ewa <- EWA %>% .[,6] %>% window(start = "2012-02-02", end = "2021-02-02") %>% 
  set_colnames("EWA") %>% as_tibble(rownames = "time")
ewc <- EWC %>% .[,6] %>% window(start = "2012-02-02", end = "2021-02-02")  %>% 
  set_colnames("EWC")%>% as_tibble(rownames = "time")
data <- full_join(ewa, ewc, by = "time") %>% mutate(time = as.Date(time))

# reshape and plot
data %>% pivot_longer(-time, "Series") %>% 
  ggplot(aes(x = time, y = value, color = Series)) + geom_line() + 
  ylab("Adj. Closing Price") + xlab("Time")+ theme_hc() + scale_color_manual(values=c("#CC6666", "#9999CC"))

ewadiff<-ts(diff(x=ewa$EWA,lag=1,differences = 1),start=c(2012,2,2),frequency=12)
ewcdiff<-ts(diff(x=ewc$EWC,lag=1,differences = 1),start=c(2012,2,2),frequency=12)
data2<-full_join(ewadiff, ewcdiff, by = "time") %>% mutate(time = as.Date(time))

plot(ewadiff)

  
# 2.b ########################################################################
data <- data %>% mutate(spread = EWA - EWC) 
data %>% ggplot(aes(x = time, y = spread)) + geom_line(color = "slateblue4", alpha = 0.6) +
  ylab("Adj. Closing Price") + xlab("Time") + ggtitle("Spread (EWA-EWC)") + theme_hc()

acfspread<-acf(data$spread)
plot(acfspread,main="ACF plot for the spread")

pacfspread<-pacf(data$spread)
plot(pacfspread,main="PACF plot for the spread")


# 2.c ########################################################################
adfEWA1 = ur.df(y = ewa$EWA, type = "none", selectlags = "AIC") 
summary(adfEWA1)

adfEWC1 = ur.df(y = ewc$EWC, type = "none", selectlags = "AIC") 
summary(adfEWC1)

adfs1 = ur.df(y = data$spread, type = "drift", selectlags = "AIC") 
summary(adfs1)

adf_res <- data %>% select(-time) %>% 
  imap(~ur.df(.x, type = "drift", selectlags = "AIC")@testreg %>% tidy() %>% filter(!grepl("diff", term)) %>% mutate(series = .y)) %>%
  do.call(what = bind_rows) %>% select(series, statistic, p.value)

pander(adf_res)



# 2.d ########################################################################
ca.jo(data %>% select(EWA, EWC)) %>% summary()
egcm(data %>% select(EWA, EWC)) %>% summary()



