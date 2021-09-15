library(lubridate) # работа с датами
library(rtools)
library(MLmetrics)
library(hydroGOF)
library(zoo) # временные ряды
library(xts) # еще ряды
library(forecast) # ARMA, ETS модели
library(tidyverse) # графики и манипуляции с данными, подключаются пакеты dplyr, ggplot2, etc
library(quantmod) # загрузка с finance.yahoo.com
library(sophisthse) # загрузка с sophist.hse.ru
install.packages("rusquant", repos = "http://r-forge.r-project.org", type = "source")
devtools::install_github("bdemeshev/sophisthse")


y <- Arima(y, order = c(1, 1, 0), include.drift = TRUE)
ggtsdisplay(y)

sophisthse("hhi_q_i$HHI_Q_DIRI")
#14
yy <- hhi_q_i$HHI_Q_DIRI
yy
yyy <- yy[1:89,]
yyy
mod_1 <- Arima(yyy, order = c(1, 0, 0))
mod_2 <- Arima(yyy, order = c(3, 0, 0))
mod_3 <- Arima(yyy, order = c(2, 0, 0))
mod_4 <- Arima(yyy, order = c(1, 1, 0))
AIC(mod_1)
AIC(mod_2)
AIC(mod_3)
AIC(mod_4)

#15
yy
yyy2 <- yy[62:89,]
yyy2
mod_a <- auto.arima(yyy2)
AIC(mod_a)

#16
yyy
mod_16 <- Arima(yyy, order = c(2, 1, 0))
prognoz_16 <- forecast(mod_16, h = 3)
prognoz_16

#17
yyy3 <- yy[1:86,]
mod_11 <- Arima(yyy3, order = c(1, 1, 2))
mod_22 <- Arima(yyy3, order = c(1, 1, 1))
mod_33 <- Arima(yyy3, order = c(1, 0, 2))
mod_44 <- Arima(yyy3, order = c(0, 1, 1))
prognoz_17 <- forecast(mod_11, h = 3)

#18
mod_19 <- Arima(yyy, order = c(1, 1, 0), seasonal = c(0, 0, 1))
AIC(mod_19)


set.seed(2)

y_1 <- arima.sim(n = 100, list(ar = 0.99))

tsdisplay(y_1) 

t_1 <- c(1:100) 

t_2 <- t_1^2 

t_3 <- t_1^3 

d <- data.frame(y = y_1, x = t_1+t_2+t_3) 
model <- lm(data = d, y ~ x)

res <- summary(model) 

res$fstatistic
#

set.seed(1)

test <- arima.sim(n=100, list(ar=0.5))
summary(test)

###########

