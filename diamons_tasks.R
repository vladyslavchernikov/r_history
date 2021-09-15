library(caret) # стандартизованный подход к регрессионным и классификационным моделям
library(AER) # инструментальные переменные
library(sandwich) # робастные стандартные ошибки
library(ivpack) # дополнительные плющки для инструментальных переменных
library(memisc) # табличка mtable
library(rio) # импорт файлов разных форматов
library(tidyverse) # графики и манипуляции с данными, подключаются пакеты dplyr, ggplot2, etc

h <- diamonds
h1 <- mutate(h, logcarat = log(carat), logdepth = log(depth),logtable=log(table),logprice=log(price))
set.seed(12345) 

train_ind <- createDataPartition(h1$price, p=0.8, list=FALSE)

h1_train <- h1[train_ind,] 

h1_test <- h1[-train_ind,]
model_1 <- lm(data=h1_train, logprice~logdepth+logtable+clarity)
pred_1<-predict(model_1,h1_test)

sum((pred_1-h1_test$price)^2)
##############
set.seed(12345) 
train_ind <- createDataPartition(h$price, p=0.8, list=FALSE) 
h_train <- h1[train_ind,] 
h_test <- h1[-train_ind,] 
model_1 <- lm(data=h1_train, logprice~logcarat+logtable+clarity) 
y_hat_1 <- predict(model_1, h1_test) 
y3<-sum((h1_test$price -y_hat_1)^2) 
y3
t<-y3/1000000000 
t


#######
help(CollegeDistance)
dat<-CollegeDistance
model_iv5 <- ivreg(data = dat, wage ~ education + region + gender + ethnicity + unemp| distance + gender  + unemp + region) 

coeftest(model_iv5)

#####
h <- CollegeDistance
set.seed(12345) 
train_ind <- createDataPartition(h$wage, p=0.9, list=FALSE) 
h_train <- h[train_ind,] 
h_test <- h[-train_ind,] 
model_iv5 <- ivreg(data = dat, wage ~ education + region + gender + ethnicity + unemp| distance + gender + ethnicity + unemp + region)  
y<-h_test$wage 
y_hat_1 <- predict(model_iv5, h_test) 
y2<-exp(y_hat_1) 
y3<-sum((y-y2)^2) 
t<-y3/1000000000 
t
sjp.lm(model_1)
y_hat_1
1
################
hh <- CollegeDistance
set.seed(42) 
train_ind <- createDataPartition(hh$wage, p=0.9, list=FALSE) 
hh_train <- hh[train_ind,] 
hh_test <- hh[-train_ind,] 
model_ivhh <- ivreg(data = hh_train, wage ~ education + region + gender + ethnicity + unemp| distance + gender + ethnicity + unemp + region)
coeftest(model_ivhh)
y_hat_hh <- predict(model_ivhh, hh_test)
y_hat_hh
