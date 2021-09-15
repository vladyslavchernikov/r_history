library("psych")
library("dplyr")
library("ggplot2")
library("Ggally")
library("Ecdat")
library("lmtest")
library("car")
chick <- ChickWeight
summary(chick)
table(chick)
subset(chick,Time = 10)
glimpse(chick)
mean(chick$weight,chick$Time=10)
summary(chick,Time = 10)
view(chick)
qplot(data=chick, Time, weight)
a <- subset(chick, Time<11, select = c(weight, Time))
a <- subset(a, Time>9, select = c(weight, Time))
a
summary(chick)
mean(a$weight)
d1 <- subset(chick, Time < 22 , select = c(weight, Time, Diet))
d1 <- subset(d1, Time > 20 , select = c(weight, Time, Diet))
d1

reg <-lm(data=chick,weight~Time+Diet)
summary(reg)
diamonds <-diamonds
qplot(data = diamonds, price,fill=cut)+facet_grid(~clarity)
qplot(data = diamonds, log(price),fill=cut)+facet_grid(~cut)
qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)
qplot(data = diamonds, price,fill=cut)+facet_wrap(~clarity)
lmlm <- lm(data=diamonds, price~carat+table+x+y+z+depth)
summary(lmlm)
lmtest<-lm(data=diamonds,price~carat+y+x)
summary(lmtest)
help(diamonds)
confint(lmtest,level=0.9)
lmtest<-lm(data=diamonds,price~carat+y+x+table+depth)
confint(lmtest,level=0.9)
food <- BudgetFood
lmfood <- lm(data=food,wfood~totexp+size)
help(BudgetFood)
nw<-data.frame(totexp=700000,size=4)
predict(lmfood, newdata = nw, interval = "prediction")
resettest(lmfood)
h <- na.omit(BudgetFood)
lmfood2 <- lm(data=h,wfood~totexp+size)
waldtest(lmfood, lmfood2)










d<-ChickWeight
d2<-d[d$Time==21,]
mean(d2[d2$Diet==4,]$weight)
qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)


mtcars <-mtcars
help(mtcars)
mllm<-lm(data=mtcars,mpg~cyl+disp)
mllm2<-lm(data=mtcars,mpg~cyl+disp+hp)
summary(mllm)
summary(mllm2)
glimpse(mtcars)

mllm3<-lm(data=mtcars,mpg~disp+hp+wt)
vif(mllm3)
h.pca <- prcomp(mtcars, scale = TRUE)
pca1 <- h.pca$x[, 1]
head(pca1)
v1 <- h.pca$rotation[, 1]
norm(v1, type="2")



h <- BudgetFood
h2 <- BudgetFood
h <- na.omit(BudgetFood)
lmh<-lm(data=h, wfood~totexp+size)
lmh2<-lm(data=h2, wfood~totexp+size)
resettest(lmh)
resettest(lmh2)


model_r<-lm(data=h,wfood~totexp+size)
model_ur<-lm(data=h,wfood~totexp*sex+size*sex)
waldtest(model_r,model_ur)






nd<-data.frame(size=4,totexp=700000)
predict(lmh2,newdata = nd, interval ="prediction", level=0.9)

qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)


qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)

qplot(data = diamonds, log(price),color=cut)+facet_grid(~cut)


car<-mtcars
lmcar<-lm(data=car,mpg~disp+hp)
lmcar2<-lm(data=car,mpg~disp+hp+wt)
summary(lmcar)
summary(lmcar2)
