library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")
library("lme4")
#Иллюстрируем переход к логорифмам
#Количественные переменные
h <- diamonds
glimpse(h)

qplot(data=h,carat,price)
bg <- qplot(data=h,log(carat),log(price))
bg + geom_hex()

f <- read.csv("flats_moscow.txt", sep="\t", header = TRUE, dec = ".")
glimpse(f)
qplot(data=f,totsp,price)
qplot(data=f,log(totsp),log(price))

#Качественные переменные
mosaic(data =f, ~walk+brick+floor, shade = TRUE)

#Количественные + качественные переменные
f <- mutate_each(f, "factor", walk, brick, floor, code)
glimpse(f)

qplot(data=f, log(price))
qplot(data=f, log(price), fill = brick)
ggplot(data = f, aes(log(price), fill = brick)) + geom_histogram(position = "dodge")
qplot(data = f, log(price), fill = brick, geom = "density")
g2 <- qplot(data = f, log(price), fill = brick, geom = "density", alpha = 0.5)

g2 + facet_grid(walk~floor)
g2 + facet_grid(~floor)

#Оценка моделей методом найменших квадратов
model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f,log(price)~log(totsp)+brick)
model_2 <- lm(data=f,log(price)~log(totsp)+brick+brick:log(totsp))

summary(model_0)
mtable(model_2)
model_2b <- lm(data=f,log(price)~brick*log(totsp))
mtable(model_2, model_2b)

plot_model(model_2)

nw <- data.frame(totsp = c(60,60), brick = factor(c(1,0)))
nw
predict(model_2, newdata = nw)
exp(predict(model_2, newdata = nw))

predict(model_2, newdata = nw, interval = "confidence")
exp(predict(model_2, newdata = nw, interval = "confidence"))

predict(model_2, newdata = nw, interval = "prediction")
exp(predict(model_2, newdata = nw, interval = "prediction"))

waldtest(model_0,model_1) # H_0: model_0 H_a: model_1
# H_0 отвергается
waldtest(model_1,model_2) # H_0: model_1 H_a: model_2
# H_0 отвергается
waldtest(model_0,model_2) # H_0: model_0 H_a: model_2
# H_0 отвергается

gg0 <- qplot(data=f,log(totsp),log(price))
gg0 + stat_smooth(method="lm")
gg0 + stat_smooth(method="lm") + facet_grid(~walk)
gg0 + aes(col = brick) + stat_smooth(method="lm") + facet_grid(~walk)

f$nonbrick <- memisc::recode(f$brick, 1 <- 0, 0 <- 1)
glimpse(f)
model_wrong <- lm(data=f,log(price)~log(totsp)+brick+nonbrick)
summary(model_wrong)

mtable(model_0, model_1, model_2)
mt012 <- mtable("Model 0"=model_0,
                "Model 1"=model_1,
                "Model 2"=model_2,
                summary.stats=c("Nagelkerke R-sq.","Deviance","AIC","N")
)

resettest(model_2)



df <- diamonds
summary(df)
glimpse(df)
modeldf <- lm(data=df, log(price)~carat)
modeldf
modeldf2 <- lm(data=df, price~carat+x+y)
modeldf2
waldtest(modeldf,modeldf2)
modeldf3 <- lm(data=df, price~carat)
summary(modeldf3)
mtable(modeldf3)
  df4 <- lm(data=df, price~carat+depth+cut)
df4 <- mutate_each(df4, "factor", cut)
mtable(df4,summary.stats = c("AIC"))
df5 <- lm(data=df, price~carat+depth)
mtable(df5,df4) 
waldtest(df5,df4)
resettest(df5)
qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_grid(~cut)
qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_wrap(~cut)
qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) 
qplot(data=df, log(carat), log(price), color = clarity) + facet_wrap(~cut)

df10 <- lm(data=df,price~log(carat))
df10
  df11 <- lm(data=df,price~carat)
df12 <- lm(data=df,price~carat+clarity)
df12 <- mutate_each(df12, "factor", clarity)
mtable(df11)
mtable(df12)
mtable(df11,summary.stats = c("AIC"))
waldtest(df11,df4)
qplot(data = df, log(price), fill=clarity, geom = "density")
qplot(data = df, log(price), fill=clarity, geom = "density", alpha = 0.5) + facet_wrap(~clarity)
qplot(data=df, log(carat), price, color = clarity) + facet_wrap(~cut) 
summary(df)
  df20 <- lm(data=df,price~carat+depth)
mtable(df20,summary.stats = c("AIC"))
mt666 <- mtable("Model 0"=df4,
                "Model 1"=df11,
                "Model 2"=df20,
                summary.stats=c("Nagelkerke R-sq.","Deviance","AIC","N","BIC")
)
mt666                
waldtest(df20,df11)
qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_wrap(~cut)

qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_grid(~cut)
df30 <- lm(data=df, price~log(carat))
df30
resettest(df11)

qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_grid(~cut)
summary(f)
df40 <- lm(data=df, price~carat+clarity)
df41 <- lm(data=df, price~carat)
df40 <- mutate_each(df40, "factor", clarity)
mtable(df40,df41)
mt777 <- mtable("Model 0"=df40,
                "Model 1"=df41,
                summary.stats=c("Nagelkerke R-sq.","Deviance","AIC","N","BIC")
)
mt777
summary(df40)
summary(df41)
qplot(data = df, log(price), fill=clarity, geom = "density", alpha = 0.5) + facet_grid(~clarity)

qplot(data = df, log(price), fill=clarity, geom = "density", alpha = 0.5) + facet_wrap(~clarity)
df50 <- lm(data=df,log(price)~log(carat))
df50


qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_wrap(~cut)

qplot(data = df, log(price), fill=cut, geom = "density", alpha = 0.5) + facet_grid(~cut)
