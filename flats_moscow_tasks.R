library("memisc")
library("dplyr")
library("psych")
library("lmtest")
library("sjPlot")
library("sgof")
library("ggplot2")
library("foreign")
library("car")
library("hexbin")

# Генерируем случайные величины
# Z_1, ... ,Z_100 ~ N(5, 9)
z <- rnorm(100, mean=5, sd=3)
z[56]
z[2:9]

qplot(z)

# Построение функции плотности
x <- seq(-10,15,by=0.5)
y <- dnorm(x, mean=5,sd=3)
qplot(x,y)
qplot(x,y,geom = "line")

# Расчет вероятности
# P(Z<3)
# P(Z<3) = F(3)
pnorm(3,mean=7,sd=3)
pt(4,df=2)
#P(Z in [4:9])
#P(Z<9)-P(Z<4)
pnorm(9,mean=5,sd=3) - pnorm(4,mean=5,sd=3)


# Квантиль распределения
#P(Z<a)=0.7 a-?
qnorm(0.7, mean=5,sd=3)


# chisq, t, f
#rt, dt, pt,qt ....


# Множественная ререссия. Проверка гипотез
h <- swiss
glimpse(h)
help(swiss)
model <- lm(data = h, Fertility~Catholic+Agriculture+Examination)
summary(model)
coeftest(model)
confint(model)
plot_model(model)

# Проверка гипотизы b_Cath=b_Agri
model_aux <- lm(data = h, Fertility ~ Catholic + I(Catholic + Agriculture) + Examination)
summary(model_aux)

# Проверка гипотезы без построения вспомогательной модели
linearHypothesis(model, "Catholic-Agriculture=0")

# Масштабируем каждую переменную (вычитаем среднее, делим на стандартную ошибку)
h_st <- mutate_each(h, "scale")
glimpse(h_st)
model_st <- lm(data = h_st, Fertility~Catholic+Agriculture+Examination)
summary(model_st)
plot_model(model, type = "std")


# Искусственый эксперимент
# Матрица в 100 строк, слепленная из вектора в котором 4100 элементов

D <- matrix(nrow = 100, rnorm(100 * 41, mean = 0, sd = 1))            
df <- data.frame(D)  
glimpse(df)

model_pusto <- lm(data = df, X1~. )
summary(model_pusto)            

# Сравнить несколько моделей
model2 <- lm(data = h, Fertility~Catholic+Agriculture)
compar_12 <- mtable(model, model2)
compar_12


# Сохранение результатов работы
stuff <- list(data=h, model=model2)
saveRDS(file = "mydata.RDS", stuff)

#Загрузка данных
mylist = readRDS("mydata.RDS")
summary(mylist$model)

# csv. comma separated values

t <- read.csv("flats_moscow.txt")
glimpse(t)

t <- read.csv("flats_moscow.txt", sep="\t", dec=".",header = TRUE)
glimpse(t)

mod_3 <- lm(data = t, price ~ totsp + brick)
summary(mod_3)




data <- diamonds
summary(data)
model3 <- lm(data = data, price ~ x + y + carat + table)
summary(model3)
linearHypothesis(model3, "y=0")
glimpse(model3)

model3 <- glm(data = data, price ~ carat)
model4 <- lm(data = data, price ~ carat + y)
summary(model3)
summary(model5)
model5 <- lm(data = data, price ~ carat + y + x)
confint(model5, level = 0.90)
help(diamonds)
