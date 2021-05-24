R PROGS


# ДЛЯ ПОРТФОЛИО - IN DIAMONDS(ggplot2)
#проверьте гипотезу о взаимосвязи
# цены (price) и каратов (carat) бриллиантов
library(ggplot2)
t1 <- diamonds
mean_price <- mean(t1$price)
mean_carat <- mean(t1$carat)
modjo <- ifelse(t1$price >= mean_price ,1 ,0)
modjo2 <- ifelse(t1$carat >= mean_carat ,1 ,0)
chisq.test(modjo, modjo2)


####!Постройте столбчатую диаграмму распределения 
#цвета глаз по цвету волос только у женщин из
#таблицы HairEyeColor. По оси X должен идти цвет волос, цвет столбиков
#должен отражать цвет глаз. По оси Y - количество наблюдений.

library("ggplot2")
mydata <- HairEyeColor
mydata <- as.data.frame(HairEyeColor[ , ,'Female'])
obj <- ggplot(data = mydata, aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
obj


#### Узнать в таблице HairEyeColor встроенная в R, 
## сколько красноволосых мужчин с голубыми глазами

red_man <- HairEyeColor['Red','Blue' ,'Male']
blue_eyes_man <- HairEyeColor[,"Blue",'Male']
red_man_sum <- sum(red_man)
blue_eyes_man_sum <- sum(blue_eyes_man)
red_men <- red_man_sum / blue_eyes_man_sum
red_men



#  Представлено в дате только месяца, которые больше, чем предыдущий
good_months <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]]




#Воспользуемся еще одним встроенным набором данных в R  -
#ToothGrowth. Данные позволяют исследовать рост зубов у
#морских свинок в зависимости от дозировки витамина C
#и типа потребляемых продуктов.

#Сравните среднее значение длины зубов свинок, 
#которые потребляли апельсиновый сок (OJ) 
#c дозировкой 0.5 миллиграмм, со средним
#значением длины зубов свинок, которые потребляли 
#аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма. 
Tooth <- ToothGrowth
a <- Tooth$supp == 'OJ' & Tooth$dose == '0.5'
b <- Tooth$supp == 'VC' & Tooth$dose == '2'


t_stat <- t.test(Tooth$len[a], Tooth$len[b])[1]


#Проведите однофакторный дисперсионный анализ с повторными измерениями:
#  влияние типа таблетки (pill) на температуру (temperature) с учётом испытуемого (patient).
# Каково p-value для влияния типа таблеток на температуру?

df <- read.csv("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv")
View(df)
df$patient <- as.factor(df$patient)
xxx <- aov(temperature ~ pill + Error(patient/pill),data=df)
summary(xxx)


#провести двухфакторный дисперсионный анализ с повторными измерениями: 
#влияние факторов doctor, влияние фактора pill и их взаимодействие на temperature.
#Учтите обе внутригрупповые переменные: и тот факт, что один и тот же больной принимает
#разные таблетки, и тот факт, что  один и тот же больной лечится у разных врачей!
# Каково F-значение для взаимодействия факторов доктора (doctor) и типа таблеток (pill)?

df2 <- read.csv("https://stepic.org/media/attachments/lesson/11505/Pillulkin.csv")
View(df2)

df2$doctor <- as.factor(df2$doctor)
df2$pill <- as.factor(df2$pill)
df2$patient <- as.factor(df2$patient)
thrill <- aov(temperature ~ doctor*pill + Error(patient/(doctor*pill)),data=df2)
summary(thrill)



#Воспользуемся уже знакомыми данными diamonds из 
#библиотеки ggplot2. Только для бриллиантов класса Ideal
#(переменная cut) c числом карат равным 0.46 (переменная carat)
#постройте линейную регрессию, где в качестве зависимой
#переменной выступает price, в качестве предиктора - переменная
#depth. Сохраните коэффициенты регрессии в переменную fit_coef.


df <- diamonds

hqd <- subset(df, cut == "Ideal" & carat == 0.46)
fit <- lm(price ~ depth, hqd)
fit_coef <- fit$coefficients
