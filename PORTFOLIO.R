
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
