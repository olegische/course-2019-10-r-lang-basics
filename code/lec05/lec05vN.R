library(dplyr)
data('mtcars')
str(mtcars)

s <- select(mtcars, hp, mpg, gear) #выбор выборки
select(mtcars, 1:7) #обращение к выборке по индексам столбцов
filter(mtcars, wt>3.5) #фильтрация
filter(mtcars, wt>3.5, carb==3)

arrange(mtcars, gear) #сортировка по умолчанию по возрастанию
arrange(mtcars, -gear)#сортировка в обратном порядке
arrange(mtcars, -gear, -carb, wt)

mutate(mtcars, wt_cyl = wt / cyl) #создать другую переменную на нашей выборке, выборка не изменится
head(mtcars)
mtcars2 <- mutate(mtcars, wt_cyl = wt / cyl)
head(mtcars2)
mutate(mtcars, wt_cyl = wt / cyl, inv_wt_cyl = 1/wt_cyl) #можно обращаться к этой вновь созданной переменной

summarise(mtcars, min_hp=min(hp), mean_hp=mean(hp), max_hp=max(hp), IQQ_hp=IQR(hp)) #для группировки удобно
summary(mtcars)

group_by(mtcars, gear) #это tibble а не датафрейм
mtcars[,2] #вектор
grouped <- group_by(mtcars, gear)
summarise(grouped, min_hp=min(hp), mean_hp=mean(hp), max_hp=max(hp), IQQ_hp=IQR(hp)) #!! сгруппированы
grouped2 <- group_by(mtcars, gear, cyl)
summarise(grouped2, min_hp=min(hp), mean_hp=mean(hp), max_hp=max(hp), IQQ_hp=IQR(hp))

Events$rate <- Events$total/Events$population *100000 
filter(Events,rate< 0.71 )
select(filter(Events, rate <= 0.71), state, region, rate)
mutate(Events, pop=population/1000000, rank_rate=rank(rate))
head(select(Events, state,population), 5)
filter(Events, (state == "New York" | state == "Texas"))
filter(Events, state %in% c("New York", "Texas"))
notFlorida <- filter(Events, state != "Florida")
notFlorida
notSouth <- filter(Events, region != "South")
notSouth
Events3 <- filter(Events, region %in% c("Northeast", "West"))
grouped3 <- group_by(Events3, state)  
summary(grouped3)
summarise(grouped3,min=min(rate), max=max(rate),mean=mean(rate), iqr=IQR(rate))
summarise(filter(Events, region %in% c("Northeast","West")),
          min_pop=min(population), mean_pop = mean(population),
          max_pop=max(population), IQR_pop=IQR(population),
          min_t=min(total), mean_t = mean(total),
          max_t=max(total), IQR_t=IQR(total),
          min_r=min(rate), mean_r = mean(rate),
          max_r=max(rate), IQR_r=IQR(rate))


##PIPE
mtcars %>% group_by(gear)
group_by(mtcars, gear) #тоже самое
mtcars %>% group_by(gear) %>% summarise(med_wt = median(wt))
mtcars4 <- mtcars %>% select(2:11) %>% filter(wt>=2) 
mtcars5 <- mtcars %>% group_by(gear, cyl) %>% mutate(wt_cyl=wt/cyl) %>% arrange(wt_cyl)

16 %>% sqrt() %>% log2()
log2(sqrt(16))  

Events %>% filter(rate< 0.71 ) %>% select(state, region, rate)

Events <- mutate(Events, rank=rank(-rate))
My_state <- filter(Events, region %in% c("Northeast", "West") & rate<1)
select(My_state, state, rate, rank) 
#тоже самое
Events %>% mutate(rank=rank(-rate)) %>% filter(region %in% c("Northeast", "West") & rate<1) %>% select(state, rate, rank) 


library(dslabs)
data("heights")
str(heights)
s <- heights %>% filter(sex=='Female') %>%  summarise(mean=mean(height), sd=sd(height))
s$mean
s1 <- heights %>% filter(sex=='Female') %>%  summarise(med=median(height), min=min(height), max=max(height))

#квантиль 25% = квартиль. 1=25% 2=50% (медиана) 3=75%. IQR = между 25 и 75
quantile(heights$height, c(0,0.5,1))

x <- 1:10
x <- rnorm(10) #другая выборка
quantile(x)
#функция считает 1й квартиль (3.25) - д/з

#PULL
class(Events)
us_Events_rate <- Events %>%
  summarise(rate=sum(total/sum(population)*1000000))
us_Events_rate %>% pull(rate)
class(us_Events_rate$rate)

#сгруппировать по полу, вывести таблицу из среднего и станд отклонения
heights %>% group_by(sex) %>% summarise(mean=mean(height), sd=sd(height))
#отсортировать по населению, по rate в обратном пордке, вывести первые 10 строк
Events %>% arrange(population, -rate) %>% head(10) #сортировка в обратном порядке
Events %>% arrange(population, -rate) %>% top_n(10, rate)


library(NHANES)
data('NHANES')
str(NHANES)
?NHANES
ref <- NHANES %>% filter(AgeDecade == ' 20-29') %>% select(BPSysAve) %>% summarise(mean_BPSysAve=mean(BPSysAve, na.rm = T), sd_BPSysAve=sd(BPSysAve,na.rm = T)) 
ref$mean_BPSysAve



#найти среднее и вернуть с помощью pull
ref_avg <- NHANES %>% filter(Age == (20:29)) %>% summarise(avg=mean(BPSysAve, na.rm = T))
ref_avg$avg
ref_avg %>% pull(avg)

#3
#По группе лиц возраста 20-29 лет мы зарегистрировали минимум ... и максимум ... кровяного давления (Syst).
NHANES %>%  filter(AgeDecade == " 20-29") %>%  select(BPSysAve) %>%  min(na.rm = T) 
NHANES %>%  filter(AgeDecade == " 20-29") %>%  select(BPSysAve) %>%  max(na.rm = T)

#функция
Min_max_fun <- function(){
  x <- as.numeric(readline(prompt = "Введите возраст: "))
  x <- as.numeric(x)
  Min <- NHANES %>%
    filter(Age == x) %>%
    select(BPSysAve) %>%
    min(na.rm = T)
  Max <- NHANES %>%
    filter(Age == x) %>%
    select(BPSysAve) %>%
    max(na.rm = T)
  result <- list(min=Min, max=Max)
}
min_max <- Min_max_fun()
min_max

# группа по полу, для каждого возраста среднее по давлению
NHANES %>% group_by(Gender, Age) %>% mean(BPSysAve, na.rm = T) #не работает
NHANES %>% group_by(Gender, Age) %>% summarise(mean(BPSysAve, na.rm= T))

# для м 40-49 лет сравнить Syst давление по рассовой принадлежности, отсортировать по возрастанию среднего давления
NHANES %>% filter(AgeDecade == " 40-49") %>% select(Race1, BPSysAve) %>% group_by(Race1) %>% summarise(mean=mean(BPSysAve, na.rm= T)) %>% arrange(mean)

# оператор .
filter(Events, region=='South') %>% mutate(rate = total/population*10^5) %>%
  summarise(med=median(rate)) %>%
  pull(med)
rates <-  filter(Events, region=='South') %>% mutate(rate = total/population*10^5) %>%
  .$rate
median(rates)

heights %>% group_by(sex) %>% fun()
heights %>% group_by(sex) %>% do(fun(.))

#Tibble
MyDf <- data.frame(x=1:4, y=c("L", "B", "LV", "Ber"))
myDfNotFactor <- data.frame(x=1:4, y=c("L", "B", "LV", "Ber"), stringsAsFactors = F) #не факторы, а строки

data('starwars')
as.data.frame(starwars)
starwars$name
as.data.frame(starwars[,1])
as.data.frame(starwars[[1]])
starwars[[1]]

MyDf[,1] #вектор или датафрейм, не известно заранее
MyDf[,1:2] #датафрейм точно
MyTib <- tibble(x=1:4, y=c("L", "B", "LV", "Ber"))
MyTib[,1] #всегда выводить тибл
MyTib[[1]] #вектор принудительно
MyTib$y # символы
MyDf$y # факторы


#создание тибл
sT <- tibble(nItems=c(12,45,107), cost=c(0.5, 1.2,1.8), totalworth=nItems*cost)

library(tibble)
data('CO2')
CO2tib <- as_tibble(CO2)
summary(CO2tib)
CO2tib
