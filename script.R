#

library(dplyr)
library(tidyr)
library(nycflights13)
library(ggplot2)
library(EDAWR)
data("toyb")


help(package = "nycflights13")

flights
class(flights)

glimpse(flights)

select(flights, ends_with('time'))
flights %>% select(ends_with('time'))

select(flights, contains("ar"))
names(flights)


filter(flights, Year == 2011 & DayOfWeek == 3)
flights %>% filter(Year == 2011 & DayOfWeek == 5)
flights %>% filter(Year == 2012)

storms %>% filter(wind>50)
storms %>% filter(storm %in% c("Alberto", "Alex"))
storms %>% filter(wind>50, storm %in% c("Alberto", "Alex"))
flights %>% filter(ArrDelay != 'NA') %>% select(UniqueCarrier, ArrTime)
flights %>% filter(!is.na(ArrDelay))

storms %>% filter(wind >50) %>% select(storm, pressure)


storms %>% mutate(ratio = pressure/wind, inverse = ratio^-1) %>% 
  select(ratio, inverse)

qplot(ratio, inverse, data = storms)

storms %>% mutate(abc = cummax(pressure)*.890) %>% select(pressure, abc)

flights %>% mutate(speed = Distance/AirTime*60) %>% 
  select(UniqueCarrier, ArrDelay, speed) %>% 
  arrange(desc(ArrDelay))


pollution %>% summarise(mean = mean(amount), variance = var(amount), 
                        median = median(amount), n=n())

pollution %>% group_by(city) %>% summarise(mean = mean(amount))

flights %>% filter(!is.na(air_time), !is.na(distance)) %>% 
  summarise(number_of_records=n(), 
            total_time = sum(air_time), 
            total_dist=sum(distance),
            n_carriers=sum(n_distinct(carrier)))

flights  %>% filter(!is.na(ArrDelay), ArrDelay>100)  %>%
  select(ArrDelay, UniqueCarrier)  %>%
  group_by(UniqueCarrier) %>% 
  summarise(total_delay_in_hours = sum(ArrDelay)/60) %>% 
  arrange(desc(total_delay_in_hours))


pollution %>% group_by(city) %>% summarise(mean(amount))

pollution
pollution %>% group_by(city) %>% arrange(city)

pollution %>% group_by(city) %>% arrange(city) %>% summarise(mean(amount))

delays <- flights %>% filter(!is.na(arr_delay)) %>% group_by(carrier) %>% 
  summarise(average_delay=mean(arr_delay)) %>% arrange(desc(average_delay))

fl <- flights %>% filter(is.na(arr_delay))                                                
length(fl)

pollution %>% ungroup()

pollution %>% group_by(city, size)

toyz <- toyb

toyz %>% group_by(country, year) %>% summarise(AVG=sum(cases))

# ?????????????? ?????????????? ???? ?????????????? ???????????? ?? ?? ?????????? ????????????
flights %>% group_by(origin, dest) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% ungroup()

#Number of cases by Country and Year
tb2 <- tb %>%
  mutate(cases = child+adult+elderly) %>% 
  select(country:sex, cases) %>% 
  filter(!is.na(cases)) %>% 
  group_by(country, year) %>% 
  summarise(cases=sum(cases)) %>% 
  ungroup() %>% 
  arrange(desc(cases))

rawtb %>% group_by(country, year, sex, age) %>% summarise(n = n())
rawtb %>% group_by(country, year, sex) %>% summarise(n = n())
rawtb %>% group_by(country, year) %>% summarise(n = n())
rawtb %>% summarise(n=n())

# TIDYR

#observation from variables
cases2 <- gather(cases, 'year', 'n', 2:4, convert = T)

#Make variables from observations
spread(cases2, year, n)

#Separate the column to more columns
storms2 <- storms %>% separate(date, c('year', 'month', 'day'), sep='-')
unite(storms2, 'date', year, month, day, sep='-')



