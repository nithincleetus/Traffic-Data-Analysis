library(ggplot2)
library(ggpubr)
library(summarytools)
library(readxl)
library(dplyr)

#Importing Excel to R dataframe

speed <- read_xlsx('D:/University/Statistics/R Prog/Q10 Speed Data.xlsx')
speed$Day <- weekdays(as.Date(speed$Date)) #adding days column
View(speed)

obstacle <- read_xlsx('D:/University/Statistics/R Prog/Q10 Obstacle Data.xlsx')
obstacle$Day <- weekdays(as.Date(obstacle$Date)) #adding days column
View(obstacle)


#Data Cleaning

summary(speed)
summary(obstacle)

speed <- speed[ -c(3,4,5,6,7,8,10,13,14,16) ]
obstacle <- obstacle[ -c(3,4,5,7,9,10,11,14,15,16,17) ]
View(speed)
View(obstacle)

#Mean Data Imputation on Speed Data
which(is.na(speed))

library(imputeTS)
speed <- na_mean(speed)

which(is.na(speed))

#Replacing NULL with 0 on Obstacle data

which(is.na(obstacle))
obstacle$Delay[is.na(obstacle$Delay)] <- 0 
which(is.na(obstacle))


#Stratified Data Sampling 


set.seed(101)
sample_date_speed <- speed %>% group_by(Date) %>% sample_n(30)

#Statistical Analysis (Finding highest average speed)

new_speed <- sample_date_speed %>% group_by(Date, Time, Day) %>% summarise_each(funs(mean))
View(new_speed)

#Stats for each day of the week.
library(psych)
describeBy(sample_date_speed, group="Day")



daywise <- sample_date_speed[ c(9,5,8) ]
daywise <- daywise %>% group_by(Day) %>% summarise_each(funs(mean, sd))
View(daywise)

#Plotting Values:

plot_speed <- sample_date_speed[ c(9,5,8) ]
plot_speed$Day <- as.factor(plot_speed$Day)
View(plot_speed)


nbplot<- ggplot(plot_speed, aes(x = Day, y = `Primary Avg Speed`, fill = `Primary Avg Speed`))+
         stat_boxplot(geom = "errorbar", width = 0.25) + geom_boxplot() + labs(title = "Northbound Avg Speed per Day Box Plot") 
sbplot<- ggplot(plot_speed, aes(x = Day, y = `Secondary Avg Speed`, fill = `Secondary Avg Speed`)) + 
         stat_boxplot(geom = "errorbar", width = 0.25) + geom_boxplot() + labs(title = "Southbound Avg Speed per Day Box Plot")
ggarrange(nbplot, sbplot, ncol=2, nrow=1)



#Analyzing Obstacle Dataset

View(obstacle)

obstacle$Severity <- as.factor(obstacle$Severity)
obstacle$EventType <- as.factor(obstacle$EventType)

day_obstacle <- obstacle[ c(8,3,5,7) ]
day_obstacle <- day_obstacle %>% group_by(Day) %>% sample_n(60)
View(day_obstacle)


new_severity<- day_obstacle %>% group_by(Day) %>% count(Severity)
new_event <- day_obstacle %>% group_by(Day) %>% count(EventType)
new_closure <- day_obstacle %>% group_by(Day) %>% count(FullClosure)


View(new_severity)
View(new_event)
View(new_closure)
