#Required libraries
library(ggplot2)
library(stargazer)
library(AER)
library(knitr)
library(lmtest)
library(sandwich)
library(car)
library(GGally)
opts_chunk$set(warning = FALSE, message = FALSE)

#CHANGE TO WORKING DIRECTORY
setwd("~/datasci/W210/should-i-go-sandbox/")

# Load Datasets
data <- read.csv('Jan2015_TimeSeries.csv')

speed <- data[ data$REGION_ID == 1, ]$SPEED
plot.ts(speed)
plot.ts(speed[0:1000])

# Remove zero and irrationally high values
data_nozero <-data[data$SPEED>0,]  
data_nozero <-data_nozero[data_nozero$SPEED<46,] 

# Group timestamps into hours
data_nozero$HOUR <- substring(as.character(data_nozero$TIMESTAMP),1,2)
data_nozero$DAYHOUR <- paste(data_nozero$WEEKDAY,data_nozero$HOUR, sep='-')


# Remove midnight - 4am when most busses are in the yard
data_nozero <-data_nozero[data_nozero$HOUR>'04',]  


#Aggregate to get average speed
avgspeed <- aggregate(SPEED ~ WEEKDAY + HOUR, data = data_nozero, mean)

#dev.off()

#Let's start by looking at a heat map of days and hours compared to speed
ggplot(data = avgspeed, aes(x=WEEKDAY, y=HOUR, fill=SPEED)) + 
  geom_tile()+
  scale_fill_gradient2(low = "white", high = "steelblue", name="Avg Speed")+
  xlab('Weekday (Monday = 0)') +
  ylab('Hour') +
  ggtitle('Average Speed by Weekday and Hour')

#Monday - Friday:
#We can see that rush hour is really intense in the 7 and 8am hours, wantes a little from the 9am 
#through 2pm hours, and then ramps back up from the 3pm through 6pm hours.  


# Scatterplot with color-coded points
ggplot(data=data_nozero, aes(x=DAYHOUR, y=SPEED)) +
  geom_point(aes(colour = factor(HasGame))) +
  ggtitle(paste('Traffic Speeds at each Weekday and Hour')) +
  xlab('Weekday-Hour') +
  ylab('Average Speed') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# We need to show how regions differ
# Boxplot of all regions over whole month
ggplot(data = data_nozero, aes(x = REGION,y = SPEED)) +
  geom_boxplot(fill = 'steelblue', outlier.size = 0.2) +
  ggtitle('Speed by Region: Overall') +
  ylab('Speed') +
  xlab('Region') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Chicago Loop
data_Loop <-data_nozero[data_nozero$REGION=='Chicago Loop',]  
# scatterplot with color-coded points
ggplot(data=data_Loop, aes(x=DAYHOUR, y=SPEED)) +
  geom_point(aes(colour = factor(HasGame))) +
  ggtitle(paste('Chicago Loop Traffic at each Weekday and Hour')) +
  xlab('Weekday-Hour') +
  ylab('Traffic Speed') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



# Near North Loop
data_NN <-data_nozero[data_nozero$REGION=='Near North',]  
# scatterplot with color-coded points
ggplot(data=data_NN, aes(x=DAYHOUR, y=SPEED)) +
  geom_point(aes(colour = factor(HasGame))) +
  ggtitle(paste('Near North Traffic at each Weekday and Hour')) +
  xlab('Weekday-Hour') +
  ylab('Traffic Speed') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



# West Town Loop
data_West <-data_nozero[data_nozero$REGION=='West Town-Near West',]  
# scatterplot with color-coded points
ggplot(data=data_West, aes(x=DAYHOUR, y=SPEED)) +
  geom_point(aes(colour = factor(HasGame))) +
  ggtitle(paste('West Town Traffic at each Weekday and Hour')) +
  xlab('Weekday-Hour') +
  ylab('Traffic Speed') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# boxplot
ggplot(data = data_West, aes(x = HOUR,y = SPEED)) +
  geom_point(aes(colour = factor(HasGame), alpha = 0.05)) +
  geom_boxplot(alpha = 0.5, outlier.size = 0.2)+
  ggtitle('West Town Speed by Hour') +
  ylab('Speed') +
  xlab('Hour') 



# Seeing some odd values - too high, too low
HighData <- data_nozero[data_nozero$SPEED<10,]

