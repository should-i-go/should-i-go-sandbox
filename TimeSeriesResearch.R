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
setwd("~/Documents/MIDS/210Capstone/should-i-go-sandbox/")

# Load Datasets
data <- read.csv('Jan2015_TimeSeries.csv')

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
  geom_point(aes(color = factor(HasGame), alpha = 0.05)) +
  geom_boxplot(alpha = 0.5, outlier.size = 0.2)+
  ggtitle('West Town Speed by Hour') +
  ylab('Speed') +
  xlab('Hour') 


# Seeing some odd values - too high, too low
HighData <- data_nozero[data_nozero$SPEED<10,]


data_West <-data_nozero[data_nozero$REGION=='West Town-Near West',]  
summary(data_West)
str(data_West)
head(data_West)
tail(data_West)

# Sort data
data_West <- data_West[order(data_West$datetime) , ]

# Plot data as time series object
ts_West <-  ts(data_West$SPEED)
plot(ts_West, type="l")
summary(ts_West)

# scatterplot with color-coded points
ggplot(data=data_West, aes(x=datetime, y=SPEED)) +
  geom_point(aes(colour = factor(HasGame))) +
  ggtitle(paste('West Town Traffic at each Weekday and Hour')) +
  xlab('Timestamp') +
  ylab('Traffic Speed') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



# NOW LET'S LOOK AT TIME SERIES
data_nozero$datetime <- format(as.POSIXct(paste(data_nozero$DATE, data_nozero$TIMESTAMP)), "%Y-%m-%d %H:%M:%S")
data_nozero$datetime <- strptime(data_nozero$datetime, "%Y-%m-%d %H:%M:%S")
data_nozero$HOUR <- as.factor(data_nozero$HOUR)
data_nozero$WEEKDAY <- as.factor(data_nozero$WEEKDAY)


# Sort data
data_nozero <- data_nozero[order(data_nozero$datetime) , ]


# Linear Regression Model
rm1 <- lm(SPEED ~ REGION + WEEKDAY + HOUR + REGION:WEEKDAY + REGION:HOUR,data = data_nozero)
summary(rm1)

rm2 <- lm(SPEED ~ REGION + WEEKDAY + HOUR + REGION:WEEKDAY,data = data_nozero)
summary(rm2)

rm3 <- lm(SPEED ~ REGION + WEEKDAY + HOUR,data = data_nozero)
summary(rm3)

rm4 <- lm(SPEED ~ REGION + WEEKDAY + HOUR + REGION:WEEKDAY + REGION:HOUR + HasGame,data = data_nozero)
summary(rm4)

rm5 <- lm(SPEED ~ REGION:WEEKDAY + REGION:HOUR + REGION:HasGame,data = data_nozero)
summary(rm5)


# Predict speed with and without game
test_data_nogame <- data_nozero[(data_nozero$DATE == "2015-01-05" & data_nozero$HOUR == '20' & data_nozero$TIMESTAMP=="20:00:00"), ]
test_data_nogame$HasGame <- 0
test_data_nogame$PredictedRate <- predict(rm1, newdata = test_data_nogame) 
test_data_nogame <- test_data_nogame[, c('datetime','REGION_ID','PredictedRate')]

test_data_game <- data_nozero[(data_nozero$DATE == "2015-01-05" & data_nozero$HOUR == '20' & data_nozero$TIMESTAMP=="20:00:00"), ]
test_data_game$HasGame <- 1
test_data_game$PredictedRate <- predict(rm5, newdata = test_data_game) 
test_data_game <- test_data_game[, c('datetime','REGION_ID','PredictedRate')]

write.csv(test_data_nogame, file = "test_data_nogame.csv")
write.csv(test_data_game, file = "test_data_game.csv")

