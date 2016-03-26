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
setwd("~/Documents/MIDS/210Capstone/")

# Load Datasets
data <- read.csv('df_segment_event_TimeSeries.csv')

# Scatterplot with color-coded points
ggplot(data=data, aes(x=WEEKDAY, y=SPEED)) +
  geom_point(aes(colour = factor(HasGame))) +
  ggtitle(paste('Traffic Speeds at each Weekday')) +
  xlab('Weekday') +
  ylab('Average Speed') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# Use tapply to find conditional mean and variance
tapply(data$SPEED,data$HasGame,mean) #shows average price for homes with and without fireplaces
tapply(data$SPEED,data$HasGame,sd) #shows variance of price for homes with and without fireplaces


# Let's try a linear regression to understand if there is any shock value from games
data$SEGMENTID <- as.factor(data$SEGMENTID)
data$WEEKDAY <- as.factor(data$WEEKDAY)

# Linear Regression Model
# Yay - finally the shock signal we expected! Statistically significant shocks.
rm1 <- lm(SPEED ~ SEGMENTID + WEEKDAY + HasGame,data = data)
summary(rm1)

# We see that certain segments are more impacted than others
rmSeg <- lm(SPEED ~ SEGMENTID + WEEKDAY + HasGame:SEGMENTID,data = data)
summary(rmSeg)
coeffSeg <- summary(rmSeg)$coefficients

# Lets investigate the relationship between event distance and impact
data$EventDistance[is.na(data$EventDistance)] <- 1000000   # Replace NA with a huge number
rmDist <- lm(SPEED ~ SEGMENTID + WEEKDAY + HasGame:EventDistance,data = data)
summary(rmDist)
coeffDist <- summary(rmDist)$coefficients


# Now lets compare segments' predicted rates under the segment model
# to segments' predicted impact under the distance model

# Let's create a data set with each segment and no game on WEEKDAY = 5
test_data_nogame <- data[(data$DATE == "2013-01-19"),]
test_data_nogame$HasGame <- 0
test_data_nogame$EventDistance <- 1000000   # Replace NA with a huge number
test_data_nogame$SegPrediction <- predict(rmSeg, newdata = test_data_nogame) 
test_data_nogame$DistPrediction <- predict(rmDist, newdata = test_data_nogame) 
test_data_nogame$PredictDelta <- test_data_nogame$SegPrediction - test_data_nogame$DistPrediction

# Let's compare the predictions in basic scatterplots and histograms
ggplot(data=test_data_nogame, aes(x=SegPrediction, y=DistPrediction)) +
  geom_point(color = 'steelblue', alpha = 0.5) +
  ggtitle(paste('No Game: Segment model predictions vs Distance model predictions')) +
  xlab('Segment prediction') +
  ylab('Distance prediction') 

ggplot(data = test_data_nogame, aes(x = PredictDelta)) +
  geom_histogram(fill = 'steelblue', bins = 50) +
  xlab('Prediction Difference (mph)') +
  ylab('# Segments') +
  ggtitle('No Game: Difference between Segment and Distance model predictions') +
  theme_bw()

# Check correlation
cor(test_data_nogame$SegPrediction, test_data_nogame$DistPrediction)




# Let's create a data set with each segment and a game on WEEKDAY = 5
test_data_game <- data[(data$HasGame == 1 & data$WEEKDAY == 5 & data$DATE == '2014-12-06'),]
test_data_game$SegPrediction <- predict(rmSeg, newdata = test_data_game) 
test_data_game$DistPrediction <- predict(rmDist, newdata = test_data_game) 
test_data_game$PredictDelta <- test_data_game$SegPrediction - test_data_game$DistPrediction

# Let's compare the predictions in basic scatterplots and histograms
ggplot(data=test_data_game, aes(x=SegPrediction, y=DistPrediction)) +
  geom_point(color = 'steelblue', alpha = 0.5) +
  ggtitle(paste('With Game: Segment model predictions vs Distance model predictions')) +
  xlab('Segment prediction') +
  ylab('Distance prediction') 

ggplot(data = test_data_game, aes(x = PredictDelta)) +
  geom_histogram(fill = 'steelblue', bins = 50) +
  xlab('Prediction Difference (mph)') +
  ylab('# Segments') +
  ggtitle('WithGame: Difference between Segment and Distance model predictions') +
  theme_bw()

# Check correlation
cor(test_data_game$SegPrediction, test_data_game$DistPrediction)

# Let's see if predictions differ more closer to or further from the event
ggplot(data=test_data_game, aes(x=EventDistance, y=SegPrediction)) +
  geom_point(color = 'steelblue', alpha = 0.5) +
  geom_point(color = 'red', aes(x=EventDistance, y=DistPrediction), alpha = 0.5) +
  ggtitle(paste('With Game: Segment model predictions vs Distance model predictions')) +
  xlab('Distance to Event (m)') +
  ylab('Predictions') 

ggplot(data=test_data_game, aes(x=EventDistance, y=PredictDelta)) +
  geom_point(color = 'steelblue', alpha = 0.5) +
  ggtitle(paste('With Game: Difference in predictions vs Distance to event')) +
  ylab('Prediction Difference (mph)') +
  xlab('Distance to Event (m)') 

# Check correlation between event distance and difference in predictions
cor(test_data_game$EventDistance, test_data_game$PredictDelta)
cor(1/test_data_game$EventDistance, test_data_game$PredictDelta)


