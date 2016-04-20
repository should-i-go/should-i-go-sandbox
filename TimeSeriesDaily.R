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
summary(data)

# Scatterplot with color-coded points - AVG SPEED
ggplot(data=data, aes(x=WEEKDAY, y=SPEED)) +
  geom_point(aes(colour = factor(GameType))) +
  geom_boxplot(alpha = 0.5, outlier.size = 0.2)+
  ggtitle(paste('Average Traffic Speeds at each Weekday')) +
  xlab('Weekday') +
  ylab('Average Speed') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Scatterplot with color-coded points - MIN SPEED
ggplot(data=data, aes(x=WEEKDAY, y=SPEED.MIN)) +
  geom_point(aes(colour = factor(GameType))) +
  ggtitle(paste('Minimum Traffic Speeds at each Weekday')) +
  xlab('Weekday') +
  ylab('Minimum Speed') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Use tapply to find conditional mean and variance depending on type of game
tapply(data$SPEED,data$GameType,mean) #shows average speed
tapply(data$SPEED.SD,data$GameType,mean) #shows variance of speed
tapply(data$SPEED.MIN,data$GameType,mean) #shows average speed
tapply(data$SPEED.MAX,data$GameType,mean) #shows variance of speed


# Show histograms of all three
data_nogame <- data[(data$GameType == ""),]
data_Bulls <- data[(data$GameType == "Bears"),]
data_Bears <- data[(data$GameType == "Bulls"),]
par(mfrow=c(3,1))
ggplot(data = data_nogame, aes(x = SPEED.MIN)) +
  geom_histogram(fill = 'steelblue', bins = 50) + 
  xlab('Min Speed') +
  ylab('# Segments') +
  ggtitle('Minimum speed distribution under no game') +
  theme_bw()
ggplot(data = data_Bulls, aes(x = SPEED.MIN)) +
  geom_histogram(fill = 'steelblue', bins = 50) + 
  xlab('Min Speed') +
  ylab('# Segments') +
  ggtitle('Minimum speed distribution under Bulls game') +
  theme_bw()
ggplot(data = data_Bears, aes(x = SPEED.MIN)) +
  geom_histogram(fill = 'steelblue', bins = 50) + 
  xlab('Min Speed') +
  ylab('# Segments') +
  ggtitle('Minimum speed distribution under Bears game') +
  theme_bw()


# Plot histograms through the week
speed_Mon <- data[(data$WEEKDAY == 0), c('SPEED')]
speed_Tue <- data[(data$WEEKDAY == 1), c('SPEED')]
speed_Wed <- data[(data$WEEKDAY == 2), c('SPEED')]
speed_Thu <- data[(data$WEEKDAY == 3), c('SPEED')]
speed_Fri <- data[(data$WEEKDAY == 4), c('SPEED')]
speed_Sat <- data[(data$WEEKDAY == 5), c('SPEED')]
speed_Sun <- data[(data$WEEKDAY == 6), c('SPEED')]

par(mfrow=c(2,4))
hist(speed_Mon, main="Monday",xlab="",xlim=c(10,40),breaks=40,col="lightblue")
abline(v = median(speed_Mon),col = "red",lwd = 1)
hist(speed_Tue, main="Tuesday",xlab="",xlim=c(10,40),breaks=40,col="lightblue")
abline(v = median(speed_Tue),col = "red",lwd = 1)
hist(speed_Wed, main="Wednesday",xlab="",xlim=c(10,40),breaks=40,col="lightblue")
abline(v = median(speed_Wed),col = "red",lwd = 1)
hist(speed_Thu, main="Thursday",xlab="",xlim=c(10,40),breaks=40,col="lightblue")
abline(v = median(speed_Thu),col = "red",lwd = 1)
hist(speed_Fri, main="Friday",xlab="",xlim=c(10,40),breaks=40,col="lightblue")
abline(v = median(speed_Fri),col = "red",lwd = 1)
hist(speed_Sat, main="Saturday",xlab="",xlim=c(10,40),breaks=40,col="lightblue")
abline(v = median(speed_Sat),col = "red",lwd = 1)
hist(speed_Sun, main="Sunday",xlab="",xlim=c(10,40),breaks=40,col="lightblue")
abline(v = median(speed_Sun),col = "red",lwd = 1)


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
write.csv(coeffSeg, file = "coeffSeg.csv")


# Lets investigate the relationship between event distance and impact
data$EventDistance[is.na(data$EventDistance)] <- 1000000   # Replace NA with a huge number
rmDist <- lm(SPEED ~ SEGMENTID + WEEKDAY + HasGame:EventDistance,data = data)
summary(rmDist)
coeffDist <- summary(rmDist)$coefficients


#-------------
#2016-04-19: CREATE DATA SERIES FOR JOHN

# Linear Regression
data$SEGMENTID <- as.factor(data$SEGMENTID)
data$WEEKDAY <- as.factor(data$WEEKDAY)
rmSeg <- lm(SPEED ~ SEGMENTID + WEEKDAY + GameType:SEGMENTID,data = data)
summary(rmSeg)
coeffSeg <- summary(rmSeg)$coefficients
write.csv(coeffSeg, file = "coeffSeg.csv")

# Use tapply to find most recent date for each weekday-game combination
data$DATE <- as.character(data$DATE)
tapply(data$DATE,list(data$WEEKDAY,data$GameType),max) 

# Create a data set with each segment and no Saturday Bulls Game
test_data_noBULLS_SAT <- data[(data$DATE == '2014-12-27'),]
test_data_noBULLS_SAT$HasGame <- 0
test_data_noBULLS_SAT$GameType <- factor("")
test_data_noBULLS_SAT$EventDistance <- 1000000   # Replace NA with a huge number
test_data_noBULLS_SAT$SegPrediction <- predict(rmSeg, newdata = test_data_noBULLS_SAT) 

# Create a data set with each segment and a Saturday Bulls Game
test_data_BULLS_SAT <- data[(data$DATE == '2014-12-27'),]
test_data_BULLS_SAT$SegPrediction <- predict(rmSeg, newdata = test_data_BULLS_SAT) 

# Create a data set with each segment and no Sunday Bears Game
test_data_noBEARS_SUN <- data[(data$DATE == '2014-12-21'),]
test_data_noBEARS_SUN$HasGame <- 0
test_data_noBEARS_SUN$GameType <- factor("")
test_data_noBEARS_SUN$EventDistance <- 1000000   # Replace NA with a huge number
test_data_noBEARS_SUN$SegPrediction <- predict(rmSeg, newdata = test_data_noBEARS_SUN) 

# Create a data set with each segment and a Sunday Bears Game
test_data_BEARS_SUN <- data[(data$DATE == '2014-12-21'),]
test_data_BEARS_SUN$SegPrediction <- predict(rmSeg, newdata = test_data_BEARS_SUN) 

# Create a data set with each segment and no Thursday Bulls Game
test_data_noBULLS_THU <- data[(data$DATE == '2014-12-25'),]
test_data_noBULLS_THU$HasGame <- 0
test_data_noBULLS_THU$GameType <- factor("")
test_data_noBULLS_THU$EventDistance <- 1000000   # Replace NA with a huge number
test_data_noBULLS_THU$SegPrediction <- predict(rmSeg, newdata = test_data_noBULLS_THU) 

# Create a data set with each segment and a Thursday Bulls Game
test_data_BULLS_THU <- data[(data$DATE == '2014-12-25'),]
test_data_BULLS_THU$SegPrediction <- predict(rmSeg, newdata = test_data_BULLS_THU) 

# Create a data set with each segment and no Thursday Bears Game
test_data_noBEARS_THU <- data[(data$DATE == '2014-12-04'),]
test_data_noBEARS_THU$HasGame <- 0
test_data_noBEARS_THU$GameType <- factor("")
test_data_noBEARS_THU$EventDistance <- 1000000   # Replace NA with a huge number
test_data_noBEARS_THU$SegPrediction <- predict(rmSeg, newdata = test_data_noBEARS_THU) 

# Create a data set with each segment and a Thursday Bears Game
test_data_BEARS_THU <- data[(data$DATE == '2014-12-04'),]
test_data_BEARS_THU$SegPrediction <- predict(rmSeg, newdata = test_data_BEARS_THU) 

# Reduce to necessary columns
test_data_noBEARS_THU <- test_data_noBEARS_THU[,c("DATE","SEGMENTID","SegPrediction","GameType")]
test_data_BEARS_THU <- test_data_BEARS_THU[,c("DATE","SEGMENTID","SegPrediction","GameType")]
test_data_noBULLS_THU <- test_data_noBULLS_THU[,c("DATE","SEGMENTID","SegPrediction","GameType")]
test_data_BULLS_THU <- test_data_BULLS_THU[,c("DATE","SEGMENTID","SegPrediction","GameType")]
test_data_noBEARS_SUN <- test_data_noBEARS_SUN[,c("DATE","SEGMENTID","SegPrediction","GameType")]
test_data_BEARS_SUN <- test_data_BEARS_SUN[,c("DATE","SEGMENTID","SegPrediction","GameType")]
test_data_noBULLS_SAT <- test_data_noBULLS_SAT[,c("DATE","SEGMENTID","SegPrediction","GameType")]
test_data_BULLS_SAT <- test_data_BULLS_SAT[,c("DATE","SEGMENTID","SegPrediction","GameType")]


# Export csv file for John
combineresults <- rbind(test_data_noBEARS_THU,test_data_BEARS_THU,
                        test_data_noBULLS_THU,test_data_BULLS_THU,
                        test_data_noBEARS_SUN,test_data_BEARS_SUN,
                        test_data_noBULLS_SAT,test_data_BULLS_SAT)
combineresults <- unique(combineresults)
colnames(combineresults)[3] <- "SPEED"
write.csv(combineresults, file = "DemoDataSeries.csv")





#-------------
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
  geom_histogram(fill = 'steelblue', bins = 100) + 
  xlim(-5, 5) +
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
  geom_histogram(fill = 'steelblue', bins = 100) + 
  xlim(-5, 5) +
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
  ylim(-4, 4) +
  ggtitle(paste('With Game: Difference in predictions vs Distance to event')) +
  ylab('Prediction Difference (mph)') +
  xlab('Distance to Event (m)') 

# Check correlation between event distance and difference in predictions
cor(test_data_game$EventDistance, test_data_game$PredictDelta)
cor(1/test_data_game$EventDistance, test_data_game$PredictDelta)
sd(test_data_game$SegPrediction)
sd(test_data_game$DistPrediction)
mean(test_data_game$SegPrediction)
mean(test_data_game$DistPrediction)
median(test_data_game$SegPrediction)
median(test_data_game$DistPrediction)

par(mfrow=c(1,2))
hist(test_data_game$DistPrediction, main="Distance Prediction",xlab="",breaks=40,col="lightblue")
abline(v = median(test_data_game$DistPrediction),col = "red",lwd = 1)
hist(test_data_game$SegPrediction, main="Segment Prediction",xlab="",breaks=40,col="lightblue")
abline(v = median(test_data_game$SegPrediction),col = "red",lwd = 1)





# So we know that the Distance model predicts less extreme results than the Segment Model.
test_data_game$Demean <- test_data_game$SegPrediction - mean(test_data_game$DistPrediction)

ggplot(data=test_data_game, aes(x=Demean, y=PredictDelta)) +
  geom_point(color = 'steelblue', alpha = 0.5) +
  geom_smooth(color = 'darkgreen',method=lm)+ 
  ylim(-2, 2) +
  xlim(-10, 10) +
  ggtitle(paste('Difference in predictions vs Deviation from mean')) +
  ylab('Difference between Segment and Distance predictions (mph)') +
  xlab('Segment deviation from mean (mph)') 

cor(test_data_game$Demean, test_data_game$PredictDelta)


# Let's remove the noise and just look at the impact from HasGame on projections
mergefile <- merge(test_data_game, test_data_nogame, by = "SEGMENTID")

mergeReduce <- mergefile[,c('SEGMENTID','EventDistance.x','DistPrediction.x','DistPrediction.y','SegPrediction.x','SegPrediction.y')]
mergeReduce$DistGameImpact = mergeReduce$DistPrediction.x - mergeReduce$DistPrediction.y
mergeReduce$SegGameImpact = mergeReduce$SegPrediction.x - mergeReduce$SegPrediction.y

ggplot(data=mergeReduce, aes(x=EventDistance.x, y=DistGameImpact)) +
  geom_point(color = 'steelblue', alpha = 0.5) +
  geom_point(color = 'red', aes(x=EventDistance.x, y=SegGameImpact), alpha = 0.5) +
  ggtitle(paste('With Game: Segment model predictions vs Distance model predictions')) +
  xlab('Distance to Event (m)') +
  ylab('Predictions') 


par(mfrow=c(1,2))
hist(mergeReduce$DistGameImpact, main="Distance Model Prediction",ylab="",xlab="Impact of a game (mph)",xlim=c(-3,3),breaks=20,col="lightblue")
abline(v = median(mergeReduce$DistGameImpact),col = "red",lwd = 1)
hist(mergeReduce$SegGameImpact, main="Segment Model Prediction",ylab="",xlab="Impact of a game (mph)",xlim=c(-3,3),breaks=20,col="lightblue")
abline(v = median(mergeReduce$SegGameImpact),col = "red",lwd = 1)


