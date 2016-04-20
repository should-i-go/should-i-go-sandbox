# From https://github.com/twitter/AnomalyDetection
# One-time installation instructions
# install.packages("devtools")
# devtools::install_github("twitter/AnomalyDetection")

library(AnomalyDetection)
data <- read.csv('Jan2015_TimeSeries.csv')
data$HOUR <- substring(as.character(data$TIMESTAMP),1,2)
data.clean <- data[data$HOUR > '05', ]

timestamps = c()
for (i in 1:nrow(data.clean)) { timestamps[i] <- paste(data.clean$DATE[i], data.clean$TIMESTAMP[i]) }
data.clean$timestamp <- timestamps

region1 <- data.clean[ data.clean$REGION_ID == 1,  ]
region1.ad <- data.frame(timestamp = timestamps, count = region1$SPEED)

region1.res <- AnomalyDetectionTs(region1.ad, max_anoms=0.02, direction='neg', plot=TRUE)
region1.res$plot
