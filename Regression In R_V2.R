Los_Angeles__CA_._County_PD_Incidents__2004.2014_ -> oldcrime
unique(oldcrime$CRIME_CATEGORY_DESCRIPTION)

#format time data
require("lubridate")

strptime(oldcrime$CRIME_DATE, "%m/%d/%Y %I:%M:%S %p") -> oldcrime$CRIME_DATE
hour(oldcrime$CRIME_DATE) -> oldcrime$hour
minute(oldcrime$CRIME_DATE) -> oldcrime$min
month(oldcrime$CRIME_DATE) -> oldcrime$month
day(oldcrime$CRIME_DATE) -> oldcrime$day
year(oldcrime$CRIME_DATE) -> oldcrime$year

#remove fraud crimes
oldcrime <- oldcrime[!(oldcrime$rank == 10), ]

#calculate frequencies and rank by crime category
as.data.frame(table(oldcrime$CRIME_CATEGORY_DESCRIPTION)) -> oldcrimetype
oldcrimetype[order(oldcrimetype$Freq, decreasing = TRUE), ] -> oldcrimetype
oldcrimetype$rank <- 1:length(oldcrimetype$Freq)

#put ranks into main data frame
unique(oldcrime$CRIME_CATEGORY_DESCRIPTION) -> crimenames
for(i in 1:length(crimenames)){
  oldcrime$rank[oldcrime$CRIME_CATEGORY_DESCRIPTION == crimenames[i]] <- oldcrimetype$rank[oldcrimetype$Var1 == crimenames[i]]
}

#make dataframe including only top 10 crimes
topcrime <- oldcrime[oldcrime$rank <= 10, ]
head(topcrime)

#order top10 by rank
topcrime <- transform(topcrime, CRIME_CATEGORY_DESCRIPTION = reorder(CRIME_CATEGORY_DESCRIPTION, rank) ) 

#make some charts

library(ggplot2)
ggplot(topcrime, aes(x=hour, fill=CRIME_CATEGORY_DESCRIPTION)) +
  geom_histogram(binwidth=1)

#load and clean weather data
NOAA.Weather.Data.Jan.2005...Feb.2016 -> weather
as.numeric(weather$TEMP) -> weather$TEMP

#filter out rows with temp >= 5
weather[weather$TEMP >= 5, ] -> weather

#clean PCP01 data
weather$Precip <- as.numeric(as.character(weather$PCP01))
weather$Precip[is.na(weather$Precip)] <- 0

#convert from GMT to PST
as.POSIXct(strptime(weather$YR..MODAHRMN, "%Y%m%d%H%M"), tz = "GMT") -> weather$time
format(weather$time, tz = "America/Los_Angeles", usetz = TRUE) -> weather$time
hour(weather$time) -> weather$hour
day(weather$time) -> weather$day
month(weather$time) -> weather$month
year(weather$time) -> weather$year
minute(weather$time) -> weather$min

#plots
ggplot(weather, aes(x=hour, y = TEMP))+ 
  stat_summary(fun.y="mean", geom = "bar")

#first, clean up hours in weather data to align with crime data
weather$hour[weather$min == 0] <- hour(weather$time[weather$min == 0]) -1
paste(weather$year,sprintf("%02d",weather$month), sprintf("%02d",weather$day), sprintf("%02d",weather$hour), sep = "") -> weather$YMDH
paste(oldcrime$year,sprintf("%02d",oldcrime$month),sprintf("%02d",oldcrime$day),sprintf("%02d",oldcrime$hour), sep = "") -> oldcrime$YMDH

#lookup crime count
library(data.table)
oldcrime$count <- 1
DTC <- as.data.table(oldcrime[as.numeric(oldcrime$YMDH) >= 2005000000 & as.numeric(oldcrime$YMDH) <= 2014120100, ])
DTC[ , sum(count), by = "YMDH"] -> DTCC
names(DTCC)[2] <- "crimecount"

ggplot(DTC[ , sum(count), by = "year"], aes(x = year, y = V1))+
  stat_summary(fun.y="sum", geom = "bar")

#lookup temp
DTW <- as.data.table(weather[as.numeric(weather$YMDH) >= 2005000000 & as.numeric(weather$YMDH) <= 2014120100, ])
DTW[ , mean(TEMP), by = c("YMDH", "hour", "Precip")] -> DTWW
names(DTWW)[4] <- "Precip"
names(DTWW)[4] <- "tempavg"

#combine temp and crime count
regdata <- merge(DTCC , DTWW, by = 'YMDH')
ggplot(regdata, aes(x=hour, y =crimecount))+ 
  stat_summary(fun.y="mean", geom = "bar")
regdata$t <- 1:length(regdata$crimecount)

#look at autocovariance, see that 1, 24, and 48 hour lags are important
acf(regdata$crimecount, xlab = "Lag (hours)")

#create columns for lag = 24 and 48

require(zoo)
crimelag1 <- lag(as.ts(regdata$crimecount), -1, na.pad = TRUE)
crimelag24 <- lag(as.ts(regdata$crimecount), -24, na.pad = TRUE)
crimelag48 <- lag(as.ts(regdata$crimecount), -48, na.pad = TRUE)
data <- as.data.frame.ts(cbind(as.ts(regdata$crimecount), crimelag1,
                  crimelag24, crimelag48, as.ts(regdata$tempavg), as.ts(regdata$Precip)))
names(data) <- c("crime", "crimelag1", 
                 "crimelag24", "crimelag48", "tempavg", "Precip")
data$t <- 1:length(data$crime)
test <- data[data$t >= (max(data$t)-240-49) & data$t < (max(data$t)-49), ]
data <- data[data$t < (max(data$t)-240-49), ] #remove rows at end with crime = NA and month of Nov 2014

acf(data$crime)

#run regression
x <- lm(data$crime ~ data$crimelag1 + data$crimelag24 + data$crimelag48
        + data$tempavg + data$t + data$Precip)
summary(x)
anova(x)
acf(resid(x), lag.max = 193)

#look at test data, plot vs regression
coef <- coef(x)
yhat <- test$t
for(i in 1:length(test$crime)){
  yhat[i] <- coef[1] + coef[2]*test$crimelag1[i] + coef[3]*test$crimelag24[i] + coef[4]*test$crimelag48[i] + coef[5]*test$tempavg[i] + coef[6]*test$t[i] + coef[7]*test$Precip[i]
  }
plot(y = test$crime, x = test$t - length(data$crime), pch = 20, xlab = "Time (hours)", ylab = "Number of Crimes") #plot test data
lines( x = test$t - length(data$crime), y = yhat , col = "red") #plot reg data
abline(v = 230)

#calculate R squared for test data
1 - sum((test$crime - yhat)^2) / sum((test$crime - mean(test$crime))^2)

