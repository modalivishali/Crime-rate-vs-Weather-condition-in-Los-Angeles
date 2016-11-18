readdata = read.csv(file="~/Documents/Dropbox/Data Science/Project Data/Los_Angeles__CA_-_County_PD_Incidents__2004-2014_.csv") -> oldcrime
readdata = read.csv(file="~/Documents/Dropbox/Data Science/Weather/NOAA Weather Data Jan 2005 - Feb 2016") -> weather

unique(oldcrime$CRIME_CATEGORY_DESCRIPTION)

#format time data
require("lubridate")
strptime(oldcrime$CRIME_DATE, "%m/%d/%Y %I:%M:%S %p") -> oldcrime$CRIME_DATE
hour(oldcrime$CRIME_DATE) -> oldcrime$hour
minute(oldcrime$CRIME_DATE) -> oldcrime$min
month(oldcrime$CRIME_DATE) -> oldcrime$month
day(oldcrime$CRIME_DATE) -> oldcrime$day
year(oldcrime$CRIME_DATE) -> oldcrime$year

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
as.numeric(weather$TEMP) -> weather$TEMP

#filter out rows with temp >= 5
weather[weather$TEMP >= 5, ] -> weather

#clean PCP01 data
as.data.frame(unique(weather$PCP01)) -> uniqPCP01
names(uniqPCP01) <- "orig"
as.numeric(as.character(uniqPCP01$orig)) -> uniqPCP01$clean
uniqPCP01$clean == "NA"
for(i in 1:length(uniqPCP01)){
  weather$Precip[weather$PCP01 == uniqPCP01$orig[i]] <- uniqPCP01$clean[uniqPCP01$orig == uniqPCP01$orig[i]]
}


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
weather$hour[weather$min == 0] <- hour(weather$time[weather$min == 0]) + 1
paste(weather$year,sprintf("%02d",weather$month), sprintf("%02d",weather$day), sprintf("%02d",weather$hour), sep = "") -> weather$YMDH
paste(oldcrime$year,oldcrime$month,oldcrime$day,oldcrime$hour) -> oldcrime$YMDH

#bring temperature data and crime into new data frame
unique(oldcrime$YMDH) -> uniquetimes
for(i in 60000:length(uniquetimes)){
  oldcrime$temp[oldcrime$YMDH == uniquetimes[i]] <- mean(weather$TEMP[weather$YMDH == uniquetimes[i]])
}

