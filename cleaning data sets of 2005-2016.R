
weather_2005_2016<- read.csv(file="NOAA Weather Data Jan 2005 - Feb 2016(0).csv")
weather_2005_2016
head(weather_2005_2016)
dim(weather_2005_2016) # [1] 120006   41 #
summary(weather_2005_2016)

#Grab colomns #
Date05_16<- weather_2005_2016$date
#processing date#

Date05_16<- as.character(Date05_16)
Y05_16<- substr(Date05_16, 1, 4)
Y05_16<- as.numeric(Y05_16)
M05_16<- substr(Date05_16, 5, 6)
M05_16<- as.numeric(M05_16)
D05_16<- substr(Date05_16, 7, 8)
D05_16<- as.numeric(D05_16)
H05_16<- substr(Date05_16, 9, 10)
H05_16<- as.numeric(H05_16)

VSB05_16<- weather_2005_2016$VSB
VSB05_16<- as.character(VSB05_16)
# transfer missing value("****") to NA
VSB05_16<- replace(VSB05_16, VSB05_16=="****", NA)
VSB05_16<- as.numeric(VSB05_16)

TEMP05_16<- weather_2005_2016$TEMP
TEMP05_16<- as.character(TEMP05_16)
# transfer missing value("****") to NA
TEMP05_16<- replace(TEMP05_16, TEMP05_16=="****", NA)
TEMP05_16<- as.numeric(TEMP05_16)

DEWP05_16<- weather_2005_2016$DEWP
# transfer missing value("****") to NA
DEWP05_16<- as.character(DEWP05_16)
DEWP05_16<- replace(DEWP05_16, DEWP05_16=="****",NA)
DEWP05_16<- as.numeric(DEWP05_16)

Y05_16<- as.factor(Y05_16)
M05_16<- as.factor(M05_16)
D05_16<- as.factor(D05_16)
H05_16<- as.factor(H05_16)
#create a dataframe with all useful columns
Weather05_16 <- data.frame(Y05_16,M05_16, D05_16, H05_16,VSB05_16,TEMP05_16, DEWP05_16)
head(Weather05_16)
Weather05_16  


#data for year 2005#
wea05 <- Weather05_16[which(Weather05_16$Y05_16==2005),]
head(wea05)
summary(wea05)
dim(wea05) # [1] 10567     7 #

# remove all missing value of weather data for 2005
wea05_2<- wea05[complete.cases(wea05),]
dim(wea05_2) # [1] 10200     7