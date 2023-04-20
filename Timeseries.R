library(zoo)
library(xts)
library(imputeTS)

#set working directory
path <- 'C:\Users\Mamun\Documents\MACHINE_LEARN'
setwd(path)
getwd()

#read in data
a <- read.csv('comal.csv')
head(b)
a$date <- as.Date(a$date)

#Extract begin and end
bgn <- as.Date(a$date[1],format='%Y/%m/%d')
end <- as.Date(a$date[length(a$date)],format='%Y/%m/%d')
datex <- seq.Date(bgn,end,"day")
pdatex <- as.Date(a$date,format='%Y/%m/%d')

#Check to see if there are missing values
theo <- length(datex)
actu <- length(a$date)
if(theo > actu)print("missing value")

#create a zoo object
Dis.zoo <- zoo(a$Discharge,pdatex)
dumc.zoo <-zoo(,datex) #Dummy dataset with time alone
Dis.zoom <- merge(dumc.zoo,Dis.zoo)
plot(Dis.zoom,xlab='Year',ylab='Discharge(cfs)') #see if there are any visible periods of missing records
summary(Dis.zoom) #check how many NAs

#interpolate for missing discharge values
Dis.ts <- as.ts(Dis.zoom) #convert to ts object of base R
Dis.tsf <- na_kalman(Dc.ts,model= "StructTS") #perform imputation
Dis.zoof <- zoo(Dis.tsf,datex) #convert back to
plot(Dis.zoof,xlab='Year',ylab='Discharge(cfs)') #see if there are any visible periods of missing records
summary(Dis.zoof)

#perform 10-day moving average
Dc10d <- rollmean(Dis.zoof,10,align='right')
plot(Dis10d,xlab='Year',ylab='Discharge(cfs)') #see if there are any visible periods of missing records
summary(Dis10d)

#aggregate to monthly values using mean
Dismon <- apply.monthly(as.xts(Dis.zoof),mean) #function in xts
plot(Dismon,xlab='Year',ylab='Discharge(cfs)') #see if there are any visible periods of missing records
summary(Dismon)

#calculate autocorrelation 
acf(Dis.zoof,lag.max=NULL,main='ACF for Comal', type = c('correlation'))
pacf(Dis.zoof,main='P.Auto.Cor.Fun Comal',type="o")
