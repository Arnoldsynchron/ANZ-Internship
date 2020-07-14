#What the data means for them and their clients.
#linking different data to take action?
#EDA to discover insight and transaction volume
#Build predictive model with Decision tree

#1. Quality data checks, 2. Insight from data - High, 3. Segment data by time and visualize it
#Correction

#Load packages and libraries
library(readxl);library(plyr);library(lubridate); library(stringr);library(tidyverse);require(modelr);require(sp);require(leaflet);require(geosphere);require(knitr);require(rpart)

ANZdata <- readxl::read_excel('C:/Users/user/Downloads/ANZ synthesised transaction dataset.xlsx')

#structure of the data
str(ANZdata)
head(ANZdata)
View(ANZdata)
#summary of the data
summary(ANZdata)

#changing format of the date column from long to short
#intialize column names
colnames(ANZdata)
ANZdata$date <- as.Date(ANZdata$date,format='%d/%m/%y')
head(ANZdata$date)

#dataset contains 91 unique records for the days. instead of 92 days 
length(unique(ANZdata$date))

#create a sequence from the first day to the last day of the transaction
daterange <- seq(min(ANZdata$date),max(ANZdata$date),1)
head(daterange); length(unique(daterange))

#To find the missing date
# %in% refers to value matching, ! is a logical. it indicates the negation (NOT)
help('%in%'); help('!')

daterange[!daterange %in% ANZdata$date]


#Derive hour data and weekday data of each transaction from extraction
class(ANZdata$extraction)
#ANZdata$extraction <- as.character(ANZdata$extraction), extract hour from the ANZdata$extraction
ANZdata$hour <- substr(ANZdata$extraction, 12, 19)

ANZdata$hour <- hour(as.POSIXct(ANZdata$hour,format='%H:%M:%S'))

ANZdata$day <- lubridate::wday(ANZdata$date, label=T, abbr = F) #ANZdata$day =weekdays(ANZdata$date, abbreviate = T); head(y)

#confrim the one-to-one link of account_id and customer_id
#Check that an account id is associated with a customer id, and that is associated with that particular account/customer

Acc_Cid=ANZdata %>% select(account,customer_id) %>% unique() %>% nrow()

#Extracting the location of customers and merchants

ANZloc <-  ANZdata[c('long_lat','merchant_long_lat')]
View(ANZloc)

ANZloc <- ANZloc%>% separate('long_lat',c('c_long','c_lat'), sep = ' ')
ANZloc <- ANZloc%>%separate('merchant_long_lat', c('merch_long', 'merch_lat'), sep=' ')
ANZloc <- data.frame(sapply(ANZloc, as.numeric))
ANZdata <- cbind(ANZdata,ANZloc)

#Range of customer location
#filtering is similar to subsetting but NA rows are not dropped

ANZ_temp <- filter(ANZdata,!(c_long>113 & c_long<154 &c_lat>(-44) &c_lat<(-10)))
length(unique(ANZ_temp$customer_id))

#Check the distribution of missing values

apply(ANZdata, 2, function(x){sum(is.na(x)|x=='')})

#check unique values
ANZdata %>% apply(2,function(x){length(unique(x))})

