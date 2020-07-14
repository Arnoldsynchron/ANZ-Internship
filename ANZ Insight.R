#FIltering out the the purchase transactions
summary(ANZdata)
#Purchase transaction could be filtered out using the merchant id. (Purchase transaction with a mechant, is tagged with a Merchant ID refers to business transaction)
Purchase <- filter(ANZdata, merchant_id != '') #Purchase <- filter(ANZdata, !merchant_id =='')

#No value is returned which refers that, the purchase transactions 
c <- ANZdata$txn_description %>% unique()
Csmp <- Purchase %>% filter(txn_description %in% c)

#Visualizing the transaction amount(distribution)
#Shows a histogram that tells us nothing because of outliers

hist(Csmp$amount)
boxplot(Csmp$amount) 

#removing outliers
Nocsmp <- Csmp[!Csmp$amount %in% boxplot.stats(Csmp$amount)$out,]
             
ggplot(Nocsmp) + aes(x =amount)+ labs (x ='Transaction Amount') + 
    geom_histogram(bins = 10) + 
    labs (title = 'Histogram of Purchase Transaction Amount') + theme_wsj()

#hist(Csmp$amount[!Csmp$amount %in% boxplot.stats(Csmp$amount)$out],
     #xlab='Transaction Amount', main='Histogram of Purchase Transaction Amount')

hist(ANZdata$amount[!ANZdata$amount %in% boxplot.stats(ANZdata$amount)$out], main = '
     Histogram Plot of Overall Transaction', xlab = 'Transaction Amount')

#Visualizing Customer's monthly average volume

Custavgvol <- dplyr::group_by(ANZdata,customer_id) %>% dplyr::summarise(mon_avg_vol =  round(dplyr::n()/3,0))

hist(Custavgvol$mon_avg_vol,xlab = 'Average Transaction', main = '
    Histogram Average Monthly Transaction', ylab='Number of Customers')

#Visualizing the transaction volume over time(day or week)

Custavgweek <- ANZdata %>% dplyr::group_by(date,day) %>% dplyr::summarise(daily_avg = n()) %>% 
    group_by(day) %>% dplyr::summarise(avg_vol = mean(daily_avg, na.rm = T))

#Adjust the factors, with monday being the first day of the week
Custavgweek$day <- factor(Custavgweek$day,levels = c('Monday', 'Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
Custavgweek

ggplot(Custavgweek)+aes(x=day, y=avg_vol, color=day) +geom_line(aes(group=1)) +
    geom_point()  +ggtitle("Average daily transaction") + labs(x='Weekday', y='Transaction volume') +theme_economist()


#Visualizing transaction volume  by the hour, average transaction amount

custhourvol <- ANZdata %>% group_by(date,hour) %>% dplyr::summarise(hour_vol = n()) %>%
    group_by(hour) %>% dplyr::summarise(hour_avg_vol = mean(hour_vol, na.rm = TRUE))

ggplot(custhourvol, aes(hour, hour_avg_vol)) +theme_economist_white() + geom_line() + geom_point(aes(group=1)) +
    labs(title = 'Average Transaction Volume per Hour', x='Hour', y = 'Transaction Volume') + expand_limits(y=4)


#Distance between Merchants and Customers
#filtering out customer who stays outside australia
ANZ <- ANZloc %>% filter(!c_lat==(-573))

#using the geosphere library
ANZ$dist <- distHaversine(ANZ[,1:2], ANZ[,3:4])/1000

hist(ANZ$dist, xlab = 'Distance of Customers to Merchants(KM)', ylab='Frequency', main = 'Histogram Plot of 
     Customers to Merchants')

hist(ANZ$dist[ANZ$dist<100], xlab = 'Distance of Customers to Merchants(KM)', ylab='Frequency', main = 'Histogram Plot of 
     Customers to Merchants')


merch_dist <- function(id){
    ### This function takes the customer id and plots the customer's location, and location of all merchants ever traded 
    ###with
    cus_icon <- makeAwesomeIcon(icon='home', markerColor = 'green')
    l = subset(Csmp[,c('customer_id','merch_long','merch_lat')], customer_id==id)
    l <- l[c('merch_long','merch_lat')]
    
    cus_loc <- unique(subset(Csmp[,c('customer_id','long_lat')], customer_id==id))
    cus_loc <- cus_loc %>% separate('long_lat',c('c_long','c_lat'), sep= ' ')
    
    df_t = data.frame(longitude=as.numeric(l$merch_long), latitude=as.numeric(l$merch_lat))
    coordinates(df_t) <- ~longitude+latitude
    leaflet(df_t)%>% addMarkers() %>% addTiles() %>% addAwesomeMarkers(lng=as.numeric(cus_loc$c_long), lat = as.numeric(cus_loc$c_lat)
                                                                       , icon = cus_icon)
}

c <- ANZdata$customer_id[9]
merch_dist(id=c)
