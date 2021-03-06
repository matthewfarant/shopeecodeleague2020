#Load packages
library(tidyverse)
library(bizdays)
#Load & Inspect
order<-read.csv(file.choose(),header=TRUE)
View(order)
str(order)
#Change orderid format into non-scientific format
orderid<-c()
for(i in 1:nrow(order)){
  orderid[i]<-format(order$orderid[i],scientific=F)
}
order$orderid<-orderid

#Change UNIX date into regular date
order$pick<-as.Date(as.POSIXct(order$pick, origin="1970-01-01"))
order$X1st_deliver_attempt<-as.Date(as.POSIXct(order$X1st_deliver_attempt, origin="1970-01-01"))
order$X2nd_deliver_attempt<-as.Date(as.POSIXct(order$X2nd_deliver_attempt, origin="1970-01-01"))
#Extract day of week from dates
order<-add_column(order,pickday=weekdays(order$pick),.after='pick')
order<-add_column(order,X1day=weekdays(order$X1st_deliver_attempt),.after='X1st_deliver_attempt')
order<-add_column(order,X2day=weekdays(order$X2nd_deliver_attempt),.after='X2nd_deliver_attempt')

#Extract region from address
#buyer
buyerloc<-c()
for(i in 1:nrow(order)){
  if(grepl("metro manila",order$buyeraddress[i],ignore.case=TRUE)){
    buyerloc[i]<-'MM'
  }else if(grepl("luzon",order$buyeraddress[i],ignore.case=TRUE)){
    buyerloc[i]<-'LZ'
  }else if(grepl("visayaz",order$buyeraddress[i],ignore.case=TRUE)){
    buyerloc[i]<-'VZ'
  }else if(grepl("mindanao",order$buyeraddress[i],ignore.case=TRUE)){
    buyerloc[i]<-'MD'
  }else{next}
}
order$buyeraddress<-buyerloc
order$buyeraddress<-replace_na(order$buyeraddress,'MM')
#seller
sellerloc<-c()
for(i in 1:nrow(order)){
  if(grepl("metro manila",order$selleraddress[i],ignore.case=TRUE)){
    sellerloc[i]<-'MM'
  }else if(grepl("luzon",order$selleraddress[i],ignore.case=TRUE)){
    sellerloc[i]<-'LZ'
  }else if(grepl("visayaz",order$selleraddress[i],ignore.case=TRUE)){
    sellerloc[i]<-'VZ'
  }else if(grepl("mindanao",order$selleraddress[i],ignore.case=TRUE)){
    sellerloc[i]<-'MD'
  }else{next}
}
order$selleraddress<-sellerloc
table(order$selleraddress)
table(order$buyeraddress)
#SLA standard for on time delivery
SLA<-c()
for(i in 1:nrow(order)){
  if((order$buyeraddress[i] %in% c('LZ') & order$selleraddress[i] %in% c('MM','LZ')) |(
           order$buyeraddress[i] %in% c('MM') & order$selleraddress[i] %in% c('LZ'))){
    SLA[i]<-5
  }else if((order$buyeraddress[i] %in% c('VZ','MD') & order$selleraddress[i] %in% c('MM','LZ','VZ','MD'))|(
           order$buyeraddress[i] %in% c('MM','LZ') & order$selleraddress[i] %in% c('VZ','MD'))){
    SLA[i]<-7
  }else {
    SLA[i]<-3
  }
}
order$SLA<-SLA

#delivery time for first and second attempt
order$firstattempt<-bizdays(order$pick,order$X1st_deliver_attempt)-1
order$secondattempt<-bizdays(order$X1st_deliver_attempt,order$X2nd_deliver_attempt)-1

#holidays
passholiday<-c()
date1=as.Date('2020-03-08')
date2=as.Date('2020-03-25')
date3=as.Date('2020-03-30')
date4=as.Date('2020-03-31')
for(i in 1:nrow(order)){
  if(date1 %in% seq(order$pick[i],order$X1st_deliver_attempt[i], by="days")
     |date2 %in% seq(order$pick[i],order$X1st_deliver_attempt[i], by="days")
     |date3 %in% seq(order$pick[i],order$X1st_deliver_attempt[i], by="days")
     |date4 %in% seq(order$pick[i],order$X1st_deliver_attempt[i], by="days")){
    passholiday[i]<-order$firstattempt[i]-1
  }else {passholiday[i]<-order$firstattempt[i]}
}
order$passholiday<-passholiday
table(order$passholiday!=order$firstattempt)
#is_late or not?
order$secondattempt<-replace_na(order$secondattempt,0)
is_late<-c()
for(i in 1:nrow(order)){
  if(order$secondattempt[i]>3){
    is_late[i]<-1
  }else if(order$passholiday[i]>order$SLA[i]){
    is_late[i]<-1
  }else if(order$passholiday[i]<=order$SLA[i]){
    is_late[i]<-0
  }
}
order$is_late<-is_late

#final submission file
final<-order %>% 
  select(orderid,is_late)
View(final)
str(final)
final$orderid<-as.integer(final$orderid)

#into csv
write.csv(final,'final6.csv',row.names = FALSE)
