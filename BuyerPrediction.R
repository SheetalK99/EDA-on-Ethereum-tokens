setwd('C:\\SKDrive\\UT Dallas\\Courses\\Sem2Spring19\\Statistics\\Projects\\Data\\')

library('readr')
library('dplyr')


install.packages('anytime')
library(anytime)

networkbatTX<-read_delim('networkbatTX.txt', delim = " ", col_names = F)

names(networkbatTX) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')
networkbatTX<-transform(networkbatTX,Date=anydate(unixTime))

length(unique(networkbatTX$fromID))


buyers <- networkbatTX %>% group_by(fromID) %>% summarise(n = n())

message('Determination of k')

buyers$transRatio<-buyers$n/sum(buyers$n)
buyers<-buyers[order(-buyers$transRatio,na.last = TRUE),]

filtered_buyers<-head(buyers,5000)

sum(filtered_buyers$transRatio)

selected_buyers<-filtered_buyers$fromID


networkbatTX_filtered<-networkbatTX[networkbatTX$fromID %in% selected_buyers,] 




## add num transactions, num unique buyers, and mean token amount
bat.eng.features <- networkbatTX_filtered %>%
  group_by(Date) %>%
  summarise(nTransactions = n(),
            nUniqueBuyers = n_distinct(toID),
            nUniqueSellers = n_distinct(fromID),
            mTokenAmt = mean(tokenAmount)) %>%
  ungroup()


bat_prices<-read_delim('bat', delim = "\t", col_names = T)


bat_prices<- bat_prices %>%
  mutate(Pt = lag(Open, 1))


bat.eng.features$Day_of_week<- weekdays(bat.eng.features$Date)

bat_prices$Date<-as.Date(bat_prices$Date,format="%m / %d / %Y ")

bat_prices$MC <- as.numeric(gsub(',', '', bat_prices$`Market Cap`))
bat_prices<- subset(bat_prices, select= -c(`Market Cap`)))


bat_prices$Open <- as.numeric(bat_prices$Open)



head(bat_prices)
head(bat.eng.features)


merged.bat <- merge(bat.eng.features, bat_prices, by="Date")
head(merged.bat)

selected <- c('Low', 'Close', 'Volume', 'MC', 'nTransactions', 'nUniqueBuyers',
              'nUniqueSellers','mTokenAmt','Day_of_week')

s<- c('Low','Close')

print(cor(merged.bat[, s],merged.bat$Open)print(cor(bat.merged.2[, selected])[, 'Price.Return'])
      
      
      fit <- lm(Close ~  Open + Volume + nTransactions +
                  nUniqueBuyers  ,
                data = merged.bat)
      print(summary(fit))

      
      
merged.bat$fitted<-predict(fit)
merged.bat$resid <- fit$residuals

plot(residuals(fit))

plot(merged.bat$fitted,merged.bat$Close)
      
      
      
      
