setwd('C:\\SKDrive\\UT Dallas\\Courses\\Sem2Spring19\\Statistics\\Projects\\Data\\')


#Q1 :Find the distribution of how many times a pair users (i.e., address1 and address2) 1 - buys, 2 - sells a token with each other. Which distribution type fits these d?stributions best? Estimate population distribution parameters.


# Token 1 : bnb

#Token 1 networkbnbTX

library('readr')
library('dplyr')
library('fitdistrplus')


#read file
networkbnbTX<-read_delim('networkbnbTX.txt', delim = " ", col_names = F)


names?networkbnbTX) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')


## filter out tokenAmounts > the total circulating token amount
decimals <- 10^18

supply <- 141175490
networkbnbTXFiltered <- networkbnbTX %>% filter(tokenAmount < decimals * supply)

messa?e('Number of outliers ', nrow(networkbnbTX)-nrow(networkbnbTXFiltered))




trans.distribution <- networkbnbTXFiltered %>% group_by(fromID,toID) %>% summarise(n = n())


Transfreqtable<-table(trans.distribution$n)
Transfreq = as.data.frame(Transfreqtable) ?colnames(Transfreq) <- c("Number_Times_trasacted", "Frequency_Users") 
head(Transfreq)

summary(Transfreq$Frequency_Users)
head(Transfreq$Frequency_Users)



barplot(Transfreq$`Frequency_Users`, names.arg = Transfreq$`Number_Times_trasacted`, main = "Frequ?ncy of number of transactions vs. number of times transacted", xlab = "Number of Time transacted", ylab = "Frequency Of Number of transactions", xlim = c(0,20), ylim = c(0,300000))

t<-barplot(Transfreq$Frequency_Users/sum(Transfreq$Frequency_Users), ylab=?Frequency",xlim = c(0,25))



fit.pois.buy <- fitdist(Transfreq$`Frequency_Users`, 'pois')
fit.gamma.buy <- fitdist(Transfreq$`Frequency_Users`, 'gamma',lower = c(0, 0), start = list(scale = 1, shape = 1))

fit.lnorm.buy <- fitdist(Transfreq$`Frequency_Use?s`, 'lnorm')

fit.nbinom.buy <- fitdist(Transfreq$`Frequency_Users`, 'nbinom')
fit.norm.buy <- fitdist(Transfreq$`Frequency_Users`, 'norm')
fit.unif.buy <- fitdist(Transfreq$`Frequency_Users`, 'unif')
fit.weibull.buy <- fitdist(Transfreq$`Frequency_Users`,?'weibull',,method="mle")



plotdist(Transfreq$`Frequency_Users`, histo = TRUE, demp = TRUE)

descdist(Transfreq$`Frequency_Users`, boot=1000)
plot(fit.gamma.buy)

plot.legend <- c("Poisson", "gamma","log normal","binomial","normal","unif","weibull") 
dens?omp(list(fit.pois.buy, fit.gamma.buy,fit.lnorm.buy,fit.nbinom.buy,fit.norm.buy,fit.unif.buy,fit.weibull.buy), legendtext = plot.legend, xlim = c(0,100), ylim = c(0,0.2))


plot(fit.lnorm.buy)



#Token 2 tenxpay

library('readr')
library('dplyr')


#read f?le
networkbnbTX<-read_delim('networktenxpayTX.txt', delim = " ", col_names = F)


names(networkbnbTX) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')


## filter out tokenAmounts > the total circulating token amount
decimals <- 10^18

supply <- 141175490?networkbnbTXFiltered <- networkbnbTX %>% filter(tokenAmount < decimals * supply)

message('Number of outliers ', nrow(networkbnbTX)-nrow(networkbnbTXFiltered))




trans.distribution <- networkbnbTXFiltered %>% group_by(fromID,toID) %>% summarise(n = n())
?
Transfreqtable<-table(trans.distribution$n)
Transfreq = as.data.frame(Transfreqtable) 
colnames(Transfreq) <- c("Number_Times_trasacted", "Frequency_Users") 
head(Transfreq)

summary(Transfreq$Frequency_Users)
head(Transfreq$Frequency_Users)



barplot(Tr?nsfreq$`Frequency_Users`, names.arg = Transfreq$`Number_Times_trasacted`, main = "Frequency of number of transactions vs. number of times transacted", xlab = "Number of Time transacted", ylab = "Frequency Of Number of transactions", xlim = c(0,25), ylim = ?(0,160000))




library('fitdistrplus')


fit.pois.buy <- fitdist(Transfreq$`Frequency_Users`, 'pois')
fit.gamma.buy <- fitdist(Transfreq$`Frequency_Users`, 'gamma',lower = c(0, 0), start = list(scale = 1, shape = 1))

fit.log.buy <- fitdist(Transfreq$`Fre?uency_Users`, 'logis')

fit.nbinom.buy <- fitdist(Transfreq$`Frequency_Users`, 'nbinom')
fit.norm.buy <- fitdist(Transfreq$`Frequency_Users`, 'norm')
fit.unif.buy <- fitdist(Transfreq$`Frequency_Users`, 'unif')
fit.weibull.buy <- fitdist(Transfreq$`Frequen?y_Users`, 'weibull',,method="mle")





plot(fit.gamma.buy)

plot.legend <- c("Poisson", "gamma","log normal","binomial","normal","unif","weibull") 
denscomp(list(fit.pois.buy, fit.gamma.buy,fit.nbinom.buy,fit.norm.buy,fit.unif.buy,fit.weibull.buy), legend?ext = plot.legend, xlim = c(0,100), ylim = c(0,0.2))


fit.weibull.buy
fit.nbinom.buy



plot(density(Transfreq$`Frequency_Users`))
message("Mean of Frequency of number of transactions: ", mean(Transfreq$`Frequency_Users`))


message("SD of Frequency of nu?ber of transactions: ", sd(Transfreq$`Frequency_Users`))



gofstat(list(fit.weibull.buy, fit.nbinom.buy))




#Token 3 bat

library('readr')
library('dplyr')


#read file
networkbatTX<-read_delim('networkbatTX.txt', delim = " ", col_names = F)


names(net?orkbatTX) <- c('fromID', 'toID', 'unixTime', 'tokenAmount')


## filter out tokenAmounts > the total circulating token amount
decimals <- 10^18

supply <- 1253998401
networkbatTXFiltered <- networkbatTX %>% filter(tokenAmount < decimals * supply)

message(?Number of outliers ', nrow(networkbatTX)-nrow(networkbatTXFiltered))




trans.distribution <- networkbatTXFiltered %>% group_by(fromID,toID) %>% summarise(n = n())

summary(trans.distribution)

Transfreqtable<-table(trans.distribution$n)
Transfreq = as.da?a.frame(Transfreqtable) 
colnames(Transfreq) <- c("Number_Times_trasacted", "Frequency_Users") 
head(Transfreq)

summary(Transfreq$Frequency_Users)
head(Transfreq$Frequency_Users)



barplot(Transfreq$`Frequency_Users`, names.arg = Transfreq$`Number_Times_?rasacted`, main = "Frequency of number of transactions vs. number of times transacted", xlab = "Number of Time transacted", ylab = "Frequency Of Number of transactions", xlim = c(0,25), ylim = c(0,160000))




library('fitdistrplus')


fit.pois.buy <- fitd?st(Transfreq$`Frequency_Users`, 'pois')
fit.gamma.buy <- fitdist(Transfreq$`Frequency_Users`, 'gamma',lower = c(0, 0), start = list(scale = 1, shape = 1))

fit.log.buy <- fitdist(Transfreq$`Frequency_Users`, 'logis')

fit.nbinom.buy <- fitdist(Transfreq$`F?equency_Users`, 'nbinom')
fit.norm.buy <- fitdist(Transfreq$`Frequency_Users`, 'norm')
fit.unif.buy <- fitdist(Transfreq$`Frequency_Users`, 'unif')
fit.weibull.buy <- fitdist(Transfreq$`Frequency_Users`, 'weibull',,method="mle")





plot(fit.gamma.buy)

p?ot.legend <- c("Poisson", "gamma","log normal","binomial","normal","unif","weibull") 
denscomp(list(fit.pois.buy, fit.gamma.buy,fit.nbinom.buy,fit.norm.buy,fit.unif.buy,fit.weibull.buy), legendtext = plot.legend, xlim = c(0,100), ylim = c(0,0.2))


fit.wei?ull.buy
fit.nbinom.buy


%>% 
plot(density(Transfreq$`Frequency_Users`))
message("Mean of Frequency of number of transactions: ", mean(Transfreq$`Frequency_Users`))


message("SD of Frequency of number of transactions: ", sd(Transfreq$`Frequency_Users`))

?
gofstat(list(fit.weibull.buy, fit.nbinom.buy))
