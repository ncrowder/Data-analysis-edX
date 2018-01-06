library(SDSFoundations)
bull <- BullRiders
view(bull)
View(bull)
USA <- bull[bull$Country=="USA",]
mean(USA$Weight)
sd(USA$Weight)
hist(USA$Weight,main='Histogram of US Bull Rider Weights',xlab='Weight(lbs)')
t.test(USA$Weight,mu=190)
Events <- bull[bull$Events14>=5,]
View(Events)
mean(Events$RidePer14)
sd(Events$RidePer14)
t.test(Events$RidePer14, mu=.5)

earnings_per<-bull$Earnings12
hist(earnings_per)
logearnings_per <- log(earnings_per)
hist(logearnings_per)
mean(logearnings_per)
logearnings_per
earnings_per<-bull$Earnings12>0
earnings_per
earnings_per<-bull[bull$Earnings12>0,]
mean(log(earnings_per$Earnings12))
bull$logper<-log(bull$Earnings12/bull$Events12)
mean(bull$logper,na.rm=TRUE)
sd(bull$logper, na.rm=TRUE)
length(which(!is.na(bull$logper)))

weights<-c(29.4,29,28.4,28.8,28.9,29.3,28.5,28.2)
mean(weights)
sd(weights)
