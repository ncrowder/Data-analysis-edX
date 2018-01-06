library(SDSFoundations)
city <- AustinCityLimits
View(city)

# store a table of counts of the grammy winners
gtab <- table(city$Grammy)
# turn that table of counts into a table of proportions
prop.table(gtab)
# store a table of 2 categorical variable counts
gtab2 <- table(city$Grammy,city$Gender)
# turn THAT into a table of proportions, where the option 1 signifies that the row adds up to 100%
# option 2 would make the column add to 100% and no option makes all the cells add up to 100%
prop.table(gtab2,1)
#plot both of these if option 'beside' is omitted then the plot is "stacked"
barplot(gtab2,legend=T,main='Gender by Grammy Winner', ylab='Count', xlab='Gender',beside=T)
barplot(prop.table(gtab2,2))

#setting the claimed proportions the sample will be compared against
claimp <- c(2/3,1/3)
#looking at the expected values for each cell to see if sample is large enough 
# (>5 for each cell in order to meet conditions)
chisq.test(gtab,p=claimp)$expected
#running the test (null hypothesis is that sample matches proposed proportions)
chisq.test(gtab,p=claimp)

#table of grammys vs age
grammyage <- table(city$Grammy,city$Age.Group)

#checking conditions (met)
chisq.test(grammyage)$expected
#run the test (null is the two categorical variables are independent) R chooses more conservative
#correction = T which has to be changed
chisq.test(grammyage,correct=F)

#table of genders
gender_tab <-table(city$Gender)
#underlying assumed proportions of population
ExpGender <- c(.50, .50)
#checking conditions and running the test
chisq.test(gender_tab, p=ExpGender)$expected
chisq.test(gender_tab, p=ExpGender)

#table of gender vs top 10 hits
gender_top10 <-table(city$Gender, city$BB.wk.top10)
#table of proportions where top 10 hits add to 100%
prop.table(gender_top10,2)
#checking conditions and running the test
chisq.test(gender_top10, correct=FALSE)$expected
chisq.test(gender_top10, correct=FALSE)

genre_tab <- table(city$Genre)
claimp <- c(1/4,1/4,1/4,1/4)
chisq.test(genre_tab,p=claimp)$expected
chisq.test(genre_tab,p=claimp)

genre_twitter <- table(city$Genre,city$Twitter.100k)
prop.table(genre_twitter,1)
chisq.test(genre_twitter, correct=FALSE)$expected
chisq.test(genre_twitter)

#adding columns to the data set to indicate whether they had a recent hit or not
city$Recent[city$Year < 2012] <- 0 
city$Recent[city$Year >= 2012] <- 1

gender_recent <- table(city$Gender,city$Recent)
prop.table(gender_recent)
chisq.test(gender_recent, correct=FALSE)$expected
chisq.test(gender_recent, correct=FALSE)


