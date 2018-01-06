library(SDSFoundations)
post <- PostSurvey
View(post)
lower <- post$classification
happy <- post$happy
underclass_happy <- post$happy[post$classification=='Freshman'|post$classification=='Sophomore']
upperclass_happy <- post$happy[post$classification=='Junior'|post$classification=='Senior']
hist(underclass_happy, xlab='Underclassman Happiness', main='Percent of Time Happy')
hist(upperclass_happy, xlab='Upperclassman Happiness', main='Percent of Time Happy')
t.test(underclass_happy, upperclass_happy)

diffhappy <- post$happy - post$post_happy
mean(diffhappy)
t.test(diffhappy)
hist(diffhappy)

diffhomeworktime <- post$hw_hours_college-post$hw_hours_HS
hw_college <- post$hw_hours_college
hw_HS <- post$hw_hours_HS
mean(diffhomeworktime)
t.test(post$hw_hours_college, post$hw_hours_HS, paired=T)

satsleepgreek <- post$sleep_Sat[post$greek=='yes']
satsleepnogreek <- post$sleep_Sat[post$greek=='no'] 
sleepdiff = mean(satsleepnogreek)-mean(satsleepgreek)
t.test(satsleepgreek, satsleepnogreek, alternative='less')

hist(diffhappy)
hist(satsleepgreek)
hist(satsleepnogreek)
hist(diffhomeworktime)

post$difference <- post$hw_hours_college-post$hw_hours_HS
postnursingdiff <- post$difference[post$major=='Nursing']
postbiodiff <- post$difference[post$major=='Biology']

hist(postnursingdiff)
hist(postbiodiff)

t.test(postnursingdiff, postbiodiff)

diffs <- c(4.8,.7,-2.1,6.1,.5,-.1,5.3,5.5,3.1,-.6,7.8,3.7,2.5,-.8,5.8,6.6)
mean(diffs)

leftbrain <- c(16.3,4.8,10.7,14.0,15.7,9.9,29.3,20.4,15.7,7.6,16.2,14.7,15.0,8.4,23.3,17.7)
rightbrain <- c(11.5,3.5,12.8,7.9,15.2,9.8,24.0,14.9,12.6,8.2,8.4,11.0,12.5,9.2,17.5,11.1)

truediffs <- leftbrain-rightbrain
mean(truediffs)
sd(truediffs)

t.test(leftbrain,rightbrain, paired=T, alternative = 'greater')
t.test(truediffs, alternative = 'greater')
t.test(truediffs)

sd(truediffs)
