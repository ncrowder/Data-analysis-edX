library(SDSFoundations)
film <- FilmData
View(film)

table(film$Rating)

# Calculate avg film budget of each group
aggregate(Budget~Rating,film,mean)

# Calculate sd of film budget within each group
aggregate(Budget~Rating,film,sd)

# Visualize the group means and variability
boxplot(film$Budget~film$Rating, main= "Film Budgets by Rating",
        ylab= "Budget", xlab= "MPAA Rating")

# Run ANOVA
modelbud <- aov(film$Budget~film$Rating)
summary(modelbud)

# Run post-hoc test if F statistic is significant
TukeyHSD(modelbud)


# Calculate avg IMDB score of each group
aggregate(IMDB~Rating,film,mean)

#Calculate sd of IMDB scores within each group
aggregate(IMDB~Rating,film,sd)

# Visualize the group means and variability
boxplot(film$IMDB~film$Rating, main= "IMDB Scores by Rating",
        ylab= "IMDB Score", xlab= "MPAA Rating")

# Run ANOVA
modelscore <- aov(film$IMDB~film$Rating)
summary(modelscore)

# Run post-hod text if F statistic is significant
TukeyHSD(modelscore)


#count films by each studio
table(film$Studio)
#each Studios average length in theatre
aggregate(Days~Studio, film, mean)
#run ANOVA comparing these
modelscore <- aov(film$Days~film$Studio)
summary(modelscore)
#Tukey
TukeyHSD(modelscore)


#Again with Earnings
aggregate(Pct.Dom~Studio, film, mean)
modelscore <- aov(film$Pct.Dom~film$Studio)
summary(modelscore)

#didn't work, why?
lowbudget <- film[film$Budget<100,]
medbudget <- film[100 <= film$Budget & film$Budget<150,] 
highbudget <- film[film$Budget>=150,]

#getting the row numbers for each condition
lb <- which(film$Budget<100)
mb <- which(100 <= film$Budget & film$Budget<150)
hb <- which(film$Budget>=150)

#didn't work to change, why?
film[lb,film$lmhb] <- "low-budget"
film[mb,film$lmhb] <- "medium-budget"
film[hb,film$lmhb] <- "high-budget"

#yes, finally
film$lmhb[lb] <- "low-budget"
film$lmhb[mb] <- "medium-budget"
film$lmhb[hb] <- "high-budget"
View(film)
table(film$lmhb)

#Averages of each category
aggregate(Pct.Dom~lmhb,film,mean)
#Boxplot of each (check assumptions)
boxplot(film$Pct.Dom~film$lmhb)

modelscore <- aov(film$Pct.Dom~film$lmhb)
summary(modelscore)

TukeyHSD(modelscore)

#3 groups of 6 officers from different precint's ticket counts for the week
section1 <- c(8,4,6,8,6,4)
section2 <- c(3,7,0,2,7,5)
section3 <- c(1,2,7,6,5,0)

total <- c(section1,section2,section3)

#MSE(total)
SSE_total <- sum((total-mean(total))^2)
MSE_total <- SSE_total/17
MSE_total
#MSE(between)
SSE_between <- 6*(mean(section1)-mean(total))^2+6*(mean(section2)-mean(total))^2+6*(mean(section3)-mean(total))^2
MSE_between <- SSE/2
MSE_between
#MSE(within)
SSE_within <- SSE_total-SSE_between
MSE_within <- SSE_within/15
MSE_within
#F-stat
MSE_between/MSE_within
#not in the critical region with alpha = .05 for F(2,15)