#Running a paired two-tailed t-test
PTT<- read.csv("PairedTTest.csv")
head(PTT)
PTT$Diff=PTT$GMF-PTT$SA
head(PTT)

########### Remove all NA columns ############

t.test(PTT$GMF, PTT$SA, paired=TRUE)
t.test(smurf$GMF, smurf$SA, paired=TRUE)
#t = 1.7984, df = 141, p-value = 0.07425, paired t-test = not significant

library(reshape2)
PTT <- na.omit(PTT)
PTT.m <- melt(PTT, id.vars=c("JDATE", "Pond"), measure.vars = c("GMF", "SA"))
m <- lm(value ~ Pond + variable*JDATE, data=PTT.m)
m.diff <- lm(Diff ~ Pond + JDATE, data=PTT)
summary(m)
summary(m.diff)
res <- residuals(m.diff)
par(mfrow=c(2,2))
acf(res[PTT$Pond=="A"])
acf(res[PTT$Pond=="B"])
acf(res[PTT$Pond=="C"])
acf(res[PTT$Pond=="D"])
par(mfrow=c(1,1))


library(ggplot2)

qplot(JDATE, value, geom="point", data=PTT.m, facets=variable~Pond)
qplot(value, geom="histogram", data=PTT.m, facets=variable~Pond)
#If we (1) replace the negative and low values with the lowest detected value for each method respectively and then log the values, we can compare methods and do a paired t-test that way... 
#We can also view histograms of the vaues for each pond by each method
smurf <- PTT
smurf$GMF <- pmax(smurf$GMF, 0.4)
smurf$SA  <- pmax(smurf$SA, 0.8)
smurf <- melt(smurf, id.vars=c("JDATE", "Pond"), measure.vars = c("GMF", "SA"))
qplot(log(value), geom="histogram", data=smurf, facets=variable~Pond, binwidth=1/3)
##Here's the t-test
t.test(value~variable, paired = TRUE, data=smurf)

## Summary stats in real
library(plyr)
ddply(smurf, .(variable), summarize,  Mean=mean(value), StDev=sd(value))
#Summary stats in log
ddply(smurf, .(variable), summarize,  Mean=mean(log(value)), StDev=sd(log(value)), Median = median(log(value)))

#####################################################################################

m1 <- glm(value~variable*JDATE, data=smurf)
summary(m1)
plot(value~variable*JDATE, data=smurf)
#significant effect of date, but no singifcant effect of an interaction
m2<- glm(value~variable*Pond, data=smurf)
summary(m2)
#Pond B is different than Pond A, and a potentially signficant interaction between pond and SA vs GMF extraction
m3<- glm(value~Pond*JDATE, data=smurf)
summary(m3)
####### But, if we look at the differences without adding .4 or .8 ######################

m4 <- glm(Diff~JDATE, data=PTT)
summary(m4)
plot(value~variable*JDATE, data=PTT)
#significant effect of date, but no singifcant effect of an interaction
m5<- glm(Diff~variable*Pond, data=PTT)
summary(m5)
#Pond B is different than Pond A, and a potentially signficant interaction between pond and SA vs GMF extraction
m6<- glm(Diff~Pond*JDATE, data=PTT)
summary(m6)

##################################################
####### Now, with adding 1.91 to everything #######
##################################################

head(PTT)
summary(PTT)
PTT$GMF1<- PTT$GMF+1.91
PTT$SA1<- PTT$SA+1.91
summary(PTT)

#Log the data
PTT$LogGMF<- log(PTT$GMF1)
PTT$LogSA<-log(PTT$SA1)
summary(PTT)

#Run a T-test on the data
t.test(PTT$LogGMF, PTT$LogSA, data=PTT, paired=TRUE) #Not a significant difference, p-val = 0.3

#Tried to see if there was an effect of date
#First reshaped the data
PTT2 <- melt(PTT, id.vars=c("JDATE", "Pond"), measure.vars = c("LogGMF", "LogSA"))
summary(PTT2)

#ran some gaussian glms
m7 <- glm(value~JDATE*variable, data=PTT2, family=gaussian) #Significant effect of time
summary(m7)
m8 <- glm(value~Pond*variable, data=PTT2, family=gaussian)
summary(m8)
m9 <- glm(value~Pond*JDATE, data=PTT2, family=gaussian)
summary(m9)

#But looking at differences
summary(PTT2)
summary(PTT)
PTT$Diff1<- (PTT$LogGMF-PTT$LogSA)
m10<- glm(Diff1~JDATE, data=PTT, family=gaussian) #No effect of date
summary(m10)
m11 <- glm(Diff1~Pond*JDATE, data=PTT, family=gaussian) #No effect of anything
summary(m11)
