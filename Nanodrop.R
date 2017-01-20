setwd("~/Iowa State University/Stats/GMF-vs-SA")
#Read in data
ExtrMeth<-read.csv("2015_GMFvsSA.csv")
head(ExtrMeth)
summary(ExtrMeth)

#What is the average of each filter type? 
aggregate(ExtrMeth[, 4:5], list(ExtrMeth$Filter), mean) 
#GMF Nanodrop average is 7.027; 
#SA Nanodrop average is 5.723

#How many values less than 0 do we have? 


#What does the data look like?
hist(ExtrMeth$Nanodrop)

#Create a dataframe without 0s
NoNegs<-ifelse(ExtrMeth$Nanodrop < 0, 0, ExtrMeth$Nanodrop) #Takes data and puts replaces anything less than 0 with 0
summary(NoNegs) #summary of aforementioned created vector
ExtrMeth$ND<-NoNegs #Adds vector to original dataset
summary(ExtrMeth) #summarizes full dataset

#Log the values
hist(log(ExtrMeth$Nanodrop))
ExtrMeth$LogDrops<- (log(ExtrMeth$Nanodrop))
summary(ExtrMeth)

hist(log(ExtrMeth$ND))
ExtrMeth$LogND<- (log(ExtrMeth$ND))
summary(ExtrMeth)
ExtrMeth$LogND2<-ifelse(ExtrMeth$LogND < 0, 0, ExtrMeth$LogND)

summary(ExtrMeth)
hist(ExtrMeth$LogND2)


#Run a T-test
t.test(Nanodrop~Filter, ExtrMeth) #p-val = 0.85, averages = 7.028, 5.732

t.test(ND~Filter, ExtrMeth) #p-val = 0.082, averages = 7.045, 5.731

t.test(LogDrops~Filter, ExtrMeth) #p-val = 0.34, averages = 1.520 and 1.606

t.test(LogND2~Filter, ExtrMeth) #p-val = 0.2813, averages = 1.501, 1.597
