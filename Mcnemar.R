#Temp Data to practice McNemar's Test
#aka Chi Square for turtles

#Read in the data
TD <- read.csv("TempData.csv") 
head(TD)
summary(TD)

library(dplyr)
#Separate out GMF/SA and 0 or 1

tbl<-TD %>% count(Extraction, Amplification) #Creates a table
tbl

#Put into a 2x2 Matrix
x1<-cbind(70,50) #GMF (0,1)
x2<-cbind(53,7)  #SA  (0,1)

x3<- matrix(c(70, 50, 53,7), byrow = TRUE, 2, 2)
x3

#Do a Mcnemar test
mcnemar.test(x3, y = NULL, correct = TRUE)

#Chi sq for funsizes
chisq.test(x3)
