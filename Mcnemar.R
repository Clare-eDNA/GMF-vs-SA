#Temp Data to practice McNemar's Test
#aka Chi Square for turtles

#Read in the data
TD <- read.csv("TurtData.csv") 
head(TD)
summary(TD)

library(dplyr)
#Separate out GMF/SA and 0 or 1

tbl<-TD %>% count(Extraction, Amp) #Creates a table
tbl

#Put into a 2x2 Matrix
x3<- matrix(c(70, 50, 97,23), byrow = TRUE, 2, 2)
x3

#Do a Mcnemar test
mcnemar.test(x3, y = NULL, correct = TRUE)
#McNemar's chi-squared = 14.395, df = 1, p-value = 0.0001482


#Chi sq for funsizes
chisq.test(x3)
#X-squared = 13.308, df = 1, p-value = 0.0002643
#===========================================#
              #Without Pond A
#===========================================#

TD2 <- TD[!TD$Pond == "Pond A",]
TD2

tbl2<-TD2 %>% count(Extraction, Amp)
tbl2
#put into a matrix
x4<- matrix(c(51, 39, 72, 18), byrow = TRUE, 2, 2)
#run McNemar's and chi sq
mcnemar.test(x4, y = NULL, correct = TRUE)
chisq.test(x4)

#Pond A stats
TD3 <- TD[TD$Pond == "Pond A",]

tbl3<-TD3 %>% count(Extraction, Amp)
tbl3
x5<- matrix(c(19, 11, 25, 5), byrow = TRUE, 2, 2)
#Mcnemar and Chisq on just Pond A... 
mcnemar.test(x4, y = NULL, correct = TRUE)
chisq.test(x4)


#============================================#
            #Average of Parameters
#============================================#


#Average of Ponds
aggregate(TD$Amp, list(TD$Pond), mean)
#Average of Extractions
aggregate(TD$Amp, list(TD$Extraction), mean)
