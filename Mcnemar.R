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

# ------- Actual McNemar's ------------ #

Test<- read.csv("McNemarData.csv")
Test$Date..summer..15.<-NULL
table1<-table(Test)

table1
#SA0 GMF0 = 59 
15+12+18+14
#SA0 GMF1 = 34
9+11+6+8
#SA1 GMF0 = 10
3+2+2+3
#SA1 GMF1 = 13
2+4+3+4
x5<- matrix(c(59, 34, 10, 13), byrow = TRUE, 2, 2)
mcnemar.test(x5,y = NULL, correct = TRUE)

#Without Pond A
#SA0 GMF0 = 44 
12+18+14
#SA0 GMF1 = 25
11+6+8
#SA1 GMF0 = 7
2+2+3
#SA1 GMF1 = 11
4+3+4
x6<-matrix(c(44, 23, 7, 11), byrow = TRUE, 2, 2)
mcnemar.test(x6)
chisq.test(x6)
