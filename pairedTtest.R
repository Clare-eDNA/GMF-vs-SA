#Running a paired two-tailed t-test
PTT<- read.csv("PairedTTest.csv")
head(PTT)
PTT$Diff=PTT$GMF-PTT$SA
head(PTT)

#Remove all NA columns

t.test(PTT$GMF, PTT$SA, paired=TRUE)
#t = 1.7984, df = 141, p-value = 0.07425, paired t-test = not significant