
## likelihood ratio test between 2019 and 2010

(304840.546524+289093.564878)-602159.143643 

-8225.032

# critial value
## 18*18=324
qchisq(p = .05, df = 324, lower.tail = FALSE)

#[1] 366.977

rm(list = ls())

Est2019<-read.csv("EsimateAgeEduRace2019BlackMale.csv")
Est2010<-read.csv("EsimateAgeEduRace2010BlackMale.csv")

EstDiff<-Est2019-Est2010

aa<-as.matrix(EstDiff)
sort(c(aa))

EstDiff[EstDiff<(-1.6)|EstDiff>1.4,]
