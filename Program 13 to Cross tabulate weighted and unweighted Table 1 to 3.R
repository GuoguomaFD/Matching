
setwd("XXX")
rm(list=ls())

library(xlsx)
library(dplyr)
library(tidyr)
library(stringr)


data<-read.xlsx("Matches Matrix Table Template.xlsx", sheetName = "2019 Raw Statistics Unweighted")
#data<-read.xlsx("Matches Matrix Table Template.xlsx", sheetName = "2019 Statistics Weighted")
#data<-read.xlsx("Matches Matrix Table Template.xlsx", sheetName = "2010 Raw Statistics Unweighted")
#data<-read.xlsx("Matches Matrix Table Template.xlsx", sheetName = "2010 Statistics Weighted")

## row 22 to Column V, the last row and column are total numbers
data<-data[1:21,1:22]


#a The number and percentage of interracial marriages; it would be great
# to also include the raw odds ratios of interracial marriage (derived
# from the simple raw count and weighted count of the 2x2 cross
# classification of White and non-White husbands and wives).

MatInterRace<-as.data.frame(matrix(,nrow = 2,ncol = 2))
names(MatInterRace)<-c("WifeWite","WifeNonwhite")
row.names(MatInterRace)<-c("HusbandWite","HusbandNonwhite")

MatInterRace[1,1]<-sum(as.numeric(as.matrix(data[4:9,5:10])))
MatInterRace[1,2]<-sum(as.numeric(as.matrix(data[4:9,11:22])))
MatInterRace[2,1]<-sum(as.numeric(as.matrix(data[10:21,5:10])))
MatInterRace[2,2]<-sum(as.numeric(as.matrix(data[10:21,11:22])))

## check
sum(as.numeric(as.matrix(data[4:21,5:22])))
sum(MatInterRace)
total<-sum(MatInterRace)

MatInterRace


MatInterRaceTable<-as.data.frame(matrix(,nrow = 3,ncol = 3))
names(MatInterRaceTable)<-c("Count","Percent","Odds Ratio")
row.names(MatInterRaceTable)<-c("Interracial marriages","Same Race marriages","Total marriages")

MatInterRaceTable[1,1]<-MatInterRace[1,2]+MatInterRace[2,1]
MatInterRaceTable[2,1]<-MatInterRace[1,1]+MatInterRace[2,2]
MatInterRaceTable[3,1]<-total

MatInterRaceTable[1,2]<-MatInterRaceTable[1,1]/total*100
MatInterRaceTable[2,2]<-MatInterRaceTable[2,1]/total*100
MatInterRaceTable[3,2]<-100

MatInterRaceTable[1,3]<-(MatInterRace[1,2]*MatInterRace[2,2])/(MatInterRace[1,1]*MatInterRace[2,1])
MatInterRaceTable[2,3]<-NA
MatInterRaceTable[3,3]<-NA

MatInterRaceTable

## 2019
## Unweighted
#                     Count   Percent Odds Ratio
#Interracial marriages  2092  11.49009  0.2451699
#Same Race marriages   16115  88.50991         NA
#Total marriages       18207 100.00000         NA

## 2019
## Weighted
#                      Count   Percent Odds Ratio
#Interracial marriages  439697  11.55471  0.3216364
#Same Race marriages   3365650  88.44529         NA
#Total marriages       3805347 100.00000         NA

## 2010
## Unweighted
#                     Count    Percent Odds Ratio
#Interracial marriages  1606   9.092453  0.2090578
#Same Race marriages   16057  90.907547         NA
#Total marriages       17663 100.000000         NA

# 2010
# Weighted
#                       Count    Percent Odds Ratio
#Interracial marriages  338646   9.211619   0.237836
#Same Race marriages   3337646  90.788381         NA
#Total marriages       3676292 100.000000         NA


#############################################
## Education 
# b) The number and percentage of educationally mixed marriages (and it
# would be great to see the raw and weighted count odds ratios as well).


MatInterEdu<-as.data.frame(matrix(,nrow = 2,ncol = 2))
names(MatInterEdu)<-c("WifeHighSchool","WifeCollege")
row.names(MatInterRace)<-c("HusbandHighSchool","HusbandCollege")

RowSelect<-which(data$NA..2%in%"High School")
ColSelect<-which(data[2,]%in%"High School")

RowH<-c(RowSelect[1]:(RowSelect[1]+2),RowSelect[2]:(RowSelect[2]+2),RowSelect[3]:(RowSelect[3]+2))
ColH<-c(ColSelect[1]:(ColSelect[1]+2),ColSelect[2]:(ColSelect[2]+2),ColSelect[3]:(ColSelect[3]+2))

RowSelectC<-which(data$NA..2%in%"College")
ColSelectC<-which(data[2,]%in%"College")

RowC<-c(RowSelectC[1]:(RowSelectC[1]+2),RowSelectC[2]:(RowSelectC[2]+2),RowSelectC[3]:(RowSelectC[3]+2))
ColC<-c(ColSelectC[1]:(ColSelectC[1]+2),ColSelectC[2]:(ColSelectC[2]+2),ColSelectC[3]:(ColSelectC[3]+2))
  
MatInterEdu[1,1]<-sum(as.numeric(as.matrix(data[RowH,ColH])))
MatInterEdu[1,2]<-sum(as.numeric(as.matrix(data[RowH,ColC])))
MatInterEdu[2,1]<-sum(as.numeric(as.matrix(data[RowC,ColH])))
MatInterEdu[2,2]<-sum(as.numeric(as.matrix(data[RowC,ColC])))

## check
sum(as.numeric(as.matrix(data[4:21,5:22])))
sum(MatInterEdu)
total<-sum(MatInterEdu)

MatInterEdu

MatInterEduTable<-as.data.frame(matrix(,nrow = 3,ncol = 3))
names(MatInterEduTable)<-c("Count","Percent","Odds Ratio")
row.names(MatInterEduTable)<-c("InterrEdu marriages","Same Edu marriages","Total marriages")

MatInterEduTable[1,1]<-MatInterEdu[1,2]+MatInterEdu[2,1]
MatInterEduTable[2,1]<-MatInterEdu[1,1]+MatInterEdu[2,2]
MatInterEduTable[3,1]<-total

MatInterEduTable[1,2]<-MatInterEduTable[1,1]/total*100
MatInterEduTable[2,2]<-MatInterEduTable[2,1]/total*100
MatInterEduTable[3,2]<-100

MatInterEduTable[1,3]<-(MatInterEdu[1,2]*MatInterEdu[2,2])/(MatInterEdu[1,1]*MatInterEdu[2,1])
MatInterEduTable[2,3]<-NA
MatInterEduTable[3,3]<-NA

MatInterEduTable

## 2019
## Unweighted
#                  Count   Percent Odds Ratio
#InterrEdu marriages  5163  28.35723   4.847164
#Same Edu marriages  13044  71.64277         NA
#Total marriages     18207 100.00000         NA

## 2019
## Weighted
#                    Count   Percent Odds Ratio
#InterrEdu marriages 1084790  28.50699   4.571423
#Same Edu marriages  2720557  71.49301         NA
#Total marriages     3805347 100.00000         NA

## 2010
## Unweighted
#                    Count   Percent Odds Ratio
#InterrEdu marriages  5172  29.28155   2.009839
#Same Edu marriages  12491  70.71845         NA
#Total marriages     17663 100.00000         NA

## 2010
# Weighted
#                    Count   Percent Odds Ratio
#InterrEdu marriages 1035019  28.15388   2.917371
#Same Edu marriages  2641273  71.84612         NA
#Total marriages     3676292 100.00000         NA


##########################################################
## c) The number and percentage of interracial couples
#(weighted and unweighted) where the White partner has more
# education, less education, and the same education

## Use Wife as the anchor

# white wife column 5:10
## non-white husband row 10:21

MatExchangeTable<-as.data.frame(matrix(,nrow = 6,ncol = 2))
names(MatExchangeTable)<-c("Count","Percent")
row.names(MatExchangeTable)<-c("White Wife - Non-white husband with lower Edu","White Wife - Non-white husband with the same Edu",
                               "White Wife - Non-white husband with higher Edu",
                               "Non-White Wife - white husband with lower Edu","Non-White Wife - white husband with the same Edu",
                               "Non-White Wife - white husband with higher Edu")

MatExchangeTable[1,1]<-sum(as.numeric(as.matrix(data[10:12,8:10])))+sum(as.numeric(as.matrix(data[16:18,8:10])))

MatExchangeTable[2,1]<-sum(as.numeric(as.matrix(data[10:12,5:7])))+sum(as.numeric(as.matrix(data[13:15,8:10])))+
                       sum(as.numeric(as.matrix(data[16:18,5:7])))+sum(as.numeric(as.matrix(data[19:21,8:10])))

MatExchangeTable[3,1]<-sum(as.numeric(as.matrix(data[13:15,5:7])))+sum(as.numeric(as.matrix(data[19:21,5:7])))

total1<-sum(as.numeric(as.matrix(data[10:21,5:10])))

MatExchangeTable[1:3,2]<-MatExchangeTable[1:3,1]/total1*100


# Non-white wife column 11:22
## white husband row 4:6

MatExchangeTable[4,1]<-sum(as.numeric(as.matrix(data[4:6,14:16])))+sum(as.numeric(as.matrix(data[4:6,20:22])))
MatExchangeTable[5,1]<-sum(as.numeric(as.matrix(data[4:6,11:13])))+sum(as.numeric(as.matrix(data[4:6,17:19])))+
                        sum(as.numeric(as.matrix(data[7:9,14:16])))+sum(as.numeric(as.matrix(data[7:9,20:22])))
MatExchangeTable[6,1]<-sum(as.numeric(as.matrix(data[7:9,11:13])))+sum(as.numeric(as.matrix(data[7:9,17:19])))

total2<-sum(as.numeric(as.matrix(data[4:9,11:22])))
MatExchangeTable[4:6,2]<-MatExchangeTable[4:6,1]/total2*100

MatExchangeTable

## 2019
## Unweigthed
##                                                Count   Percent
#White Wife - Non-white husband with lower Edu    189.0 20.203100
#White Wife - Non-white husband with the same Edu 654.0 69.909139
#White Wife - Non-white husband with higher Edu    92.5  9.887761
#Non-White Wife - white husband with lower Edu    197.5 17.077389
#Non-White Wife - white husband with the same Edu 834.0 72.114137
#Non-White Wife - white husband with higher Edu   125.0 10.808474


## 2019
## Weigthed
##                                                Count   Percent
#White Wife - Non-white husband with lower Edu     40821.5 20.367215
#White Wife - Non-white husband with the same Edu 140428.0 70.064238
#White Wife - Non-white husband with higher Edu    19178.0  9.568547
#Non-White Wife - white husband with lower Edu     43280.0 18.088390
#Non-White Wife - white husband with the same Edu 169291.0 70.753272
#Non-White Wife - white husband with higher Edu    26698.5 11.158338

## 2010
## Unweighted
#                                               Count  Percent
# White Wife - Non-white husband with lower Edu    169.0 21.00684
# White Wife - Non-white husband with the same Edu 540.5 67.18459
# White Wife - Non-white husband with higher Edu    95.0 11.80858
# Non-White Wife - white husband with lower Edu    159.0 19.83780
# Non-White Wife - white husband with the same Edu 550.5 68.68372
# Non-White Wife - white husband with higher Edu    92.0 11.47848

# 2010
## Weighted
#                                                 Count  Percent
# White Wife - Non-white husband with lower Edu     34962 20.15112
# White Wife - Non-white husband with the same Edu 114652 66.08223
# White Wife - Non-white husband with higher Edu    23885 13.76665
# Non-White Wife - white husband with lower Edu     27207 16.47441
# Non-White Wife - white husband with the same Edu 118568 71.79543
# Non-White Wife - white husband with higher Edu    19372 11.73016
####################################################################

