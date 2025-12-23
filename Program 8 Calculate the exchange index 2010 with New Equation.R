

## XXX is the location of your working folder
setwd("XXX")

rm(list=ls())


begmale2010<-read.csv("MaleEudAgeRaceBeginning2010BlackMale.csv", header=TRUE)
begfemale2010<-read.csv("FemaleEudAgeRaceBeginning2010BlackMale.csv", header=TRUE)

endmale2010<-read.csv("MaleEudAgeRaceEnding2010BlackMale.csv", header=TRUE)
endfemale2010<-read.csv("FemaleEudAgeRaceEnding2010BlackMale.csv", header=TRUE)

#############################
## row is male and columne is female



Xij2010<-read.csv("MarriedMat2010BlackMale.csv", header=TRUE)
## round those 0.5 to 1
Xij2010<-Xij2010[,2:19]
Xij2010<-round(Xij2010,digits=0)

begmale2010<-as.vector(begmale2010[1:18,2])
begfemale2010<-as.vector(begfemale2010[1:18,2])

endmale2010<-as.vector(endmale2010[1:18,2])
endfemale2010<-as.vector(endfemale2010[1:18,2])


#######################################################
UnmarriedProbMaleInitial<<-endmale2010/begmale2010
UnmarriedProbFemaleInitial<<-endfemale2010/begfemale2010



para<-read.csv("EsimateAgeEduRace2010BlackMale.csv", header=TRUE)
para<-para[,2:19]
Vpara=para
UnmarriedProbMale<-as.vector(UnmarriedProbMaleInitial)
UnmarriedProbFemale<-vector("numeric",length=18)

m<-1
while (m<=1000)
{
  
  j<-1
  
  while (j<=18)
  {
    
    qfunc<-1/(1+sum(begmale2010*UnmarriedProbMale*exp(Vpara[,j])))
    
    UnmarriedProbFemale[j]<-qfunc
    
    j<-j+1
  }
  
  UnmarriedProbMale<-vector("numeric",length=18)
  
  i<-1
  while (i<=18)
  {
    
    rfunc<-1/(1+sum(begfemale2010*UnmarriedProbFemale*exp(Vpara[i,])))
    UnmarriedProbMale[i]<-rfunc
    i<-i+1
  }
  
  m<-m+1
  
}

UnmarriedProbMale<<-UnmarriedProbMale
UnmarriedProbFemale<<-UnmarriedProbFemale

prob<-matrix(0,nrow=18,ncol=18)

i<-1

while (i<=18)
{
  j<-1
  while (j<=18)
  {
    
    prob[i,j]<-(UnmarriedProbFemale[j]*exp(Vpara[i,j]))/(1+sum(begfemale2010*UnmarriedProbFemale*exp(Vpara[i,])))
    
    j<-j+1
    
  }
  i<-i+1
}

prob2010<<-prob

## prob2010 can be used to calculate the conditional probability

## expected education status of husband of white wife and black husband
## col 1:6 and row 7:12
## high school is 8.6 years and college is 15.3 years

Xij2010[7:12,1:6]

MatHH<-sum(Xij2010[7:9,1:3])
MatHC<-sum(Xij2010[7:9,4:6])
MatCH<-sum(Xij2010[10:12,1:3])
MatCC<-sum(Xij2010[10:12,4:6])

ProbHH<-sum(prob2010[7:9,1:3])
ProbHC<-sum(prob2010[7:9,4:6])
ProbCH<-sum(prob2010[10:12,1:3])
ProbCC<-sum(prob2010[10:12,4:6])

ProbWWBH<-sum(prob2010[7:12,1:6])


ExpectEduWWBH<-(8.6*ProbHH/ProbWWBH+
                8.6*ProbHC/ProbWWBH+
                15.3*ProbCH/ProbWWBH+
                15.3*ProbCC/ProbWWBH)

ExpectEduWWBH
#1] 13.60967

#############################################


## expected education status of husband of white wife and white husband
## col 1:6 and row 1:6
## high school is 8.6 years and college is 15.3 years

Xij2010[1:6,1:6]

MatHH<-sum(Xij2010[1:3,1:3])
MatHC<-sum(Xij2010[1:3,4:6])
MatCH<-sum(Xij2010[4:6,1:3])
MatCC<-sum(Xij2010[4:6,4:6])

ProbHH<-sum(prob2010[1:3,1:3])
ProbHC<-sum(prob2010[1:3,4:6])
ProbCH<-sum(prob2010[4:6,1:3])
ProbCC<-sum(prob2010[4:6,4:6])

ProbWWWH<-sum(prob2010[1:6,1:6])


ExpectEduWWWH<-(8.6*ProbHH/ProbWWWH+
                  8.6*ProbHC/ProbWWWH+
                  15.3*ProbCH/ProbWWWH+
                  15.3*ProbCC/ProbWWWH)

ExpectEduWWWH
#[1] 12.74806

ExpectEduWWBH-ExpectEduWWWH
# 0.8616102


###############################
## Based on Jia's new Equation

nZeroWWBH<-sum(Xij2010[7:12,1:6])

MatHHWWBH<-sum(Xij2010[7:9,1:3])
MatHCWWBH<-sum(Xij2010[7:9,4:6])
MatCHWWBH<-sum(Xij2010[10:12,1:3])
MatCCWWBH<-sum(Xij2010[10:12,4:6])

ProbHHWWBH<-sum(prob2010[7:9,1:3])
ProbHCWWBH<-sum(prob2010[7:9,4:6])
ProbCHWWBH<-sum(prob2010[10:12,1:3])
ProbCCWWBH<-sum(prob2010[10:12,4:6])

ProbHusHighWWBH<-ProbHHWWBH+ProbHCWWBH
ProbHusColWWBH<-ProbCHWWBH+ProbCCWWBH

ProbWWBH<-sum(prob2010[7:12,1:6])

MatHHWWWH<-sum(Xij2010[1:3,1:3])
MatHCWWWH<-sum(Xij2010[1:3,4:6])
MatCHWWWH<-sum(Xij2010[4:6,1:3])
MatCCWWWH<-sum(Xij2010[4:6,4:6])

ProbHHWWWH<-sum(prob2010[1:3,1:3])
ProbHCWWWH<-sum(prob2010[1:3,4:6])
ProbCHWWWH<-sum(prob2010[4:6,1:3])
ProbCCWWWH<-sum(prob2010[4:6,4:6])

ProbHusHighWWWH<-ProbHHWWWH+ProbHCWWWH
ProbHusColWWWH<-ProbCHWWWH+ProbCCWWWH

ProbWWWH<-sum(prob2010[1:6,1:6])

#(8.6*(ProbHusHighWWBH/ProbWWBH-ProbHusHighWWWH/ProbWWWH)*(MatHHWWBH+MatHCWWBH)+
#    15.3*(ProbHusColWWBH/ProbWWBH-ProbHusColWWWH/ProbWWWH)*(MatCHWWBH+MatCCWWBH))/nZeroWWBH

##############
# [1] 0.407521
##############

(
  (8.6*(ProbHusHighWWBH/ProbWWBH-ProbHusHighWWWH/ProbWWWH)+15.3*(ProbHusColWWBH/ProbWWBH-ProbHusColWWWH/ProbWWWH))*(MatHHWWBH+MatHCWWBH)+
    (8.6*(ProbHusHighWWBH/ProbWWBH-ProbHusHighWWWH/ProbWWWH)+15.3*(ProbHusColWWBH/ProbWWBH-ProbHusColWWWH/ProbWWWH))*(MatCHWWBH+MatCCWWBH)
)/nZeroWWBH

# [1] 0.8616102

## predicted WWBH Education

(
  (8.6*(ProbHusHighWWBH/ProbWWBH)+15.3*(ProbHusColWWBH/ProbWWBH))*(MatHHWWBH+MatHCWWBH)+
    (8.6*(ProbHusHighWWBH/ProbWWBH)+15.3*(ProbHusColWWBH/ProbWWBH))*(MatCHWWBH+MatCCWWBH)
)/nZeroWWBH

#[1] 13.60967

############################
## Dong and Xie Exchage Index

## get the distribution of wife education for white wife and black husband

Xij2010[7:12,1:6]


MatWWBH<-sum(Xij2010[7:12,1:6])

MatHH1<-sum(Xij2010[7:9,1:3])/MatWWBH
MatHC1<-sum(Xij2010[7:9,4:6])/MatWWBH
MatCH1<-sum(Xij2010[10:12,1:3])/MatWWBH
MatCC1<-sum(Xij2010[10:12,4:6])/MatWWBH

MatHH1
MatHC1
MatCH1
MatCC1



## get the distribution of wife education for white wife and white husband

Xij2010[1:6,1:6]


MatWWWH<-sum(Xij2010[1:6,1:6])

MatHH0<-sum(Xij2010[1:3,1:3])/MatWWWH
MatHC0<-sum(Xij2010[1:3,4:6])/MatWWWH
MatCH0<-sum(Xij2010[4:6,1:3])/MatWWWH
MatCC0<-sum(Xij2010[4:6,4:6])/MatWWWH

MatHH0
MatHC0
MatCH0
MatCC0

## white wife and black husband 
## wife high school and college distribution
MatHH1+MatCH1
# 0.4166667
MatHC1+MatCC1
# 0.5833333

## white wife and white husband 
## wife high school and college distribution
MatHH0+MatCH0  
# 0.4413383
MatHC0+MatCC0
# 0.5586617

## white wife and black husband

ExpEduWWBHDX<-(8.6*MatHH1+
                 8.6*MatHC1+
                 15.3*MatCH1+
                 15.3*MatCC1)

ExpEduWWBHDX
#[1] 11.89924

ExpEduWWWHDX<-(8.6*MatHH0+15.3*MatCH0)/0.4413383*0.4166667+
                 (8.6*MatHC0+15.3*MatCC0)/0.5586617*0.5833333
ExpEduWWWHDX
# [1] 11.81272
ExpEduWWBHDX-ExpEduWWWHDX
# [1] 0.08652504

NonAdjustedEduWWWWH<-(8.6*MatHH0+
              8.6*MatHC0+
              15.3*MatCH0+
              15.3*MatCC0)

NonAdjustedEduWWWWH
#[1] 11.74225

##################
## Naive Exchange 
###################

ExpEduWWBHDX-NonAdjustedEduWWWWH
#1] 0.1569924

#############################################################################
# follow the exact formula in the paper ATT

## WWBH husband's eduation

Xij2010[7:12,1:6]


MatWWBH<-sum(Xij2010[7:12,1:6])

MatHH<-sum(Xij2010[7:9,1:6])
MatCH<-sum(Xij2010[10:12,1:6])

## number of high school husband and college husband
MatHH
MatCH

ExpEduWWBHATT<-(8.6*MatHH+15.3*MatCH)/MatWWBH

## The Weighting need to be revised to reflect the e(WWBH)/e(WWWH) where e=P(WWBH|wife's education)

EWeightWWBHHighSchoolWife<- sum(Xij2010[7:12,1:3])/sum(Xij2010[1:18,1:3])
EWeightWWBHCollegeWife<- sum(Xij2010[7:12,4:6])/sum(Xij2010[1:18,4:6])

EWeightWWWHHighSchoolWife<- sum(Xij2010[1:6,1:3])/sum(Xij2010[1:18,1:3])
EWeightWWWHCollegeWife<- sum(Xij2010[1:6,4:6])/sum(Xij2010[1:18,4:6])

AdjWeightHighSchool<-EWeightWWBHHighSchoolWife/EWeightWWWHHighSchoolWife
AdjWeightCollege<-EWeightWWBHCollegeWife/EWeightWWWHCollegeWife


ExpEduWWWHATT<- (8.6*AdjWeightHighSchool*MatHH+15.3*AdjWeightCollege*MatCH)/(AdjWeightHighSchool*MatHH+AdjWeightCollege*MatCH)

ExpEduWWBHATT
#[1] 11.89924

ExpEduWWWHATT
# [1] 12.06793

ExpEduWWBHATT-ExpEduWWWHATT

#[1] -0.1686863

## If Dong and Xie WWBH ExpEduWWBHATT and our WWWH

ExpectEduWWWH
#[1] 12.74806

11.89924-12.74806
# -0.84882