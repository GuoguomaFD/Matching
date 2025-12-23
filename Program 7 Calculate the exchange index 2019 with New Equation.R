

## Simulated Matches

## XXX is the location of your working folder
setwd("XXX")


rm(list=ls())

begmale2019<-read.csv("MaleEudAgeRaceBeginning2019BlackMale.csv", header=TRUE)
begfemale2019<-read.csv("FemaleEudAgeRaceBeginning2019BlackMale.csv", header=TRUE)

endmale2019<-read.csv("MaleEudAgeRaceEnding2019BlackMale.csv", header=TRUE)
endfemale2019<-read.csv("FemaleEudAgeRaceEnding2019BlackMale.csv", header=TRUE)


#############################
## row is male and columne is female


Xij2019<-read.csv("MarriedMat2019BlackMale.csv", header=TRUE)
## round those 0.5 to 1
Xij2019<-Xij2019[,2:19]
Xij2019<-round(Xij2019,digits=0)

begmale2019<-as.vector(begmale2019[1:18,2])
begfemale2019<-as.vector(begfemale2019[1:18,2])

endmale2019<-as.vector(endmale2019[1:18,2])
endfemale2019<-as.vector(endfemale2019[1:18,2])


#######################################################
UnmarriedProbMaleInitial<<-endmale2019/begmale2019
UnmarriedProbFemaleInitial<<-endfemale2019/begfemale2019



para<-read.csv("EsimateAgeEduRace2019BlackMale.csv", header=TRUE)
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
    
    qfunc<-1/(1+sum(begmale2019*UnmarriedProbMale*exp(Vpara[,j])))
    
    UnmarriedProbFemale[j]<-qfunc
    
    j<-j+1
  }
  
  UnmarriedProbMale<-vector("numeric",length=18)
  
  i<-1
  while (i<=18)
  {
    
    rfunc<-1/(1+sum(begfemale2019*UnmarriedProbFemale*exp(Vpara[i,])))
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
    
    prob[i,j]<-(UnmarriedProbFemale[j]*exp(Vpara[i,j]))/(1+sum(begfemale2019*UnmarriedProbFemale*exp(Vpara[i,])))
    
    j<-j+1
    
  }
  i<-i+1
}

prob2019<<-prob

## prob2019 can be used to calculate the conditional probability

## expected education status of husband of white wife and black husband
## col 1:6 and row 7:12
## high school is 8.6 years and college is 15.3 years

Xij2019[7:12,1:6]

MatHH<-sum(Xij2019[7:9,1:3])
MatHC<-sum(Xij2019[7:9,4:6])
MatCH<-sum(Xij2019[10:12,1:3])
MatCC<-sum(Xij2019[10:12,4:6])

ProbHH<-sum(prob2019[7:9,1:3])
ProbHC<-sum(prob2019[7:9,4:6])
ProbCH<-sum(prob2019[10:12,1:3])
ProbCC<-sum(prob2019[10:12,4:6])

ProbWWBH<-sum(prob2019[7:12,1:6])


ExpectEduWWBH<-(8.6*ProbHH/ProbWWBH+
                8.6*ProbHC/ProbWWBH+
                15.3*ProbCH/ProbWWBH+
                15.3*ProbCC/ProbWWBH)

ExpectEduWWBH
#[1] 13.60182
#############################################


## expected education status of husband of white wife and white husband
## col 1:6 and row 1:6
## high school is 8.6 years and college is 15.3 years

Xij2019[1:6,1:6]

MatHH<-sum(Xij2019[1:3,1:3])
MatHC<-sum(Xij2019[1:3,4:6])
MatCH<-sum(Xij2019[4:6,1:3])
MatCC<-sum(Xij2019[4:6,4:6])

ProbHH<-sum(prob2019[1:3,1:3])
ProbHC<-sum(prob2019[1:3,4:6])
ProbCH<-sum(prob2019[4:6,1:3])
ProbCC<-sum(prob2019[4:6,4:6])

ProbWWWH<-sum(prob2019[1:6,1:6])


ExpectEduWWWH<-(8.6*ProbHH/ProbWWWH+
                  8.6*ProbHC/ProbWWWH+
                  15.3*ProbCH/ProbWWWH+
                  15.3*ProbCC/ProbWWWH)

ExpectEduWWWH
#[1] 12.98457
ExpectEduWWBH-ExpectEduWWWH
#[1] 0.6172539

###############################
## Based on Jia's new Equation

nZeroWWBH<-sum(Xij2019[7:12,1:6])

MatHHWWBH<-sum(Xij2019[7:9,1:3])
MatHCWWBH<-sum(Xij2019[7:9,4:6])
MatCHWWBH<-sum(Xij2019[10:12,1:3])
MatCCWWBH<-sum(Xij2019[10:12,4:6])

ProbHHWWBH<-sum(prob2019[7:9,1:3])
ProbHCWWBH<-sum(prob2019[7:9,4:6])
ProbCHWWBH<-sum(prob2019[10:12,1:3])
ProbCCWWBH<-sum(prob2019[10:12,4:6])

ProbHusHighWWBH<-ProbHHWWBH+ProbHCWWBH
ProbHusColWWBH<-ProbCHWWBH+ProbCCWWBH

ProbWWBH<-sum(prob2019[7:12,1:6])

MatHHWWWH<-sum(Xij2019[1:3,1:3])
MatHCWWWH<-sum(Xij2019[1:3,4:6])
MatCHWWWH<-sum(Xij2019[4:6,1:3])
MatCCWWWH<-sum(Xij2019[4:6,4:6])

ProbHHWWWH<-sum(prob2019[1:3,1:3])
ProbHCWWWH<-sum(prob2019[1:3,4:6])
ProbCHWWWH<-sum(prob2019[4:6,1:3])
ProbCCWWWH<-sum(prob2019[4:6,4:6])

ProbHusHighWWWH<-ProbHHWWWH+ProbHCWWWH
ProbHusColWWWH<-ProbCHWWWH+ProbCCWWWH

ProbWWWH<-sum(prob2019[1:6,1:6])

#(8.6*(ProbHusHighWWBH/ProbWWBH-ProbHusHighWWWH/ProbWWWH)*(MatHHWWBH+MatHCWWBH)+
#      15.3*(ProbHusColWWBH/ProbWWBH-ProbHusColWWWH/ProbWWWH)*(MatCHWWBH+MatCCWWBH))/nZeroWWBH


##############
# [1] 0.473332
##############

(
  (8.6*(ProbHusHighWWBH/ProbWWBH-ProbHusHighWWWH/ProbWWWH)+15.3*(ProbHusColWWBH/ProbWWBH-ProbHusColWWWH/ProbWWWH))*(MatHHWWBH+MatHCWWBH)+
      (8.6*(ProbHusHighWWBH/ProbWWBH-ProbHusHighWWWH/ProbWWWH)+15.3*(ProbHusColWWBH/ProbWWBH-ProbHusColWWWH/ProbWWWH))*(MatCHWWBH+MatCCWWBH)
  )/nZeroWWBH

#[1] 0.6172539

#############################################
## check whether this is equivalent to just one person

  (8.6*(ProbHusHighWWBH/ProbWWBH-ProbHusHighWWWH/ProbWWWH)+15.3*(ProbHusColWWBH/ProbWWBH-ProbHusColWWWH/ProbWWWH))
#[1] 0.6172539

## predicted WWBH Education

(
  (8.6*(ProbHusHighWWBH/ProbWWBH)+15.3*(ProbHusColWWBH/ProbWWBH))*(MatHHWWBH+MatHCWWBH)+
    (8.6*(ProbHusHighWWBH/ProbWWBH)+15.3*(ProbHusColWWBH/ProbWWBH))*(MatCHWWBH+MatCCWWBH)
)/nZeroWWBH

# 1] 13.60182

############################
## Dong and Xie Exchange Index

## get the distribution of wife education for white wife and black husband

Xij2019[7:12,1:6]


MatWWBH<-sum(Xij2019[7:12,1:6])

MatHH1<-sum(Xij2019[7:9,1:3])/MatWWBH
MatHC1<-sum(Xij2019[7:9,4:6])/MatWWBH
MatCH1<-sum(Xij2019[10:12,1:3])/MatWWBH
MatCC1<-sum(Xij2019[10:12,4:6])/MatWWBH

MatHH1
MatHC1
MatCH1
MatCC1



## get the distribution of wife education for white wife and white husband

Xij2019[1:6,1:6]


MatWWWH<-sum(Xij2019[1:6,1:6])

MatHH0<-sum(Xij2019[1:3,1:3])/MatWWWH
MatHC0<-sum(Xij2019[1:3,4:6])/MatWWWH
MatCH0<-sum(Xij2019[4:6,1:3])/MatWWWH
MatCC0<-sum(Xij2019[4:6,4:6])/MatWWWH

MatHH0
MatHC0
MatCH0
MatCC0

## white wife and black husband 
## wife high school and college distribution
MatHH1+MatCH1
# 0.3188976
MatHC1+MatCC1
# 0.6811024

## white wife and white husband 
## wife high school and college distribution
MatHH0+MatCH0
# 0.2936715
MatHC0+MatCC0
# 0.7063285

## white wife and black husband

ExpEduWWBHDX<-(8.6*MatHH1+
                 8.6*MatHC1+
                 15.3*MatCH1+
                 15.3*MatCC1)

ExpEduWWBHDX
#[1] 12.45118

ExpEduWWWHDX<-(8.6*MatHH0+15.3*MatCH0)/0.2936715*0.3188976+
                 (8.6*MatHC0+15.3*MatCC0)/0.7063285*0.6811024
ExpEduWWWHDX
#[1] 12.67613

ExpEduWWBHDX-ExpEduWWWHDX
#[1] -0.2249466

NonAdjustedEduWWWWH<-(8.6*MatHH0+
              8.6*MatHC0+
              15.3*MatCH0+
              15.3*MatCC0)

NonAdjustedEduWWWWH
#[1] 12.74446

##################
## Naive Exchange 
###################

ExpEduWWBHDX-NonAdjustedEduWWWWH
#[1] -0.2932823

#############################################################################
# follow the exact formula in the paper ATT

## WWBH husband's eduation

Xij2019[7:12,1:6]


MatWWBH<-sum(Xij2019[7:12,1:6])

MatHH<-sum(Xij2019[7:9,1:6])
MatCH<-sum(Xij2019[10:12,1:6])

## number of high school husband and college husband
MatHH
MatCH

ExpEduWWBHATT<-(8.6*MatHH+15.3*MatCH)/MatWWBH

## The Weighting need to be revised to reflect the e(WWBH)/e(WWWH) where e=P(WWBH|wife's education)

EWeightWWBHHighSchoolWife<- sum(Xij2019[7:12,1:3])/sum(Xij2019[1:18,1:3])
EWeightWWBHCollegeWife<- sum(Xij2019[7:12,4:6])/sum(Xij2019[1:18,4:6])

EWeightWWWHHighSchoolWife<- sum(Xij2019[1:6,1:3])/sum(Xij2019[1:18,1:3])
EWeightWWWHCollegeWife<- sum(Xij2019[1:6,4:6])/sum(Xij2019[1:18,4:6])

AdjWeightHighSchool<-EWeightWWBHHighSchoolWife/EWeightWWWHHighSchoolWife
AdjWeightCollege<-EWeightWWBHCollegeWife/EWeightWWWHCollegeWife


ExpEduWWWHATT<- (8.6*AdjWeightHighSchool*MatHH+15.3*AdjWeightCollege*MatCH)/(AdjWeightHighSchool*MatHH+AdjWeightCollege*MatCH)

ExpEduWWBHATT
#[1] 12.45118

ExpEduWWWHATT
#[1] 12.25517

ExpEduWWBHATT-ExpEduWWWHATT

#[1] 0.1960087

## If Dong and Xie WWBH ExpEduWWBHATT and our WWWH

#ExpectEduWWWH
#[1] 12.98457

# 12.45118-12.98457
-0.53339
