
## XXX is the location of your working folder
setwd("XXX")

rm(list=ls())



begmale2010<-read.csv("MaleEudAgeRaceBeginning2010BlackMale.csv", header=TRUE)
begfemale2010<-read.csv("FemaleEudAgeRaceBeginning2010BlackMale.csv", header=TRUE)

endmale2010<-read.csv("MaleEudAgeRaceEnding2010BlackMale.csv", header=TRUE)
endfemale2010<-read.csv("FemaleEudAgeRaceEnding2010BlackMale.csv", header=TRUE)

#############################
## row is male and columne is female
#############################
## row is male and columne is female


Xij2010<-read.csv("MarriedMat2010BlackMale.csv", header=TRUE)
Xij2010<-Xij2010[,2:19]
# round those 0.5 to 1
Xij2010<-round(Xij2010,digits = 0)

begmale2010<-as.vector(begmale2010[1:18,2])
begfemale2010<-as.vector(begfemale2010[1:18,2])

endmale2010<-as.vector(endmale2010[1:18,2])
endfemale2010<-as.vector(endfemale2010[1:18,2])

UnmarriedProbMaleInitial<<-endmale2010/begmale2010
UnmarriedProbFemaleInitial<<-endfemale2010/begfemale2010

UnmarriedProbFemaleLag<<-UnmarriedProbFemaleInitial

denom=begmale2010%*%t(begfemale2010)
## adjust zero in Xij2010 to 1
Xij2010[Xij2010==0]<-1
aa=Xij2010/denom
aa=log(aa)

AlingAA=exp(aa)*10^8

##write.csv(AlingAA, "AlignQian1980RACE.csv",row.names = FALSE)

iterinitial=as.matrix(aa)


untrace(print)

majorP<-function(para)
{

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

prob[i,j]<-(UnmarriedProbFemaleLag[j]*exp(Vpara[i,j]))/(1+sum(begfemale2010*UnmarriedProbFemaleLag*exp(Vpara[i,])))

j<-j+1

}
i<-i+1
}

prob<<-prob

}


finalEstimate<-read.csv("EsimateAgeEduRace2010BlackMale.csv")

majorP(finalEstimate[,2:19])

prob

write.csv(prob,"Probability of match between i and j in 2010.csv")

## Traditional definition of oddsRatio
# oddsRatio<-prob/(1-prob)

## We just use the probability
oddsRatio<-prob

write.csv(oddsRatio,"Odds Ratio of match between i and j in 2010.csv")

WeightsofMatches<-Xij2010/sum(Xij2010)

write.csv(WeightsofMatches,"Weights of match between i and j in 2010.csv")

############################################################
##Calculate the Odds Ratio for age and education Table in paper

rm(list=ls())

oddsRatio<-read.csv("Odds Ratio of match between i and j in 2010.csv")
WeightsofMatches<-read.csv("Weights of match between i and j in 2010.csv")

oddsRatio<-oddsRatio[,2:19]
WeightsofMatches<-WeightsofMatches[,2:19]

benchmarkVec<-diag(as.matrix(oddsRatio))
benchmarkWeghts<-diag(as.matrix(WeightsofMatches))

## Rescale Weights
benchmarkWeghts<-benchmarkWeghts/sum(benchmarkWeghts)

benchmarkMean<-t(benchmarkVec)%*%benchmarkWeghts

benchmarkMean

#[1,] 1.462553e-06



## there are 6(18/3)*6 submatrix, with the upper triangle (3 cells) with older wife
## thus altogether 36*3=108 elements
WifeOlderVec<-vector("numeric",108)
WifeOLderWeight<-vector("numeric",108)

counter<-1
for (i in 1:6)
{
  for (j in 1:6)
  {
    temp<-oddsRatio[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
    tempUpperTri<-temp[upper.tri(temp, diag = FALSE)]
    WifeOlderVec[((counter-1)*3+1):((counter-1)*3+3)]<-as.numeric(tempUpperTri)
    
    Weighttemp<-WeightsofMatches[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
    WeightTri<-Weighttemp[upper.tri(Weighttemp,diag=FALSE)]
    WifeOLderWeight[((counter-1)*3+1):((counter-1)*3+3)]<-as.numeric(WeightTri)
      
    counter<-counter+1
    
  }
}

# Rescale the weights

WifeOLderWeight<-WifeOLderWeight/sum(WifeOLderWeight)

wifeOlderMean<-t(WifeOlderVec)%*%WifeOLderWeight
wifeOlderMean

##  [1,] 1.25475e-07

wifeOlderMean/benchmarkMean

#[1,] 0.08579178

#########################################
## [2,1] and [3,2] two cells * 36
HusbOlderVec<-vector("numeric",72)
HusbOLderWeight<-vector("numeric",72)

SelectMat<-matrix(0,3,3)
SelectMat[3,2]<-1
SelectMat[2,1]<-1

counter<-1
for (i in 1:6)
{
  for (j in 1:6)
  {
    temp<-oddsRatio[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
    tempUpperTri<-temp[SelectMat]
    HusbOlderVec[((counter-1)*2+1):((counter-1)*2+2)]<-as.numeric(tempUpperTri)
    
    Weighttemp<-WeightsofMatches[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
    WeightTri<-Weighttemp[SelectMat]
    HusbOLderWeight[((counter-1)*2+1):((counter-1)*2+2)]<-as.numeric(WeightTri)
    
    counter<-counter+1
    
  }
}

HusbOLderWeight<-HusbOLderWeight/sum(HusbOLderWeight)

HusbOlderMean<-t(HusbOlderVec)%*%HusbOLderWeight
HusbOlderMean

## [1,] 1.957327e-06

HusbOlderMean/benchmarkMean

#[1,] 1.338295


############################################
## much older husband

#########################################
## [3,1] two cells * 36
HusbOlder2Vec<-vector("numeric",36)
HusbOLder2Weight<-vector("numeric",36)

SelectMat<-matrix(0,3,3)
SelectMat[3,1]<-1


counter<-1
for (i in 1:6)
{
  for (j in 1:6)
  {
    temp<-oddsRatio[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
    tempUpperTri<-temp[SelectMat]
    HusbOlder2Vec[((counter-1)*1+1)]<-as.numeric(tempUpperTri)
    
    Weighttemp<-WeightsofMatches[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
    WeightTri<-Weighttemp[SelectMat]
    HusbOLder2Weight[((counter-1)*1+1)]<-as.numeric(WeightTri)
    
    counter<-counter+1
    
  }
}

HusbOLder2Weight<-HusbOLder2Weight/sum(HusbOLder2Weight)

HusbOlder2Mean<-t(HusbOlder2Vec)%*%HusbOLder2Weight
HusbOlder2Mean

##  1.957327e-06

HusbOlder2Mean/benchmarkMean

#[1,] [1,] 1.338295

#####################################################
## Simulate Education

## use 6 * 6 as the matrix, diagonal are the same education
## the same education 9 cells * 6

SameEduVec<-vector("numeric",54)
SameEduWeight<-vector("numeric",54)

counter<-1
for (rowCol in 1:6)
{
  i<-rowCol
  j<-rowCol
  
    temp<-oddsRatio[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
    SameEduVec[((counter-1)*9+1):((counter-1)*9+9)]<-as.numeric(as.matrix(temp))
    
    Weighttemp<-WeightsofMatches[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
    SameEduWeight[((counter-1)*9+1):((counter-1)*9+9)]<-as.numeric(as.matrix(Weighttemp))
    
    counter<-counter+1
    
  }

SameEduWeight<-SameEduWeight/sum(SameEduWeight)

SameEduMean<-t(SameEduVec)%*%SameEduWeight
SameEduMean

#  [1,] 1.175729e-06

###################################
## wives have higher education in (1,2),(1,4),(1,6)
## wives have higher education in (3,2),(3,4),(3,6)
## wives have higher education in (5,2),(5,4),(5,6)

## 9*9=81


WifeHigherEduVec<-vector("numeric",81)
WifeHigherEduWeight<-vector("numeric",81)


counter<-1
for (i in c(1,3,5))
{
  for (j in c(2,4,6))
{
  temp<-oddsRatio[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
  WifeHigherEduVec[((counter-1)*9+1):((counter-1)*9+9)]<-as.numeric(as.matrix(temp))
  
  Weighttemp<-WeightsofMatches[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
  WifeHigherEduWeight[((counter-1)*9+1):((counter-1)*9+9)]<-as.numeric(as.matrix(Weighttemp))
  
  counter<-counter+1
  
  print(counter)
  }
}

WifeHigherEduWeight<-WifeHigherEduWeight/sum(WifeHigherEduWeight)

WifeHigherMean<-t(WifeHigherEduVec)%*%WifeHigherEduWeight
WifeHigherMean

#[[1,] [1,] 3.3767e-07

WifeHigherMean/SameEduMean

# 1,] 0.2872007


## husbands have higher education in (2,1),(2,3),(2,5)
## husbands have higher education in (4,1),(4,3),(4,5)
## husbands have higher education in (6,1),(6,3),(6,5)

HusHigherEduVec<-vector("numeric",81)
HusHigherEduWeight<-vector("numeric",81)

counter<-1
for (i in c(2,4,6))
{
  for (j in c(1,3,5))
  {
    temp<-oddsRatio[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
    HusHigherEduVec[((counter-1)*9+1):((counter-1)*9+9)]<-as.numeric(as.matrix(temp))
    
    Weighttemp<-WeightsofMatches[((i-1)*3+1):((i-1)*3+3),((j-1)*3+1):((j-1)*3+3)]
    HusHigherEduWeight[((counter-1)*9+1):((counter-1)*9+9)]<-as.numeric(as.matrix(Weighttemp))
    
    counter<-counter+1
    
  }
}

HusHigherEduWeight<-HusHigherEduWeight/sum(HusHigherEduWeight)

HusHigherMean<-t( HusHigherEduVec)%*%HusHigherEduWeight
HusHigherMean

# [1,] 1.652246e-07

HusHigherMean/SameEduMean

# [1,] 0.1405295
