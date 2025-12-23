

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


##################################
## Simulate the impact of females as the choice set

begfemaleSimul<-(begfemale2010/sum(begfemale2010))*sum(begfemale2019)

cbind(begfemale2019,begfemaleSimul)

EstMatchSimulfemale<-as.data.frame(matrix(0,nrow=18,ncol=18))


for (i in 1:18)
{
  for (j in 1:18)
  {
    EstMatchSimulfemale[i,j]<-begmale2019[i]*prob2019[i,j]*begfemaleSimul[j]
  }
}

##
MaleNameVec<-names(Xij2019)
MaleNameVec<-gsub('^.', '', MaleNameVec)
MaleNameVec<-paste("M",MaleNameVec,sep = "")

DiffMatChoiceSetFemale<-Xij2019-EstMatchSimulfemale

##################################

begmaleSimul<-(begmale2010/sum(begmale2010))*sum(begmale2019)

cbind(begmale2019,begmaleSimul)

EstMatchSimulmale<-as.data.frame(matrix(0,nrow=18,ncol=18))


for (i in 1:18)
{
  for (j in 1:18)
  {
    EstMatchSimulmale[i,j]<-begmaleSimul[i]*prob2019[i,j]*begfemale2019[j]
  }
}

DiffMatChoiceSetMale<-Xij2019-EstMatchSimulmale

##
MaleNameVec<-names(Xij2019)
MaleNameVec<-gsub('^.', '', MaleNameVec)
MaleNameVec<-paste("M",MaleNameVec,sep = "")

DiffMatChoiceSetFemale<-Xij2019-EstMatchSimulfemale

DiffMatChoiceSetMale<-Xij2019-EstMatchSimulmale



row.names(DiffMatChoiceSetFemale)<-MaleNameVec
row.names(DiffMatChoiceSetMale)<-MaleNameVec
row.names(Xij2010)<-MaleNameVec
row.names(Xij2019)<-MaleNameVec

names(DiffMatChoiceSetFemale)<-names(Xij2019)
names(DiffMatChoiceSetMale)<-names(Xij2019)

round(DiffMatChoiceSetFemale,digits=0)
round(DiffMatChoiceSetMale,digits = 0)

write.csv(round(DiffMatChoiceSetFemale,digits=0),"Number of Marriages Changed due to females as choice set 20192010.csv",row.names = FALSE)

write.csv(round(DiffMatChoiceSetMale,digits=0),"Number of Marriages Changed due to Males as choice set 20192010.csv",row.names = FALSE)
