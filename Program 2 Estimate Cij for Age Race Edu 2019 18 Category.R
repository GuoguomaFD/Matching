
## Estimate Cij for 2019

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

UnmarriedProbMaleInitial<<-endmale2019/begmale2019
UnmarriedProbFemaleInitial<<-endfemale2019/begfemale2019

UnmarriedProbFemaleLag<<-UnmarriedProbFemaleInitial

denom=begmale2019%*%t(begfemale2019)
## adjust zero in Xij2019 to 1
Xij2019[Xij2019==0]<-1
aa=Xij2019/denom
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

prob[i,j]<-(UnmarriedProbFemaleLag[j]*exp(Vpara[i,j]))/(1+sum(begfemale2019*UnmarriedProbFemaleLag*exp(Vpara[i,])))

j<-j+1

}
i<-i+1
}

prob<<-prob

}

iterloglike<-function(para)

{

para=matrix(para,nrow=18,ncol=18)
majorP(para)
tempvalueofpara<<-para

loglike<-0

i<-1

while (i<=18)
{

j<-1

while (j<=18)
{

tempP<-log(prob[i,j])

logtemp<-tempP*Xij2019[i,j]

loglike<-loglike+logtemp

j<-j+1
}
i<-i+1
}


j<-1

while (j<=18)
{


tempR<-log(1-sum(prob[,j]*begmale2019))

logtemp<-tempR*endfemale2019[j]

loglike<-loglike+logtemp

j<-j+1
}


return(-loglike)

}


benchmarkloglike=304970.5
iterloglikevalue<-benchmarkloglike+6

steptol<-vector("numeric", length(iterinitial))
steptol[1:length(iterinitial)]<-1e-8

itercounter<-0

## convergence criteria is set to be 0.1

while (abs(iterloglikevalue-benchmarkloglike)>=0.1)
{

benchmarkloglike<-iterloglikevalue
iteroptim<-optim(iterinitial,iterloglike, method = c("BFGS"), control=list(trace=10, REPORT=10, maxit=800, ndeps=steptol))
iterloglikevalue<-iteroptim$value
iterinitial<-iteroptim$par
itercounter<-itercounter+1
UnmarriedProbFemaleLag<<-UnmarriedProbFemale

print(itercounter)
Probrecord<<-prob
UnmarriedMaleRecord<<-UnmarriedProbMale
UnmarriedFemaleRecord<<-UnmarriedProbFemale

# Keep temporary records
#write.csv(Probrecord, "Probrecord Black Male Grouping.csv")
#write.csv(UnmarriedMaleRecord, "UnmarriedMaleRecord Black Male Grouping.csv")
#write.csv(UnmarriedFemaleRecord, "UnmarriedFemaleRecord Black Male Grouping.csv")

}

library(MASS)
iteroptim$par
iteroptim$value





## organize the estimations

## Original form

write.csv(iteroptim$par, "EsimateAgeEduRace2019BlackMale.csv")


AdjEst<-exp(iteroptim$par)*10^8
write.csv(AdjEst, "EstAdj2019BlackMale.csv")

## Simulated Matches

EstMatch<-as.data.frame(matrix(0,nrow=18,ncol=18))


for (i in 1:18)
{
  for (j in 1:18)
  {
    EstMatch[i,j]<-begmale2019[i]*prob[i,j]*begfemale2019[j]
  }
}

#write.csv(EstMatch,"Estimated Matches for 2019 with Black Males Separate 18 groups.csv")

# prediction errors check

round(Xij2019-EstMatch,digits=0)

