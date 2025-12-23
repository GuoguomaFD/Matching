
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

logtemp<-tempP*Xij2010[i,j]

loglike<-loglike+logtemp

j<-j+1
}
i<-i+1
}


j<-1

while (j<=18)
{


tempR<-log(1-sum(prob[,j]*begmale2010))

logtemp<-tempR*endfemale2010[j]

loglike<-loglike+logtemp

j<-j+1
}


return(-loglike)

}


benchmarkloglike=289323.6
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

#write.csv(Probrecord, "Probrecord Black Male Grouping.csv")
#write.csv(UnmarriedMaleRecord, "UnmarriedMaleRecord Black Male Grouping.csv")
#write.csv(UnmarriedFemaleRecord, "UnmarriedFemaleRecord Black Male Grouping.csv")

}

library(MASS)
iteroptim$par
iteroptim$value




## organize the estimations

## Original form

write.csv(iteroptim$par, "EsimateAgeEduRace2010BlackMale.csv")


AdjEst<-exp(iteroptim$par)*10^8
write.csv(AdjEst, "EstAdj2010BlackMale.csv")

## Simulated Matches

EstMatch<-as.data.frame(matrix(0,nrow=18,ncol=18))


for (i in 1:18)
{
  for (j in 1:18)
  {
    EstMatch[i,j]<-begmale2010[i]*prob[i,j]*begfemale2010[j]
  }
}

#write.csv(EstMatch,"Estimated Matches for 2010 with Black Males Separate 18 groups.csv")

# prediction errors check

round(Xij2010-EstMatch,digits=0)


initial  value 289323.617741 
iter  10 value 289104.741577
iter  20 value 289098.481523
iter  30 value 289094.477648
iter  40 value 289093.753494
iter  50 value 289093.583655
final  value 289093.564820 
converged
[1] 1
initial  value 289093.565202 
final  value 289093.564878 
converged
[1] 2
