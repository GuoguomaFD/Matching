

## Simulated Matches


#############################################################################
## 2019

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

Transfer2019<-Xij2019
Transfer2019[1:18,1:18]<-NA

for (i in 1:18)
{
  for (j in 1:18)
  {
    Transfer2019[i,j]<-0.5*(log(endfemale2019[i]-log(endmale2019[j])))
  }
}

round(Transfer2019,digits = 2)

#write.csv(Transfer2019,"Transfer 2019.csv",row.names = FALSE)


###################################################
## 2010


begmale2010<-read.csv("MaleEudAgeRaceBeginning2010BlackMale.csv", header=TRUE)
begfemale2010<-read.csv("FemaleEudAgeRaceBeginning2010BlackMale.csv", header=TRUE)

endmale2010<-read.csv("MaleEudAgeRaceEnding2010BlackMale.csv", header=TRUE)
endfemale2010<-read.csv("FemaleEudAgeRaceEnding2010BlackMale.csv", header=TRUE)

Xij2010<-read.csv("MarriedMat2010BlackMale.csv", header=TRUE)
## round those 0.5 to 1
Xij2010<-Xij2010[,2:19]
Xij2010<-round(Xij2010,digits=0)

begmale2010<-as.vector(begmale2010[1:18,2])
begfemale2010<-as.vector(begfemale2010[1:18,2])

endmale2010<-as.vector(endmale2010[1:18,2])
endfemale2010<-as.vector(endfemale2010[1:18,2])

Transfer2010<-Xij2010
Transfer2010[1:18,1:18]<-NA

for (i in 1:18)
{
  for (j in 1:18)
  {
    Transfer2010[i,j]<-0.5*(log(endfemale2010[i]-log(endmale2010[j])))
  }
}

round(Transfer2010,digits = 2)

#write.csv(Transfer2010,"Transfer 2010.csv",row.names = FALSE)

TransferDiff20192010<-Transfer2019-Transfer2010

#write.csv(TransferDiff20192010,"Transfer Difference 2019 and 2010.csv")
