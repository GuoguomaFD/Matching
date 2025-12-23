
## XXX is the location of your working folder
setwd("XXX")

rm(list=ls())


## program for US data

library(readxl)

## Need to read in sheet entitled "CijEst2010"
data<-read_excel("Matches Matrix Table Template.xlsx",3)

subdata<-as.data.frame(data[4:21,5:22])

## use row and col names to get age difference

subdata<-as.data.frame(lapply(subdata, as.numeric))


MaleAge<-rep(c(26,34,42),times=6)
FemaleAge<-rep(c(24,31,38),times=6)


## age difference matrix

AgeDiffMat<-subdata
AgeDiffMat[,]<-0

for (i in 1:18)
{
  for (j in 1:18)
  {
    ## As in the Logan et al. 2008 paper
    ## the age difference = age of potential mate -age of evaluate
    ## thus since we evaluate from male perspective, so we use female minus male
    AgeDiffMat[i,j]<-FemaleAge[i]-MaleAge[j]
  }
}

table(as.numeric(as.matrix(AgeDiffMat)))

AgeDiffMale<-as.numeric(row.names(table(as.numeric(as.matrix(AgeDiffMat)))))
AgeDiffMaleUtility<-AgeDiffMale
AgeDiffMaleUtility[]<-0

for (i in 1:9)
{
  AgeDiffMaleUtility[i]<-mean(subdata[AgeDiffMat==AgeDiffMale[i]],rm.na=TRUE)
}

MaleCombined<-cbind(AgeDiffMale,AgeDiffMaleUtility)
MaleCombined

AgeDiffMale AgeDiffMaleUtility
[1,]         -18           1.626668
[2,]         -11           3.170982
[3,]         -10          11.282925
[4,]          -4           4.109545
[5,]          -3          23.866877
[6,]          -2          61.441874
[7,]           4           6.305686
[8,]           5          17.402382
[9,]          12           5.831566



x<-AgeDiffMale[c(2,3,5,6,8,9)]
## to be comparable with the Logan paper
y<-log(AgeDiffMaleUtility)[c(2,3,5,6,8,9)]
y<-log(y)-1.2

# Create a data frame
data <- data.frame(x = x, y = y)

plot(x, y, main = "Men", xlab = "Years of Difference", ylab = "Utility",type = "n",ylim=c(-1.3,0),xlim=c(-12,12))

spline_fit <- smooth.spline(x, y)
lines(spline_fit, col = "blue", lwd = 2)
par(new=TRUE)
LoganMale<-c(-0.4,-0.3,0,-0.01,-0.4,-1)
plot(x,LoganMale,col="red",type="l",ylim=c(-1.3,0),xlim=c(-12,12),ylab = "",xlab = "",lty=2,lwd=2)
legend(-5,-1,legend=c("Our findings using 2010 data","Logan et al. (2008) using 1988 data"),col=c("blue","red"),lwd=2,bty = "n",lty=1:2)


######################
## for female just flip the x

x<--AgeDiffMale[c(2,3,5,6,8,9)]
## to be comparable with the Logan paper
y<-log(AgeDiffMaleUtility)[c(2,3,5,6,8,9)]
y<-log(y)-1.2

# Create a data frame
data <- data.frame(x = x, y = y)

plot(x, y, main = "Women", xlab = "Years of Difference", ylab = "Utility",type = "n",ylim=c(-1.7,0.2),xlim=c(-12,12))

spline_fit <- smooth.spline(x, y)
lines(spline_fit, col = "blue", lwd = 2)
par(new=TRUE)
LoganFemale<-c(-0.2,-0.15,0.09,0.08,-0.5,-1.5)
plot(x,LoganFemale,col="red",type="l",ylim=c(-1.7,0.2),xlim=c(-12,12),ylab = "",xlab = "",lty=2,lwd=2)
legend(-5,-1.3,legend=c("Our findings using 2010 data","Logan et al. (2008) using 1988 data"),col=c("blue","red"),lwd=2,bty = "n",lty=1:2)

