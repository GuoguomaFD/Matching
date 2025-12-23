# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

## Explainations of variables can be found https://usa.ipums.org/usa-action/variables/SPLOC#description_section

# SPLOC is a constructed variable that indicates whether the person's spouse lived in the same household 
# and, if so, gives the person number (PERNUM) of the spouse. 

# RACE code
# 1	White	X
# 2	Black/African American	X
# 3	American Indian or Alaska Native	X
# 4	Chinese	X
# 5	Japanese	X
# 6	Other Asian or Pacific Islander	X
# 7	Other race, nec	X
# 8	Two major races	X
# 9	Three or more major races

# Education

# 00	N/A or no schooling	X
# 01	Nursery school to grade 4	X
# 02	Grade 5, 6, 7, or 8	X
# 03	Grade 9	X
# 04	Grade 10	X
# 05	Grade 11	X
# 06	Grade 12	X
# 07	1 year of college	X
# 08	2 years of college	X
# 09	3 years of college	·
# 10	4 years of college	X
# 11	5+ years of college	X

## SEX
## Male 1
## Female 2

# MARRINYR 
# 0	N/A	X
# 1	Blank (No)	X
# 2	Yes	X

# MARST Marrital status
# 1	Married, spouse present	X
# 2	Married, spouse absent	X
# 3	Separated	X
# 4	Divorced	X
# 5	Widowed	X
# 6	Never married/single	X



###################################################
## Download 2019 1% ACS data from IPUMSR website

## XXX is the location of your working folder
setwd("XXX")

rm(list=ls())

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00002.xml")
data <- read_ipums_micro(ddi)


## get summary statistics of males education

numberVec<-table(data$EDUC[data$SEX%in%1])
numberVec<-c(numberVec[1:8],0,numberVec[9:11])

EduDistri<-table(data$EDUC[data$SEX%in%1])/sum(table(data$EDUC[data$SEX%in%1]))

## add zero for choice 9

YearsofEdu<-c(0,2,6.5,9,10,11,12,13,14,15,16,17)

AverageYearsbelowhighschool<-sum(numberVec[1:7]/sum(numberVec[1:7])*YearsofEdu[1:7])
AverageYearsbelowhighschool
#8.55044
AverageYearsCollege<-sum(numberVec[8:12]/sum(numberVec[8:12])*YearsofEdu[8:12])
AverageYearsCollege
# 15.25902

#write.csv(names(data),"Variables extracted based on 2019 1% data.csv")
## education 
## 1 no school and grade 8 and below and some high school, around 21%+8%
## 2 grade 12, around 29%
## 3 some college, around 18%
## 4 college 13% and graduate 7.7%

data$EDUVar<-NA
data$EDUVar[data$EDUC<=5]<-1
data$EDUVar[data$EDUC%in%6]<-2
data$EDUVar[data$EDUC%in%c(7,8)]<-3
## there is no 3 years of college
data$EDUVar[data$EDUC%in%c(10,11)]<-4

table(data$RACE)/length(data$YEAR)*100
## Race
## 1 white 77.6%
## 2 black 10.4%
## 3 Asian and pacific 6%
## 4 remaining

data$RACEVar<-NA
data$RACEVar[data$RACE%in%1]<-1
data$RACEVar[data$RACE%in%2]<-2
data$RACEVar[data$RACE%in%c(4:6)]<-3
data$RACEVar[is.na(data$RACEVar)]<-4


## Age

## check for those married the age distribution
temp<-data[data$MARRINYR%in%2,]
summary(temp$AGE[temp$SEX%in%1])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.00   26.00   31.00   35.13   42.00   94.00 
summary(temp$AGE[temp$SEX%in%2])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 15.00   24.00   29.00   32.58   38.00   93.00 

data$AgeVar<-NA
data$AgeVar[data$SEX%in%1&data$AGE<26]<-1
data$AgeVar[data$SEX%in%2&data$AGE<24]<-1
data$AgeVar[data$SEX%in%1&data$AGE>=26&data$AGE<31]<-2
data$AgeVar[data$SEX%in%2&data$AGE>=24&data$AGE<29]<-2
data$AgeVar[data$SEX%in%1&data$AGE>=31&data$AGE<42]<-3
data$AgeVar[data$SEX%in%2&data$AGE>=29&data$AGE<38]<-3
data$AgeVar[data$SEX%in%1&data$AGE>=42&data$AGE<=95]<-4
data$AgeVar[data$SEX%in%2&data$AGE>=38&data$AGE<=95]<-4

## the final grouping 4 age groups, 4 RACE groups, and 4 education groups
## 64 groups

#####################
## shrink the groups to calculate the black male shares
## 3 age groups, 3 Race and 2 education

MaleEduAgeRace2019<-as.data.frame(matrix(0,nrow=18,ncol=1))
row.names(MaleEduAgeRace2019)[1:3]<-paste("White","HighSch","Age",1:3,sep = "")
row.names(MaleEduAgeRace2019)[4:6]<-paste("White","College","Age",1:3,sep = "")
row.names(MaleEduAgeRace2019)[7:9]<-paste("Black","HighSch","Age",1:3,sep = "")
row.names(MaleEduAgeRace2019)[10:12]<-paste("Black","College","Age",1:3,sep = "")
row.names(MaleEduAgeRace2019)[13:15]<-paste("Others","HighSch","Age",1:3,sep = "")
row.names(MaleEduAgeRace2019)[16:18]<-paste("Others","College","Age",1:3,sep = "")


# Count the Ending available males
## MARST Marrital status 4, 5, 6

temp<-data[data$SEX%in%1&data$MARST%in%c(4,5,6),]

MaleEduAgeRace2019[1,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%1])
MaleEduAgeRace2019[2,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%c(2,3)])
MaleEduAgeRace2019[3,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%4])

MaleEduAgeRace2019[4,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%1])
MaleEduAgeRace2019[5,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%c(2,3)])
MaleEduAgeRace2019[6,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%4])

MaleEduAgeRace2019[7,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%1])
MaleEduAgeRace2019[8,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%c(2,3)])
MaleEduAgeRace2019[9,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%4])

MaleEduAgeRace2019[10,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%1])
MaleEduAgeRace2019[11,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%c(2,3)])
MaleEduAgeRace2019[12,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%4])

MaleEduAgeRace2019[13,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%1])
MaleEduAgeRace2019[14,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%c(2,3)])
MaleEduAgeRace2019[15,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%4])

MaleEduAgeRace2019[16,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%1])
MaleEduAgeRace2019[17,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%c(2,3)])
MaleEduAgeRace2019[18,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%4])


MaleEduAgeRaceEnding2019<-MaleEduAgeRace2019

#####################
## shrink the groups to calculate the black Female shares
## 3 age groups, 3 Race and 2 education

FemaleEduAgeRace2019<-as.data.frame(matrix(0,nrow=18,ncol=1))
row.names(FemaleEduAgeRace2019)[1:3]<-paste("White","HighSch","Age",1:3,sep = "")
row.names(FemaleEduAgeRace2019)[4:6]<-paste("White","College","Age",1:3,sep = "")
row.names(FemaleEduAgeRace2019)[7:9]<-paste("Black","HighSch","Age",1:3,sep = "")
row.names(FemaleEduAgeRace2019)[10:12]<-paste("Black","College","Age",1:3,sep = "")
row.names(FemaleEduAgeRace2019)[13:15]<-paste("Others","HighSch","Age",1:3,sep = "")
row.names(FemaleEduAgeRace2019)[16:18]<-paste("Others","College","Age",1:3,sep = "")


# Count the Ending available Females
## MARST Marrital status 4, 5, 6

temp<-data[data$SEX%in%2&data$MARST%in%c(4,5,6),]

FemaleEduAgeRace2019[1,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%1])
FemaleEduAgeRace2019[2,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%c(2,3)])
FemaleEduAgeRace2019[3,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%4])

FemaleEduAgeRace2019[4,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%1])
FemaleEduAgeRace2019[5,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%c(2,3)])
FemaleEduAgeRace2019[6,1]<-length(temp$YEAR[temp$RACEVar%in%1&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%4])

FemaleEduAgeRace2019[7,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%1])
FemaleEduAgeRace2019[8,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%c(2,3)])
FemaleEduAgeRace2019[9,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%4])

FemaleEduAgeRace2019[10,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%1])
FemaleEduAgeRace2019[11,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%c(2,3)])
FemaleEduAgeRace2019[12,1]<-length(temp$YEAR[temp$RACEVar%in%2&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%4])

FemaleEduAgeRace2019[13,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%1])
FemaleEduAgeRace2019[14,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%c(2,3)])
FemaleEduAgeRace2019[15,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(1,2)&temp$AgeVar%in%4])

FemaleEduAgeRace2019[16,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%1])
FemaleEduAgeRace2019[17,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%c(2,3)])
FemaleEduAgeRace2019[18,1]<-length(temp$YEAR[temp$RACEVar%in%c(3,4)&temp$EDUVar%in%c(3,4)&temp$AgeVar%in%4])


FemaleEduAgeRaceEnding2019<-FemaleEduAgeRace2019

#############################
## Married Matrix
## Col is female and Row is males

MarriedMat<-as.data.frame(matrix(0,nrow=18,ncol=18))

row.names(MarriedMat)[1:3]<-paste("MWhite","HighSch","Age",1:3,sep = "")
row.names(MarriedMat)[4:6]<-paste("MWhite","College","Age",1:3,sep = "")
row.names(MarriedMat)[7:9]<-paste("MBlack","HighSch","Age",1:3,sep = "")
row.names(MarriedMat)[10:12]<-paste("MBlack","College","Age",1:3,sep = "")
row.names(MarriedMat)[13:15]<-paste("MOthers","HighSch","Age",1:3,sep = "")
row.names(MarriedMat)[16:18]<-paste("MOthers","College","Age",1:3,sep = "")

names(MarriedMat)[1:3]<-paste("FWhite","HighSch","Age",1:3,sep = "")
names(MarriedMat)[4:6]<-paste("FWhite","College","Age",1:3,sep = "")
names(MarriedMat)[7:9]<-paste("FBlack","HighSch","Age",1:3,sep = "")
names(MarriedMat)[10:12]<-paste("FBlack","College","Age",1:3,sep = "")
names(MarriedMat)[13:15]<-paste("FOthers","HighSch","Age",1:3,sep = "")
names(MarriedMat)[16:18]<-paste("FOthers","College","Age",1:3,sep = "")


table(data$MARST)/length(data$YEAR)*100
# 1         2         3         4         5         6 
# 41.639721  1.472617  1.558746  8.611121  5.554837 41.162958 
# ignore 2 and 3 as no info of spouse can be found

## Married in the past year
temp<-data[data$MARRINYR%in%2,]

## adjust the Edu to two category
temp$EDUVar[temp$EDUVar%in%c(1,2)]<-1
temp$EDUVar[temp$EDUVar%in%c(3,4)]<-2

## adjust Race to three category too
temp$RACEVar[temp$RACEVar%in%c(3:4)]<-3

## adjust age to three category
temp$AgeVar[temp$AgeVar%in%c(2,3)]<-2
temp$AgeVar[temp$AgeVar%in%4]<-3


## SERIAL is household serial number
## SPLOC gives the spouse's PERNUM

for (i in 1:length(temp$YEAR))
{
  Select<-temp[i,]
  SpouseSelect<-temp[temp$SERIAL%in%Select$SERIAL,]
  SpouseSelect<-SpouseSelect[SpouseSelect$PERNUM%in%Select$SPLOC,]
  
  Pointer<-6*(Select$RACEVar-1)+3*(Select$EDUVar-1)+Select$AgeVar
  SpousePointer<-6*(SpouseSelect$RACEVar-1)+3*(SpouseSelect$EDUVar-1)+SpouseSelect$AgeVar
  
  if (Select$SEX%in%1) 
  {
    Rowpointer<-Pointer
    Colpointer<-SpousePointer
  }
  if (Select$SEX%in%2) 
  {
    Rowpointer<-SpousePointer
    Colpointer<-Pointer
  }
  MarriedMat[Rowpointer,Colpointer]<-MarriedMat[Rowpointer,Colpointer]+1
  
  print(i)
}

## As we count both husband and wife, we need to reduce half of the observations

MarriedMat<-MarriedMat/2

## remove NA rows
MarriedMat<-MarriedMat[1:18,1:18]

FemaleEduAgeRaceBeginning2019<-FemaleEduAgeRaceEnding2019+t(colSums(MarriedMat))
MaleEduAgeRaceBeginning2019<-MaleEduAgeRaceEnding2019+t(rowSums(MarriedMat))

write.csv(FemaleEduAgeRaceBeginning2019,"FemaleEudAgeRaceBeginning2019BlackMale.csv")
write.csv(FemaleEduAgeRaceEnding2019,"FemaleEudAgeRaceEnding2019BlackMale.csv")
write.csv(MaleEduAgeRaceBeginning2019,"MaleEudAgeRaceBeginning2019BlackMale.csv")
write.csv(MaleEduAgeRaceEnding2019,"MaleEudAgeRaceEnding2019BlackMale.csv")
write.csv(MarriedMat,"MarriedMat2019BlackMale.csv")


### check the relative share of males to the white high school young group
## this share can be used to simulate the choice set effect

aa<-MaleEduAgeRaceBeginning2019/MaleEduAgeRaceBeginning2019[1,1]
aa

## Compare relative to female group

bb<-MaleEduAgeRaceBeginning2019/FemaleEduAgeRaceBeginning2019
bb

write.csv(aa,"2019 Ratio of Males by groups to WhiteHighSchAge1.csv")
write.csv(bb,"2019 Ratio of Males to Females by group.csv")
