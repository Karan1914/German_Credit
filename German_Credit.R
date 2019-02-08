

rm(list=ls())
#   Data Exploration in R
#   How to load data file(s)?
#   How to convert a variable to different data type?
#   How to transpose a table?
#   How to sort Data?
#   How to create plots (Histogram, Scatter, Box Plot)?
#   How to generate frequency tables?
#   How to do sampling of Data set?
#   How to remove duplicate values of a variable?
#   
#   How to group variables to calculate count, average, sum?
#   How to recognize and treat missing values and outliers?
#   How to merge / join data set effectively?



#preparatory work in R

getwd()

setwd("/Users/karan/Downloads/Data_mining/Assignment1/Data")



#   How to load data file(s)?
# 
# In R Input data sets can be of various formats such as (TXT, .CSV,.XLS, JSON )
# Loading Data is very easy in R
# Here , we will use csv data file to load using read.csv function

# #Read a Tab seperated file
# Tabseperated <- read.table("c:/TheDataIWantToReadIn.txt", sep="\t", header=TRUE)

install.packages("readxl")
library("readxl")

# Read CSV into R
MyData <- read_excel("GermanCredit.xls")
head(MyData)

str(MyData)


#   How to convert a variable to different data type?

str(MyData)

colnames(MyData$`OBS#`)
MyData$`OBS#`<-NULL

cols <- c("CHK_ACCT","HISTORY","NEW_CAR","USED_CAR","FURNITURE","NUM_CREDITS","RADIO/TV","EDUCATION","RETRAINING","SAV_ACCT","EMPLOYMENT","PERSONAL_STATUS","CO-APPLICANT","GUARANTOR","PRESENT_RESIDENT","REAL_ESTATE","PROP_UNKN_NONE","OTHER_INSTALL","RENT","OWN_RES","JOB","TELEPHONE","FOREIGN","RESPONSE")
# MyData[cols] <- sapply(MyData[cols],as.factor()))
MyData[,cols] <-  data.frame(apply(MyData[cols], 2, as.factor))
MyData$INSTALL_RATE <- as.numeric(MyData$INSTALL_RATE)
# sapply(MyData, class)
str(MyData)

##################################################################################

