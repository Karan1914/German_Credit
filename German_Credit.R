

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
GermanCredit <- read_excel("GermanCredit.xls")
head(GermanCredit)

str(GermanCredit)

colnames(GermanCredit$`OBS#`)
GermanCredit$`OBS#`<-NULL




cols <- c("CHK_ACCT","HISTORY","NEW_CAR","USED_CAR","FURNITURE","NUM_CREDITS","RADIO/TV","EDUCATION","RETRAINING","SAV_ACCT","EMPLOYMENT","PERSONAL_STATUS","CO-APPLICANT","GUARANTOR","PRESENT_RESIDENT","REAL_ESTATE","PROP_UNKN_NONE","OTHER_INSTALL","RENT","OWN_RES","JOB","TELEPHONE","FOREIGN","RESPONSE")
# MyData[cols] <- sapply(MyData[cols],as.factor()))
GermanCredit[,cols] <-  data.frame(apply(GermanCredit[cols], 2, as.factor))
GermanCredit$INSTALL_RATE <- as.numeric(GermanCredit$INSTALL_RATE)
# sapply(GermanCredit, class)
str(GermanCredit)










View(GermanCredit)
#GOOD : BAD ratio
table(GermanCredit$RESPONSE)
#700:300

#~~~~TREATING NA VALUES~~~~~~~*
sum(is.na(GermanCredit))
#Total 5374 missing values in the data
sum(is.na(GermanCredit$HISTORY))
#No NA values in History column
table(GermanCredit$USED_CAR)
summary(GermanCredit$NEW_CAR)
#If we notice carefully, All the variables like Used Car, new Cars, Furniture, Radio/TV, Education, Retraining which have NA in them only has 1 as their values and there are no 0 values in them
#So, we replace NA values by 0
GermanCredit$NEW_CAR[is.na(GermanCredit$NEW_CAR)] <- 0
GermanCredit$USED_CAR[is.na(GermanCredit$USED_CAR)] <- 0
GermanCredit$FURNITURE[is.na(GermanCredit$FURNITURE)] <- 0
GermanCredit$'RADIO/TV'[is.na(GermanCredit$'RADIO/TV')] <- 0
GermanCredit$EDUCATION[is.na(GermanCredit$EDUCATION)] <- 0
GermanCredit$RETRAINING[is.na(GermanCredit$RETRAINING)] <- 0
#Sum of NA values to check if the above code worked
sum(is.na(GermanCredit$NEW_CAR),GermanCredit$USED_CAR)
table(GermanCredit$USED_CAR)
summary(GermanCredit)
#Now, Lets see the variable Personal status
table(GermanCredit$PERSONAL_STATUS)
sum(is.na(GermanCredit$PERSONAL_STATUS))
#If we look at the NA values in Personal Status(It can have values 1,2,3), Assuming that these NA values could be by single people.
#We simply replace NA by 1 instead of 2 and 3. [NOT SURE THOUGH]
GermanCredit$PERSONAL_STATUS[is.na(GermanCredit$PERSONAL_STATUS)] <- 1
#There are 9 more NA values left in the AGE variable. Lets replace those values by the Median
GermanCredit$AGE[is.na(GermanCredit$AGE)] <- median(GermanCredit$AGE, na.rm=TRUE)

# *~~~~~~NOW ALL THE NA VALUES HAVE BEEN TREATED IN THE DATA SET~~~~~*

#Check the frequency of the categorical Variables
library(plyr)
count(GermanCredit[2])
count(GermanCredit[4])
count(GermanCredit[12])
count(GermanCredit[13])
count(GermanCredit[15])
count(GermanCredit[18])
count(GermanCredit[26])

#Create Barplots of the variables
barplot(table(GermanCredit$HISTORY), ylab = "Numbers", beside=TRUE)
library(ggplot2)        
ggplot(data = GermanCredit, aes(GermanCredit$HISTORY))

str(GermanCredit)



#   How to convert a variable to different data type?

str(GermanCredit)


##################################################################################

