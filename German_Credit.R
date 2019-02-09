
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

rm(list=ls())

#preparatory work in R

getwd()

# setwd("/Users/karan/Downloads/Data_mining/Assignment1/Data")

#Load Libraries:
install.packages("readxl")
install.packages("plyr")
install.packages("dplyr")
library(plyr)
library(readxl)
library(dplyr)
library(psych)
library(corrplot)
library(rpart)



#   How to load data file(s)?
# 
# In R Input data sets can be of various formats such as (TXT, .CSV,.XLS, JSON )
# Loading Data is very easy in R
# Here , we will use csv data file to load using read.csv function

# #Read a Tab seperated file
# Tabseperated <- read.table("c:/TheDataIWantToReadIn.txt", sep="\t", header=TRUE)

######################################### Data loading and view of the dataset ######################################### 

# Read XLS into R
GermanCredit <- read_excel("GermanCredit.xls")
# a quick glimpse of the dataset
head(GermanCredit)

# structure of the dataset
str(GermanCredit)

# View of the complete dataset
View(GermanCredit)

###################################################################################################################### 



######################################### Conversion of the Variables to different datatype ################################## 
#   How to convert a variable to different data type?
str(GermanCredit)

#  Remove 1st column as it is not required for our analysis 
colnames(GermanCredit$`OBS#`)
GermanCredit$`OBS#`<-NULL

# Conversion of numeric Data type variables to Categorical (Factor) Variables
cols <- c("CHK_ACCT","HISTORY","NEW_CAR","USED_CAR","FURNITURE","NUM_CREDITS","RADIO/TV","EDUCATION","RETRAINING","SAV_ACCT","EMPLOYMENT","PERSONAL_STATUS","CO-APPLICANT","GUARANTOR","PRESENT_RESIDENT","REAL_ESTATE","PROP_UNKN_NONE","OTHER_INSTALL","RENT","OWN_RES","JOB","TELEPHONE","FOREIGN","RESPONSE")
# MyData[cols] <- sapply(MyData[cols],as.factor()))
GermanCredit[,cols] <-  data.frame(apply(GermanCredit[cols], 2, as.factor))
# sapply(GermanCredit, class)
str(GermanCredit)

#################################################################################################################################


# Data Wrangling 

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
GermanCredit$PERSONAL_STATUS[is.na(GermanCredit$PERSONAL_STATUS)] <- 4
#There are 9 more NA values left in the AGE variable. Lets replace those values by the Median
GermanCredit$AGE[is.na(GermanCredit$AGE)] <- median(GermanCredit$AGE, na.rm=TRUE)

# *~~~~~~NOW ALL THE NA VALUES HAVE BEEN TREATED IN THE DATA SET~~~~~*

#Check the frequency of the categorical Variables

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
#################################################################################################################################


####### Univariate Analysis of Numerical Variables (duration,amount,age,num_dependents,install_rate) ########

#################################################################################################################################


str(GermanCredit)
describe(GermanCredit$DURATION)
describe(GermanCredit$AMOUNT)
describe(GermanCredit$NUM_DEPENDENTS)
describe(GermanCredit$INSTALL_RATE)
describe(GermanCredit$AGE)

#################################################################################################################################



#################################################################################################################################

####################### Graphical univariate analysis for dependent variable Response ########################################

#################################################################################################################################



tab<-table(GermanCredit$RESPONSE)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "Response", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "Response", 
        ylab = "Frequency", 
        col=c("orange", "steelblue"), 
        ylim=c(0,1))
box()


#Graphical univariate analysis for independent variable CHK_ACCT
tab<-table(GermanCredit$CHK_ACCT)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "CHK_ACCT", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "CHK_ACCT", 
        ylab = "Frequency", 
        col=rainbow(7), 
        ylim=c(0,1))
box()

#Graphical univariate analysis for independent variable HISTORY
tab<-table(GermanCredit$HISTORY)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "HISTORY", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "HISTORY", 
        ylab = "Frequency", 
        col=rainbow(7), 
        ylim=c(0,1))
box()

#Graphical univariate analysis for independent variable NEW_CAR
tab<-table(GermanCredit$NEW_CAR)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "NEW_CAR", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "NEW_CAR", 
        ylab = "Frequency", 
        col=rainbow(7), 
        ylim=c(0,1))
box()

#Graphical univariate analysis for independent variable USED_CAR
tab<-table(GermanCredit$USED_CAR)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "USED_CAR", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "USED_CAR", 
        ylab = "Frequency", 
        col=rainbow(7), 
        ylim=c(0,1))
box()


#Graphical univariate analysis for independent variable FURNITURE
tab<-table(GermanCredit$FURNITURE)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "FURNITURE", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "FURNITURE", 
        ylab = "Frequency", 
        col=rainbow(7), 
        ylim=c(0,1))
box()


#Graphical univariate analysis for independent variable RADIO/TV
tab<-table(GermanCredit$`RADIO/TV`)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "RADIO/TV", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "RADIO/TV", 
        ylab = "Frequency", 
        col=rainbow(7), 
        ylim=c(0,1))
box()


#Graphical univariate analysis for independent variable EDUCATION
tab<-table(GermanCredit$EDUCATION)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "EDUCATION", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "EDUCATION", 
        ylab = "Frequency", 
        col=rainbow(7), 
        ylim=c(0,1))
box()


#Graphical univariate analysis for independent variable RETRAINING
tab<-table(GermanCredit$RETRAINING)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "RETRAINING", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "RETRAINING", 
        ylab = "Frequency", 
        col=rainbow(7), 
        ylim=c(0,1))
box()

summary(GermanCredit$RETRAINING)
#################################################################################################################################



#################################################################################################################################

############################  Adding Decision Tree for the Dataset #############################

#################################################################################################################################


rpmodel<-rpart(GermanCredit$RESPONSE~. , data= GermanCredit, method = "class")
rpart.plot::prp(rpmodel, type=2, extra=1)

#################################################################################################################################
