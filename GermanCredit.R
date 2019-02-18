

############################
# LOAD LIBRARIES AND DATASET

rm(list=ls())
getwd()

# Working directory for James
setwd("/Users/karan/Documents/Data_mining_jan/datasets")

# # Working directory for Karan
#setwd("/Users/karan/Documents/Data_mining_jan/datasets")

#Load Libraries:
install.packages("readxl")
install.packages('cowplot')
install.packages("plyr")
install.packages("dplyr")
install.packages("C50")
install.packages("caret")
install.packages("e1071")
library(C50)
library(caret)
library(e1071)
library(plyr)
library(readxl)
library(dplyr)
library(psych)
library(corrplot)
library(rpart)
library(lift)
library(gdata)
library (caret)
library(ggplot2)  
library(cowplot)


# Read XLS into R
GermanCredit <- read_excel("GermanCredit.xls")

head(GermanCredit)    # a quick glimpse of the dataset

str(GermanCredit)     # structure of the dataset

summary(GermanCredit)    # View of the complete dataset

# CLEAN THE DATA
################# Remove unnecessary column 'Observation' by converting to column name ########################### 
colnames(GermanCredit$`OBS#`)
GermanCredit$`OBS#`<-NULL
GermanCredit<-rename.vars(GermanCredit,from = "CO-APPLICANT",to="COAPPLICANT")
GermanCredit<-rename.vars(GermanCredit,from = "RADIO/TV",to="RADIO")
GermanCredit<-rename.vars(GermanCredit,from = "CHK_ACCT",to="CHKACCT")

################# Treat NA/Missing values #######################################################

# There are many missing values in the data. We also need to convert many variables from numeric to factor. To make conversion
# easier, we will first fill the missing values for the variables, then convert the categorical variables from numeric
# to factors. 

sum(is.na(GermanCredit))     # Total 5374 missing values in the data


summary(GermanCredit)
# If we notice carefully, many of the variables like Used Car, new Cars, Furniture, Radio/TV, Education, Retraining 
# which have NA in them only has 1 as their values, and there are no 0 values in them
# So, we replace NA values by 0
GermanCredit$NEW_CAR[is.na(GermanCredit$NEW_CAR)] <- 0
GermanCredit$USED_CAR[is.na(GermanCredit$USED_CAR)] <- 0
GermanCredit$FURNITURE[is.na(GermanCredit$FURNITURE)] <- 0
GermanCredit$RADIO[is.na(GermanCredit$RADIO)] <- 0
GermanCredit$EDUCATION[is.na(GermanCredit$EDUCATION)] <- 0
GermanCredit$RETRAINING[is.na(GermanCredit$RETRAINING)] <- 0

summary(GermanCredit)

#Now, Lets see the variable Personal status
table(GermanCredit$PERSONAL_STATUS)
sum(is.na(GermanCredit$PERSONAL_STATUS))
#If we look at the NA values in Personal Status(It can have values 1,2,3), and many 310 NA values. Because there 
# might be information contained in the lack of an applicant listing Personal Status, we will create a fourth value
# and assign all NA values this value (4)
GermanCredit$PERSONAL_STATUS[is.na(GermanCredit$PERSONAL_STATUS)] <- 4
#There are 9 more NA values left in the AGE variable. Lets replace those values by the Median
GermanCredit$AGE[is.na(GermanCredit$AGE)] <- median(GermanCredit$AGE, na.rm=TRUE)
#Let's confirm that all NAs have been removed
sum(is.na(GermanCredit))

# *~~~~~~NOW ALL THE NA VALUES HAVE BEEN TREATED IN THE DATA SET~~~~~*

############## Convert certain numeric data type variables to categorical (factor) variables ######################

cols <- c("CHKACCT","HISTORY","NEW_CAR","USED_CAR","FURNITURE","NUM_CREDITS","RADIO","EDUCATION","RETRAINING","SAV_ACCT","EMPLOYMENT","PERSONAL_STATUS","COAPPLICANT","GUARANTOR","PRESENT_RESIDENT","REAL_ESTATE","PROP_UNKN_NONE","OTHER_INSTALL","RENT","OWN_RES","JOB","TELEPHONE","FOREIGN","RESPONSE")
# MyData[cols] <- sapply(MyData[cols],as.factor()))
GermanCredit[,cols] <-  data.frame(apply(GermanCredit[cols], 2, as.factor))
# sapply(GermanCredit, class)
str(GermanCredit)

# The data is ready to examine

################## DATA EXPLORATION #############################################

# Our dependent variable, GOOD : BAD ratio
table(GermanCredit$RESPONSE)
#700:300

#Check the frequency of the categorical Variables

count(GermanCredit[1])
count(GermanCredit[4])
count(GermanCredit[12])
count(GermanCredit[13])
count(GermanCredit[15])
count(GermanCredit[18])
count(GermanCredit[26])

#Create Barplots of the variables
barplot(table(GermanCredit$HISTORY), ylab = "Numbers", beside=TRUE)
     
ggplot(data = GermanCredit, aes(GermanCredit$HISTORY)) + geom_bar() 

str(GermanCredit)



####### Univariate Analysis of Numerical Variables (duration,amount,age,num_dependents,install_rate) ########

str(GermanCredit)
describe(GermanCredit$DURATION)
describe(GermanCredit$AMOUNT)
describe(GermanCredit$NUM_DEPENDENTS)
describe(GermanCredit$INSTALL_RATE)
describe(GermanCredit$AGE)


####################### Graphical univariate analysis for dependent variable Response ########################################

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


#Graphical univariate analysis for independent variable CHKACCT
tab<-table(GermanCredit$CHKACCT)
ptab<-prop.table(tab)
#output
#now the table seems to be have better distribution
#Barplot
barplot(tab, main = "Bar Plot", xlab = "CHKACCT", ylab = "Frequency")

barplot(ptab, main = "Bar Plot", 
        xlab = "CHKACCT", 
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
tab<-table(GermanCredit$RADIO)
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

#####################################################################################################

# BIVARIATE ANALYSIS DATA EXPLORATION WITH DEPENDENT VARIABLE 'RESPONSE'

#bivariate analysis of numeric independent variables (DURATION, AMOUNT, INSTALL_RATE, AGE) vs. factor dependent variable (RESPONSE)


# check the visuals...boxplot and density curve
a <- GermanCredit %>% ggplot(aes(RESPONSE, DURATION, fill = RESPONSE))+
  geom_boxplot() +
  ggtitle("DURATION")
b <- GermanCredit %>% ggplot(aes(RESPONSE, AMOUNT, fill = RESPONSE))+
  geom_boxplot() +
  ggtitle("AMOUNT")
c <- GermanCredit %>% ggplot(aes(RESPONSE, INSTALL_RATE, fill = RESPONSE))+
  geom_boxplot() +
  ggtitle("INSTALL_RATE")
d <- GermanCredit %>% ggplot(aes(RESPONSE, AGE, fill = RESPONSE))+
  geom_boxplot() +
  ggtitle("AGE")
plot_grid(a, b, c, d)

e <- GermanCredit %>% ggplot(aes(DURATION, fill=RESPONSE)) +
  geom_density(alpha = .5, adjust = 2) + 
  xlab("DURATION") +  ylab("Frequency") + 
  ggtitle("DURATION")
f <- GermanCredit %>% ggplot(aes(AMOUNT, fill=RESPONSE)) +
  geom_density(alpha = .5, adjust = 2) + 
  xlab("AMOUNT") +  ylab("Frequency") + 
  ggtitle("AMOUNT")
g <- GermanCredit %>% ggplot(aes(INSTALL_RATE, fill=RESPONSE)) +
  geom_density(alpha = .5, adjust = 2) + 
  xlab("INSTALL-RATE") +  ylab("Frequency") + 
  ggtitle("INSTALL-RATE")
h <- GermanCredit %>% ggplot(aes(AGE, fill=RESPONSE)) +
  geom_density(alpha = .5, adjust = 2) + 
  xlab("AGE") +  ylab("Frequency") + 
  ggtitle("AGE")
plot_grid(e, f, g, h)


# The boxplots and density curves indicate that there is a different distributions for relationship between DURATION and RESPONSE, with an obvious difference in median and IQR and the shape of the distribution. Longer duration loans tend to have worse credit. AMOUNT also shows a difference albiet likely smaller, suggesting a relationship - good credit for larger amounts.

# Let's check with an ANOVA test for each numeric variable against the dependent variable RESPONSE.
summary(aov(DURATION~RESPONSE, data = GermanCredit))[[1]]$'Pr(>F)'
summary(aov(AMOUNT~RESPONSE, data = GermanCredit))[[1]]$'Pr(>F)'
summary(aov(INSTALL_RATE~RESPONSE, data = GermanCredit))[[1]]$'Pr(>F)'
summary(aov(AGE~RESPONSE, data = GermanCredit))[[1]]$'Pr(>F)'

# The ANOVA tests confirm the idea that the distribution of RESPONSE is related to all the numeric variables. 
# Likely, DURATION and possibly AMOUNT are the most closely related, and will be more important in our decision tree.

# BIVARIATE ANALYSIS OF FACTOR VARIABLES VS. DEPENDENT FACTOR VARIABLE 'RESPONSE'
# Let's also run a chi-squared test on certain factor variables, and see which variables seem statistically related to RESPONSE.
chisq.test(GermanCredit$RESPONSE, GermanCredit$CHKACCT)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$HISTORY)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$NEW_CAR)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$USED_CAR)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$FURNITURE)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$RADIO)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$EDUCATION)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$RETRAINING)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$SAV_ACCT)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$EMPLOYMENT)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$PERSONAL_STATUS)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$COAPPLICANT)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$GUARANTOR)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$PRESENT_RESIDENT)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$REAL_ESTATE)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$PROP_UNKN_NONE)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$OTHER_INSTALL)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$RENT)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$OWN_RES)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$NUM_CREDITS)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$JOB)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$NUM_DEPENDENTS)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$TELEPHONE)$p.value
chisq.test(GermanCredit$RESPONSE, GermanCredit$FOREIGN)$p.value

# From these chi-squared tests, we can see that 
# FURNITURE, RETRAINING, CO-APPLICANT, GUARANTOR, PRESENT RESIDENT, JOB, NUMBER OF DEPENDENTS, 
# and TELEPHONE do not have statistically significant relationships, with pvalues > .05.
# So likely, these factors will not be important in our decision tree.


##########                      QUESTION 2.                        ##########                      

#############################################################################


# Q2(a)  Develop a rpart decision tree model on testing data only, and check parameters.

rpModel1=rpart(RESPONSE ~ ., data=GermanCredit, method="class")
#print the model -- text form
print(rpModel1)
#plot the model
rpart.plot::prp(rpModel1, type=2, extra=1)




# Q2(b)
#Which variables do you think will be most relevant for the outcome of interest, how do you determine these, and do they match expectations?
# evaluate the model
# General purpose CART decision tree procedures (rpart included) do not attempt to find the tree that best fits the data.
# Instead they simply attempt to find a "good" tree by searching greedily.
# Greedy means that, any time a split is under consideration,
# the best split is returned without considering any information from potential future splits. 
# https://stats.stackexchange.com/questions/222081/which-algorithm-can-learn-exactly-a-tree-structure-without-noise/222125#222125

# visually, the rpart tree we generated with the full GermanCredit data uses CHKACCT, DURATION, HISTORY,  AMOUNT, SAV-ACCT, USED_CAR, NUM-CRED, and GUARANTOR.
# We can see that they are likely displayed graphically in approximately descending order. 

# With respect to our numerical variables, this matches our expectations from our bivariate analysis, 
# where we predicted that DURATION and AMOUNT showed a relationship with RESPONSE. 

summary(rpModel1)
# Running summary on the decision tree model, 
# the 'Variable importance' confirms our intuitions from the data exploration. 
# CHK_ACCT, DURATION, HISTORY and SAV-ACCT are the most important variables. 
# After,there is a big drop in importance, and we can likely prune the remaining variable.


# Q2(c) check level of accuracy for model on all data. Obtain and interpret lift chart.

#Obtain the model's predictions on the full dataset
# Confusion matrix
cm <- table(pred=predict(rpModel1,GermanCredit, type="class"), true=GermanCredit$RESPONSE)
cm
# Accuracy on full data
a <- mean(predict(rpModel1,GermanCredit, type="class") == GermanCredit$RESPONSE)
a
# The model has an overall 79% accuracy on the full data.
# Accuracy on the good credit and bad credit, calculated from confusion matrix...

precisiongd <- a[2,2]/(a[2,2]+a[2,1]) # precision on good credit
precisiongd # Precision is 68.07%


recallgd <- a[2,2]/(a[2,2]+a[1,2]) # recall on good credit
recallgd # Recall is 59% 

f1 <- (2 * recallgd * precisiongd)/(recallgd + precisiongd)
f1 # F1 score is 63% on good credit

precisionbd <- a[1,1]/(a[1,1]+a[1,2]) #precision on bad credit
precisionbd # Precision is 88.15% on Bad Credit


recallbd <- a[1,1]/(a[1,1]+a[2,1]) # recall on bad credit
recallbd   # Recall is 83.37% on Bad Credit



f1 <- (2 * recallbd * precisionbd)/(recallbd + precisionbd)
f1   # F1 SCore is 85.70% on Bad Credit


# Create a lift chart
library('lift')
# First, get the 'scores' from applying the model to the data
predGermanCreditProb=predict(rpModel1, GermanCredit, type='prob')
head(predGermanCreditProb)
#we need the score and actual class (RESPONSE) values
GermanCreditSc <- subset(GermanCredit, select=c("RESPONSE"))  # selects the RESPONSE column into GermanCreditSc
GermanCreditSc['score']<-predGermanCreditProb[, 1]  #add a column named 'Score' with prob(default) values in the first column of predGermanCreditProb
head(GermanCreditSc)
#sort by score
GermanCreditSc<-GermanCreditSc[order(GermanCreditSc$score, decreasing=TRUE),]
#we will next generate the cumulaive sum of the RESPONSE values -- for this, we want 'bad credit'=1, and 'good credit'=0.   
# Notice that RESPONSE is a factor (ie. categorical) variable
str(GermanCreditSc)
levels(GermanCreditSc$RESPONSE)
# we need to convert these to numbers, and assign good credit = 1, and bad credit = 0.
levels(GermanCreditSc$RESPONSE)[0] <- 0
levels(GermanCreditSc$RESPONSE)[1] <- 1
# convert response to numeric data type
GermanCreditSc$RESPONSE<-as.numeric(as.character(GermanCreditSc$RESPONSE))
str(GermanCreditSc)
#obtain the cumulative sum of default cases captured
GermanCreditSc$cumBadCredit<-cumsum(GermanCreditSc$RESPONSE)
print(head(GermanCreditSc, 10))
#Plot the cumDefault values (y-axis) by numCases (x-axis)
plot(seq(nrow(GermanCreditSc)), GermanCreditSc$cumBadCredit,type = "l", xlab='#cases', ylab='#bad credit')
#value of lift in the top decile
TopDecileLift(GermanCreditSc$score, GermanCreditSc$RESPONSE)



# Question: Do we think this is a robust model? 

# Solution: 
# It is difficult to know, because we could only evaluate this by running the model on some testing data, 
# which was not used to create the model in the first place. 
# Without this, we do not know whether our model has high variance when used with new data.






####################### Question 3  ###########################
######################## CREATE TRAINING AND TESTING DATASETS ###########################
# 50-50 split
#split the data into training and test(validation) sets - 50% for training, rest for validation
nr=nrow(GermanCredit) # gets the number of rows
set.seed(123) # set seed for reproducible results
trnIndex = sample(1:nr, size = round(0.5*nr), replace=FALSE) #get a random 50%sample of row-indices
GermanCreditTrn=GermanCredit[trnIndex,]   #training data with the randomly selected row-indices
GermanCreditTst = GermanCredit[-trnIndex,]  #test data with the other row-indices
dim(GermanCreditTrn) 
dim(GermanCreditTst)


############### BUILD THE RPART DECISION TREE AND CHECK PERFORMANCE ####################################

# THIS IS OUR BEST RPART MODEL. YOU CAN CHANGE PARAMETERS AND REMOVE #COMMENT LINES, TO RUN TESTS
rpModel2=rpart(RESPONSE ~ ., data=GermanCreditTrn, method="class", control = rpart.control(cp = 0.03, minsplit = 50, minbucket = 10))
summary(rpModel2)
print(rpModel2)
#plot the model
rpart.plot::prp(rpModel2, type=2, extra=1)

# PERFORMANCE ANALYSIS OF RPART MODEL
#training data confusion table
table(pred = predict(rpModel2, GermanCreditTrn, type='class'), true=GermanCreditTrn$RESPONSE)
#Accuracy of model2 on training data
mean(predict(rpModel2,GermanCreditTrn, type="class") == GermanCreditTrn$RESPONSE)      #76%

#run the test data through model2, and create confusion matrix
cm <- table(pred=predict(rpModel2,GermanCreditTst, type="class"), true=GermanCreditTst$RESPONSE)
cm
#Check Overall Accuracy on testing data
a <- mean(predict(rpModel2,GermanCreditTst, type="class") == GermanCreditTst$RESPONSE)
a   # Overall Accuracy on Testing data = 73.4%


# check overall error rate
error.rate <- 1-a
error.rate
# check precision, recall and f1 on 'Good Credit' cases
precisiongd <- cm[2,2]/(cm[2,2]+cm[2,1]) # precision on good credit
precisiongd
recallgd <- cm[2,2]/(cm[2,2]+cm[1,2]) # recall on good credit
recallgd
f1 <- (2 * recallgd * precisiongd)/(recallgd + precisiongd)
f1
# check precision, recall and f1 on 'Bad Credit' cases
precisionbd <- cm[1,1]/(cm[1,1]+cm[1,2]) #precision on bad credit
precisionbd
recallbd <- cm[1,1]/(cm[1,1]+cm[2,1]) # recall on bad credit
recallbd
f1 <- (2 * recallbd * precisionbd)/(recallbd + precisionbd)
f1



############## Question 3 (b) #####################

######################## BUILD A C.50 DECISION TREE AND EVALUATE PERFORMANCE #######################



# THIS IS OUR BEST C50 MODEL. 
c50model <- C5.0(RESPONSE~., data = GermanCreditTrn, trials = 100, control = C5.0Control(minCases = 50, subset = T, CF = 0.25, winnow = F))
summary(c50model)
p <- predict(c50model, newdata = GermanCreditTst, type = 'class')
#c50accuracy = sum(p==GermanCreditTst$RESPONSE)/length(p)
#c50accuracy
# evaluate performance
confusionMatrix(p, GermanCreditTst$RESPONSE, positive = "1")
plot(c50model)

# create rules from the c5.0 model
rule_mod <- C5.0(RESPONSE~., data = GermanCreditTrn, rules = T, control = C5.0Control(minCases = 50, subset = T, CF = 0.25, winnow = F))
rule_mod
summary(rule_mod)
postResample(predict(rule_mod, GermanCreditTst), GermanCreditTst$RESPONSE)





#################### THIS CODE CONTAINS THRESHOLD CHANGE, LIFT AND ROC CURVES ##############

# Try different paramters and reevaluate performance....
## if we try different thresholds, the maximum accuracy seems to be about 0.5, 
#and results in the same accuracy as the default values in rpart.
#Here is an example using a different threshold of 0.2, resulting in a lower accuracy of about 60%

CTHRESH=0.3
predProbTrn=predict(rpModel2, GermanCreditTrn, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[, '0'] >= CTHRESH, '0', '1')
ct = table( pred = predTrn, true=GermanCreditTrn$RESPONSE)
ct
#Accuracy
mean(predTrn==GermanCreditTrn$RESPONSE)

# check lift curve, ROC, AUC

# CHECK THE LIFT CURVE
# First, get the 'scores' from applying the model to the data
predGermanCreditProb=predict(rpModel2, GermanCreditTst, type='prob')
head(predGermanCreditProb)
#we need the score and actual class (RESPONSE) values
GermanCreditSc <- subset(GermanCreditTst, select=c("RESPONSE"))  # selects the RESPONSE column into GermanCreditSc
GermanCreditSc$score<-predGermanCreditProb[, 1]  #add a column named 'Score' with prob(default) values in the first column of predGermanCreditProb
#sort by score
GermanCreditSc<-GermanCreditSc[order(GermanCreditSc$score, decreasing=TRUE),]
#we will next generate the cumulaive sum of the RESPONSE values -- for this, we want 'bad credit'=0, and 'good credit'=0.   Notice that RESPONSE is a factor (ie. categorical) variable
str(GermanCreditSc)
levels(GermanCreditSc$RESPONSE)
# convert response to numeric data type
GermanCreditSc$RESPONSE<-as.numeric(as.character(GermanCreditSc$RESPONSE))
str(GermanCreditSc)
#obtain the cumulative sum of default cases captured
GermanCreditSc$cumDefault<-cumsum(GermanCreditSc$RESPONSE)
head(GermanCreditSc)
#Plot the cumDefault values (y-axis) by numCases (x-axis)
plot(seq(nrow(GermanCreditSc)), GermanCreditSc$cumDefault,type = "l", xlab='#cases', ylab='#bad credit')
#value of lift in the top decile
TopDecileLift(GermanCreditSc$score, GermanCreditSc$RESPONSE)

# CHECK THE ROC CURVE
library('ROCR')
#obtain the scores from the model for the class of interest, here, the prob('0')
scoreTst=predict(rpModel1,GermanCreditTst, type="prob")[,'1']  
#same as predProbTst
#now apply the prediction function from ROCR to get a prediction object
rocPredTst = prediction(scoreTst, GermanCreditTst$RESPONSE, label.ordering = c('0', '1'))  

#obtain performance using the function from ROCR, then plot
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst)


###Optimal cutoff from ROC
costPerf = performance(rocPredTst, "cost")
rocPredTst@cutoffs[[1]][which.min(costPerf@y.values[[1]])]

###optimal cost with different costs for fp and fn
costPerf = performance(rocPredTst, "cost", cost.fp = 2, cost.fn = 1)
rocPredTst@cutoffs[[1]][which.min(costPerf@y.values[[1]])]


###other performance measures with the performance function
accPerf = performance(rocPredTst, measure = "acc")
plot(accPerf)

#AUC value
aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values

####Examine the ROCR prediction object
class(rocPredTst)
slotNames(rocPredTst)
sapply(slotNames(rocPredTst), function(x) class(slot(rocPredTst, x)))
sapply(slotNames(rocPredTst), function(x) length(slot(rocPredTst, x)))

#The prediction object has a set of slots with names as seen above.
#The slots hold values of predictions,  labels, cutoffs, fp values, ....etc.

####Similarly, examine the ROCR performance object
class(accPerf)
slotNames(accPerf)

accPerf@x.name   #values in the x.name slot
accPerf@y.name

#So the x.values give the cutoff values and the y.values goves the corresponding accuracy. 
#We can use these to find, for example, the cutoff corresponding to the maximum accuracy

####Optimal cutoff
#The accuracy values are in y.values slot of the acc.perf object. 
#This is a list, as seen by: 
class(accPerf@y.values)
#... and we can get the values through 
accPerf@y.values[[1]]

#get the index of the max value of accuracy
ind=which.max(accPerf@y.values[[1]])

#get the accuracy value coresponding to this index
acc = (accPerf@y.values[[1]])[ind]

#get the cutoff corresponding to this index
cutoff = (accPerf@x.values[[1]])[ind]

#show these results
print(c(accuracy= acc, cutoff = cutoff))

#Similarly, for the optimal ROC curve based cutoff (using the "cost" as performance measure), we can get the corresponding TP and FP values
costPerf = performance(rocPredTst, "cost")
optCutoff = rocPredTst@cutoffs[[1]][which.min(costPerf@y.values[[1]])]
optCutInd = which.max(perfROCTst@alpha.values[[1]] == optCutoff)

#get the ROC curve values, i.e. TPRate and FPRate for different cutoffs
perfROCTst=performance(rocPredTst, "tpr", "fpr")
#You should check the slotNames of perROCTst....the FPRate is in x.values and TPRate is in y.values

OptCutoff_FPRate = perfROCTst@x.values[[1]][optCutInd]
OptCutoff_TPRate = perfROCTst@y.values[[1]][optCutInd]
print(c(OptimalCutoff=optCutoff, FPRAte=OptCutoff_FPRate, TPRate =OptCutoff_TPRate))




############# Question 4 ###################


######################## CREATE TRAINING AND TESTING DATASETS ###########################
# 70-30 split
#split the data into training and test(validation) sets - 70% for training, rest for validation
nr=nrow(GermanCredit) # gets the number of rows
set.seed(123) # set seed for reproducible results
trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE) #get a random 70% sample of row-indices
GermanCreditTrn=GermanCredit[trnIndex,]   #training data with the randomly selected row-indices
GermanCreditTst = GermanCredit[-trnIndex,]  #test data with the other row-indices
dim(GermanCreditTrn) 
dim(GermanCreditTst)




###Calculating the Profits and cumulative profits
PROFITVAL=100
COSTVAL=-500

scoreTst=predict(rpModel2, GermanCreditTrn, type="prob")[,'1']
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, GermanCreditTrn$RESPONSE)
#check what is in prLifts ....head(prLifts)

prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`GermanCreditTrn$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))
head(prLifts)
plot(prLifts$cumProfits)

mean(scoreTst==prLifts$cumProfits)

#Calculate score
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))


CTHRESH=0.5
predProbTrn=predict(rpModel2, GermanCreditTst, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[, '0'] >= CTHRESH, '0', '1')
ct = table( pred = predTrn, true=GermanCreditTst$RESPONSE)
ct
#Accuracy
mean(predTrn==GermanCreditTst$RESPONSE)



# Model Performance for C5.0 model
c50model <- C5.0(RESPONSE~., data = GermanCreditTrn, trials = 100, control = C5.0Control(minCases = 50, subset = T, CF = 0.25, winnow = F))
summary(c50model)
p <- predict(c50model, newdata = GermanCreditTst, type = 'class')
c50accuracy = sum(p==GermanCreditTst$RESPONSE)/length(p)
c50accuracy
# evaluate performance
confusionMatrix(p, GermanCreditTst$RESPONSE, positive = "1")
plot(c50model)





######### Question 5 ##########

# Develop a full tree and then prune it

#The default parameters for rpart (see https://www.rdocumentation.org/packages/rpart/versions/4.1-13/topics/rpart.control)
#use cp=0.01 -- this prevent splits which do not improve fit by the specified value (0.01)
#We can instead grow a full tree and then prune based on the CP value which gives lowest cross-validated errors.
#Grow a tree, with cp=0
rpModel2cp = rpart(RESPONSE ~ ., data=GermanCreditTrn, method="class", control = rpart.control(cp = 0.0))
# this creates a larger tree
rpart.plot::prp(rpModel2cp, type=2, extra=1)
# check performance on test data
table(pred=predict(rpModel2cp,GermanCreditTst, type="class"), true=GermanCreditTst$RESPONSE)
#Accuracy of the full, unpruned model on testing data
mean(predict(rpModel2cp,GermanCreditTst, type="class") == GermanCreditTst$RESPONSE)
# our accuracy on the testing data fell to about 72%.
#we can then prune the tree using the best cp value
#first, check the errors for different levels of CP
printcp(rpModel2cp)
#plot the errors for different CP levels (https://www.rdocumentation.org/packages/rpart/versions/4.1-13/topics/plotcp)
plotcp(rpModel2cp)
#The best pruned tree can be considered as simplest model with error within 1 standard error of the minimum error #tree.
# The line in the plot above shows the 1 std err above the minimum error. 
# So, which cp level would you use for #the best pruned tree?
# 0.19 seems to be the best value of complexity
rpModel2_pruned = prune(rpModel2cp, cp=0.01)
#evaluate performance of pruned model
mean(predict(rpModel2_pruned,GermanCreditTst, type="class") == GermanCreditTst$RESPONSE)
# here we have increased overall performance from 72% to 74%, using complexity 0.01. 
# This is the same accuracy as the default parameters in rpart, and likely the cp chosen by the default parameters.


# smoothed values for these ‘probabilities’ for “Good” and “Bad” cases in these nodes
# calculate the Laplace smoothing and m-estimate smoothing values.



# Probabilities when laplace = 0 
nb_default <- naiveBayes(RESPONSE ~ ., data= GermanCreditTrn)


default_pred <- predict(nb_default, GermanCreditTst, type="class")


table(default_pred, GermanCreditTst$RESPONSE,dnn=c("Prediction","Actual"))

# Laplace Smoothing
# Probabilities when laplace = 1 

nb_laplace1 <- naiveBayes(RESPONSE~., data=GermanCreditTrn, laplace=1)
laplace1_pred <- predict(nb_laplace1, GermanCreditTst, type="class")

table(laplace1_pred, GermanCreditTst$RESPONSE,dnn=c("Prediction","Actual"))


# After Laplace smoothing we have observed  that the conditional probabilities 
# for the two models are now different. 
# The higher the laplace smoothing value, the more you are making the models the same.



######### Question 6 ##############

# PROFITVAL=100
# COSTVAL=-500
# 
# scoreTst=predict(rpModel2, GermanCreditTrn, type="prob")[,'1']
# prLifts=data.frame(scoreTst)
# prLifts=cbind(prLifts, GermanCreditTrn$RESPONSE)
# #check what is in prLifts ....head(prLifts)
# 
# prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score
# 
# #add profit and cumulative profits columns
# prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`GermanCreditTrn$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))
# head(prLifts)
# plot(prLifts$cumProfits)
# 
# mean(scoreTst==prLifts$cumProfits)
# 
# #Calculate score
# maxProfit= max(prLifts$cumProfits)
# maxProfit_Ind = which.max(prLifts$cumProfits)
# maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
# print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))


################################

nr=nrow(GermanCredit) # gets the number of rows

set.seed(123) # set seed for reproducible results
trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE) #get a random 50%sample of row-indices
GermanCreditTrn=GermanCredit[trnIndex,]   #training data with the randomly selected row-indices
GermanCreditTst = GermanCredit[-trnIndex,]  #test data with the other row-indices
dim(GermanCreditTrn) 
dim(GermanCreditTst)
CTHRESH = 0.50
costMatrix <- matrix(c(0,100,500, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')
Tree_Cost = rpart(RESPONSE ~ ., data=GermanCreditTst, method="class", parms = list( prior = c(.70,.30), loss = costMatrix, split = "information"))
# Finding the model's predictions on the training data
prediction_Trn=predict(Tree_Cost, GermanCreditTst, type='class')
#Confusion table
table(pred = prediction_Trn, true=GermanCreditTst$RESPONSE)
PROFITVAL=100
COSTVAL=-500

scoreTst=predict(rpModel2, GermanCreditTrn, type="prob")[,'1']
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, GermanCreditTrn$RESPONSE)
#check what is in prLifts ....head(prLifts)

prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score

#add profit and cumulative profits columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`GermanCreditTrn$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))
head(prLifts)
plot(prLifts$cumProfits)

mean(scoreTst==prLifts$cumProfits)

#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$testscore[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))

#Calculate score
nr=nrow(GermanCredit) 
trnSplit1 = sample(1:nr, size = round(0.7*nr), replace=FALSE) #get a random 50%sample of row-indices
g_train1=GermanCredit[trnSplit1,]   #training data with the randomly selected row-indices
g_test1 = GermanCredit[-trnSplit1,]  #test data with the other row-indices

CTHRESH = 0.83

costMatrix <- matrix(c(0,100,500, 0), byrow=TRUE, nrow=2)
colnames(costMatrix) <- c('Predict Good','Predict Bad')
rownames(costMatrix) <- c('Actual Good','Actual Bad')

Tree_Cost = rpart(RESPONSE ~ ., data=g_test1, method="class", parms = list( prior = c(.70,.30), loss = costMatrix, split = "information"))
#Obtain the model's predictions on the training data
prediction_Trn=predict(Tree_Cost, g_test1, type='class')
#Confusion table
table(pred = prediction_Trn, true=g_test1$RESPONSE)
#Accuracy
mean(prediction_Trn==g_test1$RESPONSE)
                                                                            
t1 = costMatrix[2,1]/(costMatrix[2,1] + costMatrix[1,2])
t1  
modelForPruning <- rpart(RESPONSE ~ ., data=g_test1, method="class",  parms = list(split = 'information'))
                                                                            
printcp(modelForPruning)
plotcp(modelForPruning)
prunTree<- prune(modelForPruning, cp= modelForPruning$cptable[which.min(modelForPruning$cptable[,"xerror"]),"CP"])
plot(prunTree, uniform=TRUE, main="Pruned Classification Tree")
text(prunTree, cex = 0.5)
                                                                            
mean(modelForPruning==g_test1$RESPONSE)
plot(modelForPruning, uniform=TRUE,  main="Decision Tree for German Credit History rating")
text(modelForPruning, use.n=TRUE, all=TRUE, cex=.7)
library(rpart.plot)
rpart.plot::prp(modelForPruning, type=2, extra=1)
##Question 6
###Calculating the Profits and cumulative profits
library(dplyr)
PROFITVAL=100
COSTVAL=-500

scoreTst=predict(rpModel2, GermanCreditTrn, type="prob")[,'1']
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, GermanCreditTrn$RESPONSE)
# head(prLifts)
                                                                            
prLifts=prLifts[order(-scoreTst) ,]  #sort by descending score
                                                                            
#add cumulative and profit  columns
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`GermanCreditTrn$RESPONSE`=='1', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))
head(prLifts)
plot(prLifts$cumProfits)
                                                                            
mean(scoreTst==prLifts$cumProfits)
                                                                            
#Calculate score
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))




###############################
# Question 7

nr=nrow(GermanCredit) # gets the number of rows
trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE) #get a random 70%sample of row-indices
Trn=GermanCredit[trnIndex,]   #training data with the randomly selected row-indices
Tst = GermanCredit[-trnIndex,]  #test data with the other row-indices
dim(GermanCreditTrn) 
dim(GermanCreditTst)

str(Tst)


# Q7: Random Forest tree
install.packages("randomForest")
library('randomForest')
#for reproducible results, set a specific value for the random number seed
set.seed(123)


#develop a training model with 200 trees, and obtain variable importance
#check the model -- see what OOB error rate it gives
rfModel1 = randomForest(RESPONSE ~ ., data=Trn, ntree=200,mtry=6, importance=TRUE )
rfModel
rfModel2 = randomForest(RESPONSE ~ ., data=Trn, ntree=400,mtry=6, importance=TRUE )
rfModel
rfModel3 = randomForest(RESPONSE ~ ., data=Trn, ntree=500,mtry=6, importance=TRUE )
rfModel3




?randomForest

#develop a test model with 500 trees, and obtain variable importance
#check the model -- see what OOB error rate it gives
rfModel5 = randomForest(RESPONSE ~ ., data=Tst, ntree=500,mtry=6, importance=TRUE )
rfModel5


#Variable importance
importance(rfModel5)
varImpPlot(rfModel5)




#Draw the ROC curve for the randomForest model
perf_rfTst=performance(prediction(predict(rfModel3,Tst, type="prob")[,2], Tst$RESPONSE), "tpr", "fpr")
plot(perf_rfTst)

#ROC curves for the decision-tree model and the random forest model in the same plot 

perfROC_dt1Tst=performance(prediction(predict(rpModel1,Tst, type="prob")[,2],Tst$RESPONSE), "tpr", "fpr")
perfRoc_dt2Tst=performance(prediction(predict(rpModel2_pruned,Tst, type="prob")[,2], Tst$RESPONSE), "tpr", "fpr")
perfRoc_rfTst=performance(prediction(predict(rfModel3,Tst, type="prob")[,2], Tst$RESPONSE), "tpr", "fpr")

plot(perfROC_dt1Tst, col='red')
plot(perfRoc_dt2Tst, col='blue', add=TRUE)
plot(perfRoc_rfTst, col='green', add=TRUE)
legend('bottomright', c('DecisionTree-1', 'DecisionTree-2', 'RandomForest'), lty=1, col=c('red', 'blue', 'green'))

