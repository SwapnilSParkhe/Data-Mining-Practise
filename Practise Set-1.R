###############################################################################
#PRACTISE SET-1 (books: Elements of Stats. Learning, & Intro to Stats. Learning)
###############################################################################


#******************************************************************************
#Q1:
#Compare the classification performance of linear regression and k–nearest 
#neighbor classification on the zipcode data. In particular, consider only
#the 2’s and 3’s, and k = 1, 3, 5, 7 and 15. Show both the training & test 
#error for each choice. The zipcode data are available from the website 
#www-stat.stanford.edu/ElemStatLearn
#******************************************************************************

#------Importing data---
train<-read.table("zip.train")
test<-read.table("zip.test")

#-----High level inspection of data (similar to what desired)
dim(train)
names(train)
str(train)
aggregate(train$V1, by=list(c(train$V1)), length)

dim(test)
names(test)
str(test)
aggregate(test$V1, by=list(c(test$V1)), length)

#------Modeling with Linear regression and KNN methods
#----JUST FOR PRACTISE---LM_1 (all rows G=0,1,2,3,...9) has high Err(train)=75%, hence segregating training and testing data for 2 & 3 separately for training and test, respectively
train_1<-train
test_1<-test
LM_1<-lm(V1~., data=train_1)
pred_train_1<-round(LM_1$fitted.values)
error_train_1<-mean(pred_train_1!=train_1$V1)
pred_test_1<-round(predict.lm(LM_1, test_1))
error_test_1<-mean(pred_test_1!=test_1$V1)

###LM_2 (rows with G=2,3) has Err(train)=0.5%, Err(test)=4.1%
train_2<-subset(train, train$V1==2|train$V1==3)
test_2<-subset(test,test$V1==2|test$V1==3)
LM_2<-lm(V1~., data=train_2)
pred_train_2<-round(LM_2$fitted.values)
error_train_2<-mean(pred_train_2!=train_2$V1)
pred_test_2<-round(predict.lm(LM_2,test_2))
error_test_2<-mean(pred_test_2!=test_2$V1)

##KNN (with k=1,3,5,7,15)
#install.packages("class")
library(class)

k<-c(1,3,5,7,15)
error_train_3<-rep(NA,length(k))
for(x in 1:length(k)){
  pred_train_3<-knn(train_2,train_2,cl=train_2$V1,k[x])
  error_train_3[x]<-mean(pred_train_3!=train_2$V1)
}

k<-c(1,3,5,7,15)
error_test_3<-rep(NA,length(k))
for(x in 1:length(k)){
  pred_test_3<-knn(train_2,test_2,cl=train_2$V1,k[x])
  error_test_3[x]<-mean(pred_test_3!=test_2$V1)
}

Errors_train_test<-matrix(c(error_train_1, error_train_2, error_train_3, error_test_1, error_test_2, error_test_3), ncol=2, nrow=7, byrow=FALSE)
colnames(Errors_train_test)<-c("Train-Error", "Test-Error")
row.names(Errors_train_test)<-c("LM_1 (using all G values)", "LM_2 (only using G=2,3)", paste("K-NN having K=",k, "(only using G=2,3)"))
write.csv(Errors_train_test, "Q1_Errors.csv")


#******************************************************************************
#Q2: 
#This exercise involves the Auto data set studied in the lab. Make sure that the missing values have been removed from the data.
#(a) Which of the predictors are quantitative, and which are quali- tative?
#(b) What is the range of each quantitative predictor? You can an- swer this using the range() function
#(c) What is the mean and standard deviation of each quantitative predictor?
#(d) Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?
#(e) Using the full data set, investigate the predictors graphically, using scatterplots or other tools of your choice. Create some plots highlighting the relationships among the predictors. Comment on your findings.
#(f) Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. Do your plots suggest that any of the other variables might be useful in predicting mpg? Justify your answer.
#******************************************************************************

#------Importing the file
library(readxl)
auto<-read_excel("Assignment1_Auto_data.xls")
dim(auto)

#-----Removing missing value rows
auto<-na.omit(auto)
dim(auto)

#-----Inspecting data at higher level
View(auto)
names(auto)
str(auto)

#-----a-----
str(auto)
sapply(auto, class)
auto$origin<-factor(auto$origin)

#-----b----
quant<-sapply(auto, is.numeric)
quant

sapply(auto[,quant] , range)

#-----c-----
sapply(auto[,quant], mean)
sapply(auto[,quant], sd)


#-----d-----
auto1<-auto[-c(10:85),]

sapply(auto1[,quant] , range)
sapply(auto1[,quant], mean)
sapply(auto1[,quant], sd)

#-------e----
#install.packages('plyr')
#install.packages('psych')
#install.packages('PerformanceAnalytics')
library(plyr)
library(psych)
library(PerformanceAnalytics)

dim(auto)
multi.hist(auto[,-9])
chart.Correlation(auto[,-9], pch=21)


#******************************************************************************
#Q3: 
#This question involves the use of multiple linear regression on the Auto data set.
#(a) Produce a scatterplot matrix which includes all of the variables in the data set.
#(b) Compute the matrix of correlations between the variables using the function cor(). You will need to exclude the name variable, which is qualitative.
#(c) Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except name as the predictors. Use the summary() function to print the results. Comment on the output. For instance:
#    i. Is there a relationship between the predictors and the re- sponse?
#    ii. Which predictors appear to have a statistically significant relationship to the response?
#    iii. What does the coefficient for the year variable suggest?
#(d) Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?
#(e) Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant?
#******************************************************************************

#------Importing the file
library(readxl)
auto<-read_excel("Assignment1_Auto_data.xls")
dim(auto)

#-----Inspecting data at higher level
View(auto)
names(auto)
str(auto)

#install.packages("pastecs") - for stat summary
library(pastecs)
stat.desc(auto)

#-------a--------
pairs(auto[, -9], pch=21)

#--------b--------
cor(auto[,-9])
write.csv(cor(auto[,-9]),"Auto_cor.csv")

#-------c-------
#--Removing missing value rows
auto<-na.omit(auto)
dim(auto)

lm_auto<-lm(mpg~.-name,auto)
summary(lm_auto)

#--------d------
par(mfrow=c(2,2))
plot(lm_auto)

#-------e-----
#All predictors and predictor pairs at a time
lm_auto_1<-lm(mpg~.^2 , auto[,-9])
summary(lm_auto_1)

#Taking all predictors along with effect of previously significant and insignificant predictor  )
lm_auto_2<-lm(mpg~cylinders:displacement + cylinders:weight +
                horsepower*displacement + horsepower:weight +
                displacement:weight, auto[,-9])
summary(lm_auto_2)

#Taking all predictors along with interactions among previosuly insignificant predictors)
lm_auto_3<-lm(mpg~.+cylinders:horsepower + cylinders:acceleration + horsepower:acceleration , auto[,-9])
summary(lm_auto_3)

#Taking only highly correlated predictor pairs
lm_auto_4<-lm(mpg ~ cylinders*displacement+displacement*weight, data = auto[, -9])
summary(lm_auto_4)

#---------JUST FOR PRACTICE-----Two all and some predictor pairs at a time
lm_auto_5_a<-lm(mpg~.+cylinders:displacement,auto[,-9])
lm_auto_5_b<-lm(mpg~.+horsepower*weight,auto[,-9])
lm_auto_5_c<-lm(mpg~.+acceleration:year,auto[,-9])
summary(lm_auto_5_a)
summary(lm_auto_5_b)
summary(lm_auto_5_c)

#--------f-------
#Transformation-Using some predictors that do not have a linear relationship with response (horsepower and weight)
mpg_pred<-data.frame(mpg=auto$mpg, hp=auto$horsepower, wt=auto$weight)
chart.Correlation(mpg_pred)

mpg_transf_pred<-data.frame(mpg=auto$mpg, 
              hp_log=log(auto$horsepower), hp_sqrt=sqrt(auto$horsepower), hp_sq=(auto$horsepower)^2,
              wt_log=log(auto$weight), wt_sqrt=sqrt(auto$weight),wt_sq=(auto$weight)^2
              )
chart.Correlation(mpg_transf_pred)


#******************************************************************************
#Q4:
#This exercise involves the Boston housing data set.
#(a) To begin, load in the Boston data set. The Boston data set is part of the MASS library in R. > library(MASS). How many rows are in this data set? How many columns? What do the rows and columns represent?
#(b) Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.
#(c) Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
#(d) Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.
#(e) How many of the suburbs in this data set bound the Charles river?
#(f) What is the median pupil-teacher ratio among the towns in this data set?
#(g) Which suburb of Boston has lowest median value of owner- occupied homes? What are the values of the other predictors for that suburb, and how do those values compare to the overall ranges for those predictors? Comment on your findings.
#(h) In this data set, how many of the suburbs average more than seven rooms per dwelling? More than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per dwelling.
#******************************************************************************

#--------a-------
library(MASS)
Boston
?Boston

dim(Boston)

#---------b-------
pairs(Boston)

par(mfrow=c(2,3))
plot(Boston$rad, Boston$tax)
plot(Boston$indus, Boston$nox)
plot(Boston$age, Boston$nox)
plot(Boston$nox, Boston$dis)
plot(Boston$age, Boston$dis)
plot(Boston$medv, Boston$lstat)

#---------c-------
write.csv(cor(Boston$crim, Boston), "crim_corr.csv")

#---------d--------
par(mfrow=c(1,3))
hist(Boston$crim, breaks=30)
hist(Boston$tax, breaks=30)
hist(Boston$ptratio, breaks=30)

#---------e------
sum(Boston$chas==1)

#---------f-----
median(Boston$ptratio)

#---------g-----
which.min(Boston$medv)
Boston[which.min(Boston$medv),14]
Boston[which.min(Boston$medv),]
sapply(Boston, range)

#--------h------
summary(Boston$rm>7)
summary(Boston$rm>8)

x<-which(Boston$rm>8)
Boston[which(Boston$rm>8),]
par(mfrow=c(5,3))
for (i in 1:ncol(Boston)){
  hist(Boston[, i], main=colnames(Boston)[i], breaks="FD")
  abline(v=Boston[x, i], col="red", lw=1)
}