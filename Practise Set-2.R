###############################################################################
#PRACTISE SET-2 (books: Elements of Stats. Learning, & Intro to Stats. Learning)
###############################################################################


#******************************************************************************
#Q1:
#This problem involves the Boston data set, which we saw in the lab for this chapter. We will now try to predict per capita crime rate using the other variables in this data set. In other words, per capita crime rate is the response, and the other variables are the predictors.
#(a) For each predictor, fit a simple linear regression model to predict the response. Describe your results. In which of the models is there a statistically significant association between the predictor and the response? Create some plots to back up your assertions.
#(b) Fit a multiple regression model to predict the response using all of the predictors. Describe your results. For which predictors can we reject the null hypothesis H0 : βj = 0?
#(c) How do your results from (a) compare to your results from (b)? Create a plot displaying the univariate regression coefficients from (a) on the x-axis, and the multiple regression coefficients from (b) on the y-axis. That is, each predictor is displayed as a single point in the plot. Its coefficient in a simple linear regres- sion model is shown on the x-axis, and its coefficient estimate in the multiple linear regression model is shown on the y-axis.
#(d) Is there evidence of non-linear association between any of the predictors and the response? To answer this question, for each predictor X, fit a model of the form
#    Y = β0 +β1X +β2X2 +β3X3 +ε
#******************************************************************************

#------Getting relevant libraries
library(MASS)
library(psych)

#------Getting data
data("Boston")
View(Boston)
?Boston

#------Inspecting data at a higher level & Reformating required columns
dim(Boston)
names(Boston)
head(Boston)
str(Boston)
attributes(Boston)

Boston$chas<-as.factor(Boston$chas)
Boston$rad<-as.factor(Boston$rad)

#----Exploratory data analysis
#****Numeric data summary
#Univariate (all variables)
summary(Boston)
stat.desc(Boston)

#Bivariate (Corr matrix for numeric,)
cor(Boston[,-c(4,9)])

#****Graphical Summary
#Univariate summary (all variables - numric and catg)
multi.hist(Boston[,c(1:3,5:7)])
multi.hist(Boston[,10:14])
par(mfrow=c(1,2))
plot(table(Boston$chas))
plot(table(Boston$rad))

#Bivariates summary (among all variables; only numeric variables; response vs catg predictor)
pairs(Boston, col=Boston$chas)
chart.Correlation(Boston[,-c(4,9)])
par(mfrow=c(1,2))
boxplot(crim~chas, data=Boston, col=c("red","green"))
boxplot(crim~rad, data=Boston)

#---------(a)----------
#Building linearl model for each predictor one-by-one 
lm_univariate<-NULL
for (i in names(Boston)[2:14]){
  lm_univariate[[i]]<-lm(as.formula(paste("crim ~", i)),data=Boston)
}
lapply(lm_univariate,summary)

#Checking plots of response with all predictors
par(mfrow=c(3,5))
for(i in 2:14){
  plot(x=Boston[,i], y=Boston$crim, 
       xlab=names(Boston)[i], ylab="crim")
}

#Checking disgnostic plots of all individual models
par(mfrow=c(3,5))
for(i in names(Boston)[2:14]){
  plot(lm(as.formula(paste("crim~",i)), data=Boston),
       main=paste("LM between crim and", i),which=1)
}

#------(b)--------
#Building and Summarrizing linear model (LM) using all predictors
lm(crim~., data=Boston)
summary(lm(crim~., data=Boston))

#------(c)--------
uni_betas<-c(lm(crim~zn, data=Boston)$coefficients[2],
             lm(crim~indus, data=Boston)$coefficients[2],
             lm(crim~chas, data=Boston)$coefficients[2],
             lm(crim~nox, data=Boston)$coefficients[2],
             lm(crim~rm, data=Boston)$coefficients[2],
             lm(crim~age, data=Boston)$coefficients[2],
             lm(crim~dis, data=Boston)$coefficients[2],
             lm(crim~rad, data=Boston)$coefficients[-1],
             lm(crim~tax, data=Boston)$coefficients[2],
             lm(crim~ptratio, data=Boston)$coefficients[2],
             lm(crim~black, data=Boston)$coefficients[2],
             lm(crim~lstat, data=Boston)$coefficients[2],
             lm(crim~medv, data=Boston)$coefficients[2])

#----Rough work----
#x<-sapply(lapply(lm_univariate[-8], coefficients),"[[",2)
#y<-lapply(lm_univariate, coefficients)$rad[-1]
#uni_betas<-c(y,x)

multi_betas<-lm(crim~., data=Boston)$coefficients[-1]
uni_betas
multi_betas

par(mfrow=c(1,1))
plot(x=uni_betas,y=multi_betas, asp=1)
#install.packages("calibrate")
library(calibrate)
textxy(uni_betas,multi_betas,labs=names(uni_betas), 
       cx=0.3, m=c(0,0))

#--------(d)--------
nlm_univariate<-NULL
for (i in names(Boston)[-c(1,4,9)]){
  nlm_univariate[[i]]<-lm(as.formula(paste("crim ~", i, "+I(", i, "^2)", "+I(", i, "^3)")),data=Boston)
}
lapply(nlm_univariate,summary)


#******************************************************************************
#Q2:
#This question should be answered using the Weekly data set, which is part of the ISLR package. This data is similar in nature to the Smarket data from this chapter’s lab, except that it contains 1,089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010.
#(a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any patterns?
#(b) Use the full data set to perform a logistic regression with Direction as the response and the five lag variables plus Volume as predictors. Use the summary function to print the results. Do any of the predictors appear to be statistically significant? If so, which ones?
#(c) Compute the confusion matrix and overall fraction of correct predictions. Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression.
#(d) Now fit the logistic regression model using a training data period from 1990 to 2008, with Lag2 as the only predictor. Compute the confusion matrix and the overall fraction of correct predictions for the held out data (that is, the data from 2009 and 2010).
#(e) Repeat (d) using LDA.
#(f) Repeat (d) using QDA.
#(g) Repeat (d) using KNN with K = 1.
#(h) Which of these methods appears to provide the best results on this data?
#(i) Experiment with different combinations of predictors, includ- ing possible transformations and interactions, for each of the methods. Report the variables, method, and associated confu- sion matrix that appears to provide the best results on the held out data. Note that you should also experiment with values for K in the KNN classifier
#******************************************************************************

#------Loading required and useful packages
library(ISLR)
library(pastecs)
library(psych)
library(PerformanceAnalytics)

#------Getting data
data(Weekly)
View(Weekly)

#------Inspecting data at a higher level
dim(Weekly)
names(Weekly)
head(Weekly)
str(Weekly)
attributes(Weekly)

#------(a)--------
#****Numeric summary (Univariates, Bivariates)
#Univariate summary
summary(Weekly)
stat.desc(Weekly)

#Bivariate summary (Corr matrix for numeric, tests of significance for others)
cor(Weekly[,c(-9)])

#****Graphical Summary (Using functions in "PerformanceAnalytics" package)
#Univariate summary (All variables - numeric and catg)
multi.hist(Weekly[-9])

par(mfrow=c(1,1))
plot(table(Weekly$Direction))

#Bivariates summary (among all variables; numeric variables; between response and predictors)
pairs(Weekly, col=Weekly$Direction, bg=c("red","blue"))

chart.Correlation(Weekly[,c(-9)],)

par(mfrow=c(1,8))
for(i in names(Weekly)[-9]){
  boxplot(data=Weekly,
          as.formula(paste(i,"~Direction")),
          xlab="Direction", ylab=i,
          main=i, col=c("lightgreen","yellow"))
}


#------(b)-------
#Building Logit model using instruction in part b
logit_model<-glm(Direction~.-Today-Year,data=Weekly, family="binomial")

#Model Summary (Lag2 seems to be statistically significant)
logit_model
summary(logit_model)


#------(c)------
#Predictions (for training data based on logit_model)
pred_logit<-predict(logit_model, type="response")

#Confusion matrix and Accuracy(for training data based on logit_model)
logit_cfm<-table(ifelse(pred_logit>0.5,"Up","Down"),Weekly$Direction, dnn=c("Predicted", "Actual"))
acc_logit<-sum(diag(logit_cfm))/sum(logit_cfm)
logit_cfm
acc_logit

#------(d)-----
#Subsetting the data into training and test as per part d instructions
train_Weekly<- subset(Weekly,Weekly$Year<=2008)
test_Weekly <- subset(Weekly,Weekly$Year>=2009)

#Building Logit model using instruction in part c
logit_model_1<-glm(Direction~Lag2 ,data=train_Weekly, family="binomial")

#Model Summary (Lag2 seems to be statistically significant)
logit_model_1
summary(logit_model_1)

#Predictions (for training data based on logit_model)
pred_logit_test<-predict(logit_model_1, newdata=test_Weekly, type="response")

#Confusion matrix and Accuracy(for training data based on logit_model)
logit_cfm_1<-table(ifelse(pred_logit_test>0.5,"Up","Down"),test_Weekly$Direction, dnn=c("Predicted", "Actual"))
acc_logit_1<-sum(diag(logit_cfm_1))/sum(logit_cfm_1)
logit_cfm_1
acc_logit_1

#-----(e)----------
library(MASS)

#Building LDA model using instruction in part d
lda_model_1<-lda(Direction~Lag2 ,data=train_Weekly)

#Look into results
lda_model_1
plot(lda_model_1)

#Predictions (for training data based on lda_model)
pred_lda_test<-predict(lda_model_1, newdata=test_Weekly, type="response")

#Confusion matrix and Accuracy(for training data based on lda_model)
lda_cfm_1<-table(pred_lda_test$class,test_Weekly$Direction, dnn=c("Predicted", "Actual"))
acc_lda_1<-sum(diag(lda_cfm_1))/sum(lda_cfm_1)
lda_cfm_1
acc_lda_1

#-----(f)----------
library(MASS)

#Building QDA model using instruction in part d
qda_model_1<-qda(Direction~Lag2 ,data=train_Weekly)

#Look into results
qda_model_1

#Predictions (for training data based on qda_model)
pred_qda_test<-predict(qda_model_1, newdata=test_Weekly, type="response")

#Confusion matrix and Accuracy(for training data based on qda_model)
qda_cfm_1<-table(pred_qda_test$class,test_Weekly$Direction, dnn=c("Predicted", "Actual"))
acc_qda_1<-sum(diag(qda_cfm_1))/sum(qda_cfm_1)
qda_cfm_1
acc_qda_1

#-----(g)-------
library(class)
?knn

#Building KNN model using instruction in part d
knn_model_1<-knn(as.data.frame(train_Weekly$Lag2),
                 as.data.frame(test_Weekly$Lag2),
                 cl=train_Weekly$Direction,k=1)

#Look into results
knn_model_1
summary(knn_model_1)

#Confusion matrix and Accuracy(for training data based on lda_model)
knn_cfm_1<-table(knn_model_1,test_Weekly$Direction, dnn=c("Predicted", "Actual"))
acc_knn_1<-sum(diag(knn_cfm_1))/sum(knn_cfm_1)
knn_cfm_1
acc_knn_1

#-------(h)------
#Answered in the pdf file (LDA and QDA with training and test)

#-------(i)------
#****LOGIT model with different variations

#Multiple models with different variable combinations
logit_model_2<-glm(Direction~log(abs(Lag2)) ,data=train_Weekly, family="binomial")
logit_model_3<-glm(Direction~Lag1:Lag2 ,data=train_Weekly, family="binomial")
logit_model_4<-glm(Direction~Lag2:Volume ,data=train_Weekly, family="binomial")
logit_model_5<-glm(Direction~log(Volume) ,data=train_Weekly, family="binomial")
logit_model_all<-list(logit_model,logit_model_1,logit_model_2, logit_model_3, logit_model_4, logit_model_5)

#Model Summary (Only Lag2 seems to be statistically significant)
for(i in 1:length(logit_model_all)){
  print(coefficients(summary(logit_model_all[[i]])))
  cat("\n")
}

#Predictions; Confusion matrix; Accuracy (on Test data)
pred_logit_test_all<-NULL
logit_cfm_all<-NULL
acc_logit_all<-NULL
for(i in 1:length(logit_model_all)){
  pred_logit_test_all[[i]]<-predict(logit_model_all[[i]], newdata=test_Weekly, type="response")
  logit_cfm_all[[i]]<-table(ifelse(pred_logit_test_all[[i]]>0.5,"Up","Down"),test_Weekly$Direction, dnn=c("Predicted", "Actual"))
  acc_logit_all[[i]]<-sum(diag(logit_cfm_all[[i]]))/sum(logit_cfm_all[[i]])
}

logit_Confusion_Matrix<-list(Model_type=rep("LOGIT",6),
                             Iteration=c("All (except Today and Year)", "Only Lag2", "Log(Lag2)", "Lag1:Lag2", "Lag2:Volume", "Log(Volume)"),
                             Confusion_Matrix_for_Iteration=logit_cfm_all)
logit_Accuracy_table<-data.frame(Model_type=rep("LOGIT",6),
                                 Iteration=c("All (except Today and Year)","Only Lag2", "Log(Lag2)", "Lag1:Lag2", "Lag2:Volume", "Log(Volume)"),
                                 Accuracy=round(acc_logit_all,3))

logit_Confusion_Matrix
logit_Accuracy_table

#****LDA model with different variations
library(MASS)

#Multiple models with different variable combinations
lda_model_2<-lda(Direction~log(abs(Lag2)) ,data=train_Weekly, family="binomial")
lda_model_3<-lda(Direction~Lag1:Lag2 ,data=train_Weekly, family="binomial")
lda_model_4<-lda(Direction~Lag2:Volume ,data=train_Weekly, family="binomial")
lda_model_5<-lda(Direction~log(Volume) ,data=train_Weekly, family="binomial")
lda_model_all<-list(lda_model_1,lda_model_2, lda_model_3, lda_model_4, lda_model_5)

#Model Summary (Only Lag2 seems to be statistically significant)
for(i in 1:length(lda_model_all)){
  print((lda_model_all[[i]]))
  cat("\n")
}

#Predictions; Confusion matrix; Accuracy (on Test data)
pred_lda_test_all<-NULL
lda_cfm_all<-NULL
acc_lda_all<-NULL
for(i in 1:length(lda_model_all)){
  pred_lda_test_all[[i]]<-predict(lda_model_all[[i]], newdata=test_Weekly, type="response")
  lda_cfm_all[[i]]<-table(pred_lda_test_all[[i]]$class,test_Weekly$Direction, dnn=c("Predicted", "Actual"))
  acc_lda_all[[i]]<-sum(diag(lda_cfm_all[[i]]))/sum(lda_cfm_all[[i]])
}

lda_Confusion_Matrix<-list(Model_type=rep("lda",5),
                           Iteration=c("Only Lag2", "Log(Lag2)", "Lag1:Lag2", "Lag2:Volume", "Log(Volume)"),
                           Confusion_Matrix_for_Iteration=lda_cfm_all)
lda_Accuracy_table<-data.frame(Model_type=rep("lda",5),
                               Iteration=c("Only Lag2", "Log(Lag2)", "Lag1:Lag2", "Lag2:Volume", "Log(Volume)"),
                               Accuracy=round(acc_lda_all,3))

lda_Confusion_Matrix
lda_Accuracy_table


#****QDA model with different variations
library(MASS)

#Multiple models with different variable combinations
qda_model_2<-qda(Direction~log(abs(Lag2)) ,data=train_Weekly, family="binomial")
qda_model_3<-qda(Direction~Lag1:Lag2 ,data=train_Weekly, family="binomial")
qda_model_4<-qda(Direction~Lag2:Volume ,data=train_Weekly, family="binomial")
qda_model_5<-qda(Direction~log(Volume) ,data=train_Weekly, family="binomial")
qda_model_all<-list(qda_model_1,qda_model_2, qda_model_3, qda_model_4, qda_model_5)

#Model Summary (Only Lag2 seems to be statistically significant)
for(i in 1:length(qda_model_all)){
  print((qda_model_all[[i]]))
  cat("\n")
}

#Predictions; Confusion matrix; Accuracy (on Test data)
pred_qda_test_all<-NULL
qda_cfm_all<-NULL
acc_qda_all<-NULL
for(i in 1:length(qda_model_all)){
  pred_qda_test_all[[i]]<-predict(qda_model_all[[i]], newdata=test_Weekly, type="response")
  qda_cfm_all[[i]]<-table(pred_qda_test_all[[i]]$class,test_Weekly$Direction, dnn=c("Predicted", "Actual"))
  acc_qda_all[[i]]<-sum(diag(qda_cfm_all[[i]]))/sum(qda_cfm_all[[i]])
}

qda_Confusion_Matrix<-list(Model_type=rep("QDA",5),
                           Iteration=c("Only Lag2", "Log(Lag2)", "Lag1:Lag2", "Lag2:Volume", "Log(Volume)"),
                           Confusion_Matrix_for_Iteration=qda_cfm_all)
qda_Accuracy_table<-data.frame(Model_type=rep("QDA",5),
                               Iteration=c("Only Lag2", "Log(Lag2)", "Lag1:Lag2", "Lag2:Volume", "Log(Volume)"),
                               Accuracy=round(acc_qda_all,3))

qda_Confusion_Matrix
qda_Accuracy_table

#****KNN model with different variations (with k=1,3,5,7,15)
library(class)

k<-c(1,3,5,7,15)
pred_KNN_test_all<-NULL
KNN_cfm_all<-NULL
acc_KNN_all<-NULL
for(i in 1:length(k)){
  pred_KNN_test_all[[i]]<-knn(as.data.frame(train_Weekly$Lag2),
                              as.data.frame(test_Weekly$Lag2),
                              cl=train_Weekly$Direction,k[i])
  KNN_cfm_all[[i]]<-table(pred_KNN_test_all[[i]],test_Weekly$Direction, dnn=c("Predicted", "Actual"))
  acc_KNN_all[[i]]<-sum(diag(KNN_cfm_all[[i]]))/sum(KNN_cfm_all[[i]])
}

KNN_Confusion_Matrix<-list(Model_type=rep("KNN",5),
                           Iteration=c("k=1", "k=3", "k=5", "k=7", "k=15"),
                           Confusion_Matrix_for_Iteration=KNN_cfm_all)
KNN_Accuracy_table<-data.frame(Model_type=rep("KNN",5),
                               Iteration=c("k=1", "k=3", "k=5", "k=7", "k=15"),
                               Accuracy=round(acc_KNN_all,3))

KNN_Confusion_Matrix
KNN_Accuracy_table

