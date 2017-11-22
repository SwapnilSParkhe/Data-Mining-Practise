###############################################################################
#PRACTISE SET-2 (books: Elements of Stats. Learning, & Intro to Stats. Learning)
###############################################################################


#******************************************************************************
#Q1:
#Consider the Gini index, classification error, and entropy in a simple 
#classification setting with two classes. Create a single plot that displays 
#each of these quantities as a function of pˆm1. The x- axis should display 
#pˆm1, ranging from 0 to 1, and the y-axis should display the value of the 
#Gini index, classification error, and entropy. Hint: In a setting with two 
#classes, pˆm1 = 1 − pˆm2. You could make this plot by hand, but it will be much easier to make in R
#******************************************************************************

prob<-c(0:100)/100
Gini_index<-prob*(1-prob)+prob*(1-prob)
Classify_error<-1-pmax(prob,1-prob)
Cross_entropy<-(prob*log(prob)+(1-prob)*log(1-prob))
matplot(prob,cbind(Gini_index,Classify_error,Cross_entropy), 
        col=c("purple","maroon","darkgreen"),
        xlab="Probability", ylab="Gini_index,Classify_error,Cross_entropy",
        main="Different Indices vs probability",
        cex=1,pch=c(20,21,22))
legend("bottom", cex=0.75,inset=0.05, legend=c("Gini", "C_error", "Entropy"),
       pch=c(20,21,22), col=c("purple","maroon","darkgreen"), horiz=TRUE)


#******************************************************************************
#Q2:
#In the lab, a classification tree was applied to the Carseats data set af- ter converting Sales into a qualitative response variable. Now we will seek to predict Sales using regression trees and related approaches, treating the response as a quantitative variable.
#(a) Split the data set into a training set and a test set.
#(b) Fit a regression tree to the training set. Plot the tree, and inter- pret the results. What test MSE do you obtain?
#(c) Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test MSE?
#(d) Use the bagging approach in order to analyze this data. What test MSE do you obtain? Use the importance() function to de- termine which variables are most important.
#(e) Use random forests to analyze this data. What test MSE do you obtain? Use the importance() function to determine which vari- ables are most important. Describe the effect of m, the number of variables considered at each split, on the error rate obtained.
#******************************************************************************

#Importing relevant library and checking its content
library(ISLR)
library(help=ISLR)
ls("package:ISLR")

#Loading dataset "Carseats" and inspecting at a higher level
data(Carseats)
View(Carseats)
dim(Carseats)
names(Carseats)
summary(Carseats)

#----(a)------
set.seed(110)
ID<-sample(2,nrow(Carseats),replace=TRUE,prob=c(0.5,0.5))
Train<-Carseats[ID==1,]
Test<-Carseats[ID==2,]

#----(b)-----
#Importing the relevant "rpart" library and rpart.plot library
library(rpart)
library(rpart.plot)

#Generating simple rpart tree model (without any parameters and arguments)
#Note: Though no parameter is used, it's not a fully grown tree
set.seed(1322)
simple_rpart<-rpart(Sales~.,data=Train)

#Summary and Visualisation
print(simple_rpart)
summary(simple_rpart)
rpart.plot(simple_rpart)

#Prediction and error rate (around 4.8)
simple_rpart_pred<-predict(simple_rpart,newdata = Test)
simple_rpart_error<-mean((simple_rpart_pred-Test$Sales)^2)

#------(c)--------
#Making decision tree model (fully grown first)
set.seed(1322)
FullyGrown_rpart<-rpart(Sales~.,data=Train,
                        method="anova",
                        control=rpart.control(minbucket=1,cp=0))

#---Pruning (based on optimal Complexity Parameter or CP using CV)
#Finding optimal CP first using crossvalidation or CV
plotcp(FullyGrown_rpart)
printcp(FullyGrown_rpart)
cp_optimal<-FullyGrown_rpart$cptable[FullyGrown_rpart$cptable[,"xerror"]==min(FullyGrown_rpart$cptable[,"xerror"]),"CP"]

#Pruning using optimal CP obtained from CV
Pruned_rpart<-prune(FullyGrown_rpart,cp=cp_optimal[1])

#---Summarizing and visualization (Fully grown and pruned)
print(FullyGrown_rpart)
summary(FullyGrown_rpart)

print(Pruned_rpart)
summary(Pruned_rpart)

#---Visualizing model tree (in R and outside R for clarity)
rpart.plot(FullyGrown_rpart)
rpart.plot(Pruned_rpart)

#Prediction and error rate (5.7 for FullyGrown, 5.3 for Pruned)
FullyGrown_rpart_pred<-predict(FullyGrown_rpart,newdata = Test)
FullyGrown_rpart_error<-mean((FullyGrown_rpart_pred-Test$Sales)^2)

Pruned_rpart_pred<-predict(Pruned_rpart,newdata = Test)
Pruned_rpart_error<-mean((Pruned_rpart_pred-Test$Sales)^2)

#------(d)--------
#Note: using bagging as a special case for random forest with m=p=10 (in this case =10)

#Importing the relevant "randomForest" library 
library(randomForest)

#Generating simple bagging model 
set.seed(12224)
simple_bagging<-randomForest(Sales ~ ., data = Train, 
                             mtry = 10, ntree = 1000, importance = TRUE)
  
#Summary and visualization
print(simple_bagging)
summary(simple_bagging)
plot(simple_bagging)

#Prediction and error rate (around 2.6)
simple_bagging_pred<-predict(simple_bagging, newdata = Test)
simple_bagging_error<-mean((simple_bagging_pred - Test$Sales)^2)

#Importance function to get important vars (Price and ShelveLoc)
importance(simple_bagging)

#------(e)--------
#Importing the relevant "randomForest" library
library(randomForest)

#Generating simple randomForest model 
set.seed(122124)
simple_rf<-randomForest(Sales ~ ., data = Train, 
                        mtry = 3, ntree = 1000, importance = TRUE)

#Summary and visualization
print(simple_rf)
summary(simple_rf)
plot(simple_rf)

#Prediction and error rate (around 2.9)
simple_rf_pred<-predict(simple_rf, newdata = Test)
simple_rf_error<-mean((simple_rf_pred - Test$Sales)^2)

#Importance function to get important vars (Price and ShelveLoc)
importance(simple_rf)

#Effect of m on err rate
mtry.values=1:10
err.values=rep(NA,length(mtry.values))
for(i in 1:length(mtry.values)){
  m=mtry.values[i]
  rf=randomForest(Sales~.,data=Train,
                           mtry=mtry.values[i], ntree=1000,
                           importance=T)
  pred=predict(rf,Test)
  error=mean((pred-Test$Sales)^2)
  err.values[i]=error
}
err.values

plot(cbind(mtry.values,err.values),type="b",main="simple_rf")


#******************************************************************************
#Q3:
#We now use boosting to predict Salary in the Hitters data set.
#(a) Remove the observations for whom the salary information is unknown, and then log-transform the salaries.
#(b) Create a training set consisting of the first 200 observations, and a test set consisting of the remaining observations.
#(c) Perform boosting on the training set with 1,000 trees for a range of values of the shrinkage parameter λ. Produce a plot with different shrinkage values on the x-axis and the corresponding training set MSE on the y-axis.
#(d) Produce a plot with different shrinkage values on the x-axis and the corresponding test set MSE on the y-axis.
#(e) Compare the test MSE of boosting to the test MSE that results from applying two of the regression approaches seen in Chapters 3 and 6.
#(f) Which variables appear to be the most important predictors in the boosted model?
#(g) Now apply bagging to the training set. What is the test set MSE for this approach?
#******************************************************************************

#Importing relevant library and checking its content
library(ISLR)
library(help=ISLR)
ls("package:ISLR")

#Loading dataset "Hitters" and inspecting at a higher level
data(Hitters)
View(Hitters)
dim(Hitters)
names(Hitters)
summary(Hitters)

#----(a)-----
#Omitting data rows where Salary is unavailable
Hitters<-Hitters[!(is.na(Hitters$Salary)),]

#Log transforming Salary data
Hitters$Salary<-log(Hitters$Salary)

#----(b)-----
Train_Q4_ID<-c(1:200)
Train_Q4<-Hitters[Train_Q4_ID,]
Test_Q4<-Hitters[-Train_Q4_ID,]

#----(c)-----
#Importing the relevant "gbm" library 
library(gbm)

#Generating gbm model (First setting a range of values for lambda)
set.seed(13245)
lambda<-10^(c(-100:-1)/10)

gbm_boosting_train_error<-NULL
for (i in 1:length(lambda)) {
  gbm_boosting<-gbm(Salary ~ ., data = Train_Q4, distribution = "gaussian",
                    n.trees = 1000, shrinkage = lambda[i])
  
  gbm_boosting_train_pred<-predict(gbm_boosting, Train_Q4, n.trees = 1000)
  
  gbm_boosting_train_error[i]<-mean((gbm_boosting_train_pred-Train_Q4$Salary)^2)
}


#Visualization of training set MSE with different lambda
plot(lambda, gbm_boosting_train_error, col="red", type="b",
     xlab = "Lambda or Shrinkage values", ylab = "Training set MSE",
     main="Train set MSE vs Lambda")

#----(d)-----
#Generating gbm model (First setting a range of values for lambda)
set.seed(13245)
lambda<-10^(c(-100:-1)/10)

gbm_boosting_test_error<-NULL
for (i in 1:length(lambda)) {
  gbm_boosting<-gbm(Salary ~ ., data = Train_Q4, distribution = "gaussian",
                    n.trees = 1000, shrinkage = lambda[i])
  
  gbm_boosting_test_pred<-predict(gbm_boosting, Test_Q4, n.trees = 1000)
  
  gbm_boosting_test_error[i]<-mean((gbm_boosting_test_pred-Test_Q4$Salary)^2)
}


#Visualization of test set MSE with different lambda (min test error at lambda = 0.16)
plot(lambda, gbm_boosting_test_error, col="darkgreen", type="b",
     xlab = "Lambda or Shrinkage values", ylab = "Test set MSE",
     main="Test set MSE vs Lambda")

#Lowest test error (0.27) and correpsonding lambda
min_test_error<-min(gbm_boosting_test_error)
opt_lambda<-lambda[which.min(gbm_boosting_test_error)]

#-----(e)-----
#Generating simple linear model
simple_linear<-lm(Salary~.,data=Train_Q4)

#Summary and visualization
print(simple_linear)
summary(simple_linear)

#Prediction and error rate (around 0.49)
simple_linear_pred<-predict(simple_linear, newdata = Test_Q4)
simple_linear_error<-mean((simple_linear_pred - Test_Q4$Salary)^2)

#Importing the relevant "glmnet" library 
library(glmnet)

#Generating simple gmlnet ridge regresion model
simple_ridge<-glmnet(x=model.matrix(Salary ~ ., data = Train_Q4),
                     y=Train_Q4$Salary,
                     alpha=0)
                
#Summary and visualization
print(simple_ridge)
summary(simple_ridge)
#plot(simple_ridge)

#Prediction and error rate (around 0.46)
simple_ridge_pred<-predict(simple_ridge, s=0.01, 
                           newx = model.matrix(Salary ~ ., data = Test_Q4))
simple_ridge_error<-mean((simple_ridge_pred - Test_Q4$Salary)^2)

#----(f)-----
#Re-Generating optimal gbm model with optimal paramenter for shrinkage parameter obtained
gbm_boosting<-gbm(Salary ~ ., data = Train_Q4, distribution = "gaussian",
                  n.trees = 1000, shrinkage = opt_lambda)

#Summary and visualization
print(gbm_boosting)
summary(gbm_boosting)
plot(gbm_boosting)

#----(g)-----
#Note: using bagging as a special case for random forest with m=p=19 (in this case =19)

#Importing the relevant "randomForest" library 
library(randomForest)

#Generating simple bagging model 
set.seed(12824)
simple_bagging_Q4<-randomForest(Salary ~ ., data = Train_Q4, 
                                mtry = 19, ntree = 1000, importance = TRUE)

#Summary and visualization
print(simple_bagging_Q4)
summary(simple_bagging_Q4)
plot(simple_bagging_Q4)

#Prediction and error rate (around 0.23)
simple_bagging_Q4_pred<-predict(simple_bagging_Q4, newdata = Test_Q4)
simple_bagging_Q4_error<-mean((simple_bagging_Q4_pred - Test_Q4$Salary)^2)


