## Machine Learning Quiz 3 Question 1
# Load the cell segmentation data from the AppliedPredictiveModeling package 
# using the commands:
#         library(AppliedPredictiveModeling)
#         data(segmentationOriginal)
#         library(caret)
# 1. Subset the data to a training set and testing set based on the Case variable
#    in the data set.
# 2. Set the seed to 125 and fit a CART model with the rpart method using all 
#    predictor variables and default caret settings.
# 3. In the final model what would be the final model prediction for cases with 
#    the following variable values:
#         a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
#         b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
#         c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
#         d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

# 1. subsetting
table(segmentationOriginal$Case)
training<-segmentationOriginal[segmentationOriginal$Case=="Train",]
testing<-segmentationOriginal[segmentationOriginal$Case=="Test",]

# 2. Model building
install.packages("e1071")
library(e1071)
set.seed(125)
modFit<-train(Class~., method="rpart", data=training)
install.packages("rattle")
library(rattle)
install.packages("rpart.plot")
fancyRpartPlot(modFit$finalModel)



## Machine Learning Quiz 3 Question 4
# Load the South Africa Heart Disease Data and create training and test sets with the the
# following code:
#         library(ElemStatLearn)
#         data(SAheart)
#         set.seed(8484)
#         train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
#         trainSA = SAheart[train,]
#         testSA = SAheart[-train,]
# 
#         Then set the seed to 13234 and fit a logistic regression model (method="glm", 
# be sure to specify family="binomial") with Coronary Heart Disease (chd) as the 
# outcome and age at onset, current alcohol consumption, obesity levels, cumulative
# tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors. 
# Calculate the misclassification rate for your model using this function and a 
# prediction on the "response" scale:
#         missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
# 
# What is the misclassification rate on the training set? What is the 
# misclassification rate on the test set?

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modFit<-train(chd~age+alcohol+obesity+tobacco+typea+ldl, method="glm", family="binomial", data=trainSA)
trainhat<-predict(modFit, newdata = trainSA)
testhat<-predict(modFit, newdata = testSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, trainhat)
missClass(testSA$chd, testhat)



## Machine Learning Quiz 3 Question 5
# Load the vowel.train and vowel.test data sets:
#         library(ElemStatLearn)
#         data(vowel.train)
#         data(vowel.test)
# 
# Set the variable y to be a factor variable in both the training and test set. 
# Then set the seed to 33833. Fit a random forest predictor relating the factor 
# variable y to the remaining variables. Read about variable importance in random 
# forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr 
# The caret package uses by default the Gini importance.
# 
# Calculate the variable importance using the varImp function in the caret package. 
# What is the order of variable importance?

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.test$y<-as.factor(vowel.test$y)
vowel.train$y<-as.factor(vowel.train$y)
set.seed(33833)

install.packages("randomForest")
library(randomForest)
library(caret)
output<- randomForest(y~., data=vowel.train, importance=TRUE)
varImp(output)

