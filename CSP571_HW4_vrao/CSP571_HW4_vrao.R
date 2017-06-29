# CSP 571 Homework 4

createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

# 1. Please write a function called backwards() that implements the
# backward selection algorithm using AIC.

backwards <- function(model)
{
  a<-step(model,direction="backward")
  return(a)
}


# 2. Download the credit card default data set from the UCI machine learning
# repository. https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
# Load the data into R.

mydat <- read.csv(file = "CC-Defaulter.csv", sep =',',strip.white=TRUE,
                  col.names = c("ID", "LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE", "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6", "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6", "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6", "DEFAULT_PAYMENT_NEXT_MONTH"))

mydat <- mydat[-1,]
mydat <- mydat[,-1]

head(mydat)

# 3. Identify all the relevant categorical, numeric, and logical variables.

#All Factors
mydat$SEX <- sapply(mydat$SEX , as.factor)
mydat$EDUCATION <- sapply(mydat$EDUCATION , as.factor)
mydat$MARRIAGE <- sapply(mydat$MARRIAGE , as.factor)

mydat$PAY_0 <- sapply(mydat$PAY_0 , as.factor)
mydat$PAY_2 <- sapply(mydat$PAY_2 , as.factor)
mydat$PAY_3 <- sapply(mydat$PAY_3 , as.factor)
mydat$PAY_4 <- sapply(mydat$PAY_4 , as.factor)
mydat$PAY_5 <- sapply(mydat$PAY_5 , as.factor)
mydat$PAY_6 <- sapply(mydat$PAY_6 , as.factor)

#All Numeric

# This is numeric but it's corrupting that's why it is commented

#mydat$LIMIT_BAL <- sapply(mydat$LIMIT_BAL , as.numeric)

mydat$AGE <- sapply(mydat$AGE, as.numeric)
mydat$BILL_AMT1 <- sapply(mydat$BILL_AMT1 , as.numeric)
mydat$BILL_AMT2 <- sapply(mydat$BILL_AMT2 , as.numeric)
mydat$BILL_AMT3 <- sapply(mydat$BILL_AMT3 , as.numeric)
mydat$BILL_AMT4 <- sapply(mydat$BILL_AMT4 , as.numeric)
mydat$BILL_AMT5 <- sapply(mydat$BILL_AMT5 , as.numeric)
mydat$BILL_AMT6 <- sapply(mydat$BILL_AMT6 , as.numeric)
mydat$PAY_AMT1 <- sapply(mydat$PAY_AMT1, as.numeric)
mydat$PAY_AMT2 <- sapply(mydat$PAY_AMT2, as.numeric)
mydat$PAY_AMT3 <- sapply(mydat$PAY_AMT3, as.numeric)
mydat$PAY_AMT4 <- sapply(mydat$PAY_AMT4, as.numeric)
mydat$PAY_AMT5 <- sapply(mydat$PAY_AMT5, as.numeric)
mydat$PAY_AMT6 <- sapply(mydat$PAY_AMT6, as.numeric)

# This is logical but it's corrupting that's why it is commented
#mydat$DEFAULT_PAYMENT_NEXT_MONTH <- sapply(mydat$DEFAULT_PAYMENT_NEXT_MONTH, as.logical)

targetVar <- 'DEFAULT_PAYMENT_NEXT_MONTH'
varsToRemove <- NA

# Can be useful to get all the numeric data fields and categorical data
# fields as variables

catVars <- names(which(sapply(mydat,is.factor)))
numVars <- names(which(sapply(mydat,is.numeric)))
logicalVars <- names(which(sapply(mydat,is.logical)))

# Make sure you didn't miss any variables
stopifnot(length(numVars) + length(catVars) + length(logicalVars)== ncol(mydat))

xVars <- c(catVars, numVars, logicalVars)
xVars <- xVars[!xVars %in% c(targetVar, varsToRemove)]
xVars

# 4. Perform all required EDA on this data set.
#Head of the data set
head(mydat)

#Tail of the data set
tail(mydat)

# Name of the columns
names(mydat)

# Number of Rows
nrow(mydat)

# Number of Columns
ncol(mydat)

# Always examine the data types to ensure they are imported properly
# Many ways to do this
str(mydat)
sapply(mydat, class)

# R provides a summary function to quickly assess data
# As well as Looking for missing data
summary(mydat)

# Dimension of data set
dim(mydat)

# Let's look at the missing data counts
missCounts <- sapply(mydat,function(x) sum(is.na(x)))
missCounts

levels(mydat[,'SEX'])
levels(mydat[,'EDUCATION'])
levels(mydat[,'MARRIAGE'])

levels(mydat[,'PAY_0'])
levels(mydat[,'PAY_2'])
levels(mydat[,'PAY_3'])
levels(mydat[,'PAY_4'])
levels(mydat[,'PAY_5'])
levels(mydat[,'PAY_6'])

# 5.Build a logistic regression model to determine whether or not a
# customer defaulted. Use all of the variables. Validate the model on a
# test data set. Use the comments to discuss the performance of the model.

library('lattice')
library('caret')
library('ggplot2')
trainPct = .8
inTrain <- createDataPartition(y = mydat[,targetVar], p = trainPct, list = FALSE)
train <- mydat[inTrain,]
test <- mydat[-inTrain,]
stopifnot(nrow(train) + nrow(test) == nrow(mydat))

modelForm <- createModelFormula(targetVar, xVars)

fit <- glm(modelForm,family=binomial(link='logit'),data=train)

summary(fit) # display results
anova(fit, test="Chisq")

# Let's see the performance on the test set
# It's giving error because there is new level for LIMIT_BAL
fitted.results <- predict(fit,newdata = test[,xVars],type='response')
hist(fitted.results)
test[,'fitted.results'] <- fitted.results

# We output the probabilities, but we want to turn the probabilities into
# a classification of defaulted or not. .5 is a reasonable starting cutoff.
defaulted.pred <- ifelse(fitted.results > 0.5,1,0)

mean(defaulted.pred)
mean(train[,targetVar])

# Let's use a confusion matrix to evaluate how good our results are
confusion <- confusionMatrix(data = defaulted.pred
                             , reference = test[,targetVar]
                             , dnn = c("Predicted Defaulter", 'Actual Defaulter')
)
confusion


# Let's look at the ROC curve
library(ROCR)
pr <- prediction(fitted.results, test$DEFAULT_PAYMENT_NEXT_MONTH)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# We can look through the graph to see the performance
# Let's look at the precision recall curves
library('DMwR')
PRcurve(preds = fitted.results, trues = test$DEFAULT_PAYMENT_NEXT_MONTH)

# Let's take a look at the deviance
llcomponents <- function(y, predicted.y){
  return(y*log(predicted.y) + (1-y)*log(1-predicted.y))
}

y <- train[,targetVar]
predicted.y <- predict(fit,newdata = train[,xVars],type='response')

deviance <- sign(as.numeric(y) - predicted.y)*sqrt(-2*llcomponents(as.numeric(y), predicted.y))

summary(deviance)

# Extract the AIC
aic<- 2 * length(fit$coefficients) - 2*logLik(fit)
aic

# 6. Using forward selection, determine the best model.

modelForm <- createModelFormula(targetVar, xVars)
fit1 <- glm(modelForm,family=binomial(link='logit'),data=train)
fit2 <- glm(DEFAULT_PAYMENT_NEXT_MONTH ~ 1,family=binomial(link='logit'),data=train)

for_aic <- step(fit2, direction = "forward",scope = list(upper=fit1, lower=fit2))
for_aic

# 7. Using the backwards selection function you implemented in #1
# , determine the best model

modelForm <- createModelFormula(targetVar, xVars)
model <- glm(modelForm,family=binomial(link='logit'),data=train)
ret_back <- backwards(model)
ret_back

# 8. Run an implementation of backwards selection found in an R package on this
# data set. Discuss any differences between the results of this implementation
# and your implemnetation in question 7.

library(MASS)

modelForm <- createModelFormula(targetVar, xVars)
model <- glm(modelForm,family=binomial(link='logit'),data=train)
ret_for<-stepAIC(model,direction="backward")
ret_for

# 9. Run lasso regression on the data set. Briefly discuss how you determined
# the appropriate tuning parameters.
library(Matrix)
library(lars)
library(glmnet)
x <- scale( cbind(mydat$LIMIT_BAL,mydat$SEX,mydat$EDUCATION,mydat$MARRIAGE,mydat$AGE,mydat$PAY_0,mydat$PAY_2,mydat$PAY_3,mydat$PAY_4,mydat$PAY_5,mydat$PAY_6,mydat$BILL_AMT1,mydat$BILL_AMT2,mydat$BILL_AMT3,mydat$BILL_AMT4,mydat$BILL_AMT5,mydat$BILL_AMT6,mydat$PAY_AMT1,mydat$PAY_AMT2,mydat$PAY_AMT3,mydat$PAY_AMT4,mydat$PAY_AMT5,mydat$PAY_AMT6) )
y <- as.numeric(as.matrix(mydat$DEFAULT_PAYMENT_NEXT_MONTH))
las <- lars(x, y, type="lasso")
las
plot(las, plottype="coefficients")
plot(las, plottype="Cp")

#
# By using cross-validation with the lasso,
# a good (hopefully near-optimal) value for
# the "fraction" can be determined.
cvlas <- cv.lars(x, y, type="lasso")
cvlas
frac <- cvlas$fraction[which.min(cvlas$cv)]
frac
las.coef <- predict.lars(las, type="coefficients", mode="fraction", s=frac)
las.coef

#
# As a check, let's see if setting the value of
# s (the fraction, in the mode being used) to 1
# yields the coefficient values from the OLS fit.
las.coef_s_1 <- predict.lars(las, type="coefficients", mode="fraction", s=1)
las.coef_s_1
# Hence tuning parameters s could be determined

# 10. Run ridge regression on the data set. Briefly discuss how you determined
# the appropriate tuning parameters.
x <- scale( cbind(mydat$LIMIT_BAL,mydat$SEX,mydat$EDUCATION,mydat$MARRIAGE,mydat$AGE,mydat$PAY_0,mydat$PAY_2,mydat$PAY_3,mydat$PAY_4,mydat$PAY_5,mydat$PAY_6,mydat$BILL_AMT1,mydat$BILL_AMT2,mydat$BILL_AMT3,mydat$BILL_AMT4,mydat$BILL_AMT5,mydat$BILL_AMT6,mydat$PAY_AMT1,mydat$PAY_AMT2,mydat$PAY_AMT3,mydat$PAY_AMT4,mydat$PAY_AMT5,mydat$PAY_AMT6) )
y <- as.numeric(as.matrix(mydat$DEFAULT_PAYMENT_NEXT_MONTH))
ridgefit<-glmnet(x,y,alpha=0)
plot(ridgefit,xvar="lambda",label=TRUE)

# 11. Run naive bayes on the data set.
library(e1071)
modelForm <- createModelFormula(targetVar, xVars)
model <- naiveBayes(modelForm, data = train)
model
class(model)
summary(model)

# 12. Build a decision tree to classify the customers as defaulted
# or not-defaulted. Plot the resulting tree. Discuss whether you
# feel this is a good model.

library(rpart)
modelForm <- createModelFormula(targetVar, xVars)
fit <- rpart(modelForm,data=train, method="class")

fit
summary(fit)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
Actual <- test$DEFAULT_PAYMENT_NEXT_MONTH

con <- confusionMatrix(reference = Actual, data = Prediction)
con

# Since the accuracy is 82% , so it's not a very good model, it's performing average

# 13. Build a random forest model and apply it to classify the test data set.

#install.packages('randomForest')
library(randomForest)
set.seed(415)

# Caret works best if you specify the X and Y explicitly
x = train[,xVars]
# We need to convert this to a factor, which is annoying
y = as.numeric(train[, targetVar])

fit4 <- randomForest(x = x, y = y, data=train,importance=TRUE,ntree=2000)

fit4
varImpPlot(fit4)

# Again, let's test our model
Prediction4 <- predict(fit4, test, type = "response")
Actual <- test$DEFAULT_PAYMENT_NEXT_MONTH

confusionMatrix(reference = Actual, data = Prediction4)

# 14. Discuss the comparative performance of all of the models used. How should
# we determine which is best? Provide justification for your answer.
# We will use performance metrics in order to judge best model.
# These are accuracy, F-Score, ROC curve, average precision, precision/recall break even point.
# There are few more metrics such as the probability metrics,squared error and cross-entropy
# which interpret the predicted values of each case as the conditional probability of 
# that case being in the positive class

# To justify it let us look on the output of 
# confusionMatrix(reference = Actual, data = Prediction) , where "Prediction" denotes
# values of predicted class from each model.Using this we can find out precision , recall
# accuracy , F1 Score to select the best model whose overall scores would be highest
