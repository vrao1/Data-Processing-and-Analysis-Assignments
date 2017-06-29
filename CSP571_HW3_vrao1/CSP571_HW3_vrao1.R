# CSP571
# Homework 3


# 1.Load in the auto mpg data set: https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data

#install.packages("data.table")
download.file(url = "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data", destfile = "auto-mpg.csv")
print("Auto MPG data set has been downloaded")
mydat <- read.csv(file = "auto-mpg.csv", sep ='',col.names = c("mpg","cylinders","displacement","horsepower","weight","acceleration","model year","origin","car name"),strip.white=TRUE,na.strings = c('NA','?'))
#mydat <- read.csv(file = "auto-mpg.csv", sep ='',col.names = c("mpg","cylinders","displacement","horsepower","weight","acceleration","model year","origin","car name"),strip.white=TRUE)
mydat$cylinders <- sapply(mydat$cylinders , as.factor)
mydat$model.year <- sapply(mydat$model.year , as.factor)
mydat$origin <- sapply(mydat$origin , as.factor)
mydat$horsepower <- sapply(mydat$horsepower , as.numeric)

# 2. Identify all of the categorical variables, all of the numeric variables
# and all of the binary variables.

# The link 'https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.names' has shown
#Categorical Variables : cylinders, model year, origin
#Numerical Variables : mpg, displacement, horsepower, weight, acceleration
#There are no Binary Variables.

#Following are the column names :
#1. mpg:           continuous
#2. cylinders:     multi-valued discrete
#3. displacement:  continuous
#4. horsepower:    continuous
#5. weight:        continuous
#6. acceleration:  continuous
#7. model year:    multi-valued discrete
#8. origin:        multi-valued discrete
#9. car name:      string (unique for each instance)


# 3. Identify the appropriate descriptive statistics and graph for this data set.
# Execute on those and use the comments to discuss relevant relationships or insights discovered.

summary(mydat)

# Graphs
ggplot(mydat, aes(x = weight, y = mpg)) + geom_point()
# Grpah shows mpg is negatively correlated to weight

ggplot(mydat, aes(x = displacement, y = mpg)) + geom_point()
# Grpah shows mpg is negatively correlated to displacement

ggplot(mydat, aes(x = horsepower, y = mpg)) + geom_point()
# Grpah shows mpg is negatively correlated to horsepower

ggplot(mydat, aes(x = acceleration, y = mpg)) + geom_point()
# Grpah shows mpg is positively correlated to acceleration


# 4. Create a correlation matrix for all of the numeric variables.
# Since these columns ("mpg","displacement","horsepower","weight","acceleration") are the the only numeric variables , 
# So I am drawing correlation plot for those 5 only

corMat <- cor(na.omit(mydat[c("mpg","displacement","horsepower","weight","acceleration")]))
corMat

# 5. Identify the columns (if any) with missing data.

colnames(mydat)[colSums(is.na(mydat)) > 0] 

# 6. Divide the data into a train/test set (80% and 20% respectively) using stratified sampling

#install.packages('SparseM')
#install.packages('lattice')
#install.packages("ggplot2")
#install.packages('caret')
library('lattice')
library('ggplot2')
library('caret')
set.seed(1234)
inTrain <- createDataPartition(y = mydat$mpg, p = 0.8, list = FALSE)
custTrain <- mydat[inTrain,]
custTest <- mydat[-inTrain,]
stopifnot(nrow(custTrain) + nrow(custTest) == nrow(mydat))


# 7. Fit a linear model to the data using the numeric variables only. Calculate the R**2 on the test set.

lm_mpg <- lm(mpg ~ displacement + horsepower + weight + acceleration , custTrain )
summary(lm_mpg)
custTest_mpgPredicted <- predict(lm_mpg, custTest)
head(custTest_mpgPredicted)
custTest_res <- custTest_mpgPredicted - custTest['mpg']
SS.test.total      <- sum((custTest[,'mpg'] - mean(custTest[,'mpg']))^2)
SS.test.residual   <- sum((na.omit(custTest_res))^2)

test.rsq <- SS.test.residual/SS.test.total  
test.rsq
#0.2875341


# 8. Programmatically identify and remove the non-significant variables (alpha = .05). Fit a new model with those variables removed.
# Calculate the R**2 on the test set with the new model. Did this improve performance?

alpha <- 0.05
p_value <- summary(lm_mpg)$coefficients
sigVars <- rownames(p_value[p_value[,4]<alpha,])[2]
revisedModel <- lm(paste("mpg ~", paste(sigVars, collapse = '+ ')), custTrain)
summary(revisedModel)

custTest[,"mpgPredicted"] <- predict(revisedModel, custTest)

custTest[,'res'] <- custTest[,'mpgPredicted'] - custTest[,'mpg']
SS.test.total.rev      <- sum((custTest$mpg - mean(custTest$mpg))^2)
SS.test.residual.rev   <- sum((na.omit(custTest$res))^2)

test.rsq.rev <- SS.test.residual.rev/SS.test.total.rev  
test.rsq.rev
#0.4072733
# Improvement is there

# 9. Attempt to fit a model on all of the relevant independent variables (including carName).
# Then calculate the R**2 on a test set. You will likely encounter an error.
# Explain why this error occurs. Fix this error.
alpha <- 0.05
p_value <- summary(lm_mpg)$coefficients
sigVars <- rownames(p_value[p_value[,4]<alpha,])[2]
revisedModel1 <- lm(paste("mpg ~ car.name + origin + model.year + ", paste(sigVars, collapse = '+ ')), custTrain)
summary(revisedModel1)

custTest[,"mpgPredicted"] <- predict(revisedModel1, custTest) # Giving error due to character variable (string) introduced here as car.name

revisedModel2 <- lm(paste("mpg ~ origin + model.year + ", paste(sigVars, collapse = '+ ')), custTrain)
summary(revisedModel2)
custTest[,'res'] <- custTest[,'mpgPredicted'] - custTest[,'mpg']
SS.test.total.rev.1      <- sum((custTest$mpg - mean(custTest$mpg))^2)
SS.test.residual.rev.1   <- sum((na.omit(custTest$res))^2)

test.rsq.rev.1 <- SS.test.residual.rev.1/SS.test.total.rev.1  
test.rsq.rev.1

# 10. Determine the relationship between model year and mpg.
# Interpret this relationship.
# Theorize why this relationship might occur.

mpgCol <- mydat$mpg
modelYear <- mydat$model.year

corCo <- cor(mpgCol,modelYear)
corCo

#The correlation coefficient of mpg and model year is 0.5786673
#This shows the positive relationship btween both.
#As model year increases mpg also increases

# 11. Build the best linear model you can (as measured by R**2 on the test data)
# Record the value obtained in the comments below. Make sure to show all your code.
#R**2 values are following from previous work :
test.rsq
#0.2875341
test.rsq.rev
#0.4072733
test.rsq.rev.1
#0.4072733

# All R**2 values have been fetched from above code in different sections
#Hence The best model could be the one with the all significant features and adding independent variables like model year and origin doesn't make any difference to the model
