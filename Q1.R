require(ggplot2)
require(GGally)
require(glmnet)

## Step 1
## Read the data into a data frame
ceo_data <- read.csv(".\\ceo.csv")
ceo_data<-as.data.frame(ceo_data)
names(ceo_data)
summary(ceo_data)

## Step 2 
## Remove last column of de data frame
ceo_data <- ceo_data[,0:7]
names(ceo_data)
summary(ceo_data)

## Step 3 and 4
## Selecting sufficiently random training and test sets.
## For now deleted using the if false statement. Takes a lot of time.
#if(FALSE){

  minDif <- 999999999999
  count <- 0
  iter <- 50000
  while(count < iter){
  
    #Generate random sample for training and testing
    train_index <- sample(nrow(ceo_data), floor(nrow(ceo_data) * 0.75))
    trainData <- ceo_data[train_index, ]
    testData <- ceo_data[-train_index, ]
    difTrain <- abs(cov(ceo_data) - cov(trainData))
    difTest <- abs(cov(ceo_data) - cov(testData))
    totalDif <- sum(difTrain) + sum(difTest)
    if(totalDif < minDif){
      train_index_min <- train_index
      minDif <- totalDif    
    }
    count <- count + 1  
    if(count - floor(count/1000)*1000 == 0){
      print(count)
    }
  }
  # Use the best set
  trainData <- ceo_data[train_index_min, ]
  testData <- ceo_data[-train_index_min, ]
#}
### Step 4.
## EDA to check if the split is random enough
## Pair Plots
ggpairs(ceo_data, diag=list(continuous="densityDiag",   discrete="barDiag"), axisLabels="show")
ggpairs(trainData, diag=list(continuous="densityDiag",   discrete="barDiag"), axisLabels="show", aes(colour='red'))
ggpairs(testData, diag=list(continuous="densityDiag",   discrete="barDiag"), axisLabels="show", aes(colour='blue'))
## Box Plots
boxplot(ceo_data$salary, trainData$salary, testData$salary,
        names = list("DataSet", "Train", 'Test'), main = "Salary")
boxplot(ceo_data$totcomp, trainData$totcomp, testData$totcomp,
        names = list("DataSet", "Train", 'Test'), main = "TotComp")
boxplot(ceo_data$tenure, trainData$tenure, testData$tenure,
        names = list("DataSet", "Train", 'Test'), main = "tenure")
boxplot(ceo_data$age, trainData$age, testData$age,
        names = list("DataSet", "Train", 'Test'), main = "age")
boxplot(ceo_data$sales, trainData$sales, testData$sales,
        names = list("DataSet", "Train", 'Test'), main = "sales")
boxplot(ceo_data$profits, trainData$profits, testData$profits,
        names = list("DataSet", "Train", 'Test'), main = "profits")
boxplot(ceo_data$assets, trainData$assets, testData$assets,
        names = list("DataSet", "Train", 'Test'), main = "assets")

### Step 5
# Standarizing independent variables
y.var = names(trainData) %in% c('totcomp')
trainData.x <- scale(trainData[!y.var])
trainData.y <- trainData[y.var]

colMeans(trainData.x)
sd(trainData.x[,2])

# REMEMBER When I standarize the test set, I have to use the same mean and stdev to center and scale.

### Step 6 Fitting a Lasso model with 10-fold CV
lasso.cv=cv.glmnet(trainData.x, unlist(trainData.y), alpha = 1)
plot(lasso.cv)
bestlam = lasso.cv$lambda.min
out = glmnet(trainData.x, unlist(trainData.y), alpha=1)
lasso.coef = predict(out,type = "coefficients", s=bestlam)
lasso.coef

### Step 7 Fitting a Lasso model with 10-fold CV to log(totcomp)
trainData.y.log <- log(trainData.y)
lasso.cv.log=cv.glmnet(trainData.x, unlist(trainData.y.log), alpha = 1)
plot(lasso.cv.log)
lasso.cv.log
bestlam.log = lasso.cv.log$lambda.min
out.log = glmnet(trainData.x, unlist(trainData.y.log), alpha=1)
lasso.coef.log = predict(out,type = "coefficients", s=bestlam.log)
lasso.coef.log

### Step 8
testData.y <- testData$totcomp
testData.x <- testData[!y.var]

# Get Mean and Stdev of training set to standarize the X's in the test set
means.x <- colMeans(trainData[,!y.var])
stdev.x <- sqrt(diag(var(trainData[,!y.var])))
testData.x <- as.matrix((testData.x - means.x) / stdev.x)

## First Lasso Model
lasso.pred <- predict(out,newx=testData.x,s=bestlam)
lasso.error <- mean((lasso.pred-testData.y)^2)
lasso.error

## Second Lasso Model (log)
lasso.pred.log <- predict(out.log, newx=testData.x, s=bestlam.log)
lasso.error.log <- mean(((lasso.pred.log)-log(testData.y))^2)
lasso.error.log

matrix(c(lasso.pred.log, exp(lasso.pred.log), testData.y, exp(lasso.pred.log) - log(testData.y) ), nrow=length(lasso.pred.log))
