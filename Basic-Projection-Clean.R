# Basic Projection - Clean
#                                         |
# Run following commands in one direction |
#                                         V


# set working directory - location of PrepData.Csv
setwd('/Users/Png/Desktop/SSTM2018/workspace/Projection')


# Highlight-1 -> Define Functions - & read data
# =========================================================
# Start Highlight-1
# =========================================================
# Import Library
library(rpart)
library(randomForest)
library(mgcv)
library(tidyverse)
library(neuralnet)
library(GGally)
library(RColorBrewer)

# Functions
readData <- function() {
  
  tableData <- read.csv('PrepData.csv', header = TRUE)
  
  minimumVisitorTotal <- 500      # minimum number of visitors to consider
  newTableData <<- subset(tableData, tableData[ , "visitorTotal"] >= minimumVisitorTotal) 
  newTableData$avgTemp <<- (newTableData$minTemp + newTableData$maxTemp) / 2
  newTableData$date <<- as.Date(newTableData$date)
  
  minDate <<- min(newTableData$date)
  maxDate <<- max(newTableData$date)
  
  baseData <<- newTableData
}
readData()      # define readData() and immediately import data

# Model Generation - Adjust Model Parameters, if necessary
generateLM <- function() {
  # convert categorical data (binary)
  lmData <- newTableData
  lmData$Fcloudy <- factor(newTableData$cloudy)
  lmData$Frain <- factor(newTableData$rain)
  lmData$Fsnow <- factor(newTableData$snow)
  lmData$Fstorm <- factor(newTableData$storm)
  lmData$Fweekend <- factor(newTableData$weekend)
  lmData$Fholiday <- factor(newTableData$holiday)
  lmData <<- lmData
  
  trainIndex <- sample(1:nrow(lmData), 0.8 * nrow(lmData))
  testIndex <- setdiff(1:nrow(lmData), trainIndex)
  
  trainData <<- lmData[trainIndex,]   # keep for leveling predictor
  testData <- lmData[testIndex,]
  
  lm1 <<- lm(visitorTotal ~ avgTemp + Fstorm + Fsnow + Frain + Fcloudy + wind + Fholiday + Fweekend, trainData) # removed Fclear (flat variable)
  
}

generateRPART <- function() {
  # convert categorical data (binary)
  rpartData <- newTableData
  rpartData$Fcloudy <- factor(newTableData$cloudy)
  rpartData$Frain <- factor(newTableData$rain)
  rpartData$Fsnow <- factor(newTableData$snow)
  rpartData$Fstorm <- factor(newTableData$storm)
  rpartData$Fweekend <- factor(newTableData$weekend)
  rpartData$Fholiday <- factor(newTableData$holiday)
  rpartData <<- rpartData
  
  trainIndex <- sample(1:nrow(rpartData), 0.8 * nrow(rpartData))
  testIndex <- setdiff(1:nrow(rpartData), trainIndex)
  
  trainData <- rpartData[trainIndex,]
  testData <- rpartData[testIndex,]
  
  rpart1 <<- rpart(visitorTotal ~ avgTemp + Fstorm + Fsnow + Frain + Fcloudy + wind + Fholiday + Fweekend, trainData)
  
}

generateRandomForest <- function() {
  # convert categorical data (binary)
  randomForestData <- newTableData
  
  randomForestData$Fcloudy <- factor(newTableData$cloudy)
  randomForestData$Frain <- factor(newTableData$rain)
  randomForestData$Fsnow <- factor(newTableData$snow)
  randomForestData$Fstorm <- factor(newTableData$storm)
  randomForestData$Fweekend <- factor(newTableData$weekend)
  randomForestData$Fholiday <- factor(newTableData$holiday)
  randomForestData <<- randomForestData
  
  trainIndex <- sample(1:nrow(randomForestData), 0.8 * nrow(randomForestData))
  testIndex <- setdiff(1:nrow(randomForestData), trainIndex)
  
  trainData <- rpartData[trainIndex,]
  testData <- rpartData[testIndex,]
  
  rf1 <<- randomForest(visitorTotal ~ avgTemp + Fstorm + Fsnow + Frain + Fcloudy + wind + Fholiday + Fweekend, trainData, ntree=100) # removed Fclear (flat variable)
}

generateANN <- function() {
  annData <<- newTableData
  
  trainIndex <- sample(1:nrow(annData), 0.8 * nrow(annData))
  testIndex <- setdiff(1:nrow(annData), trainIndex)
  
  trainData <- annData[trainIndex,]
  testData <- annData[testIndex,]
  
  # remove unused columns
  trainData = trainData[, c('visitorTotal', 'avgTemp', 'storm', 'snow', 'rain', 'cloudy', 'wind', 'weekend', 'holiday')]
  testData = testData[, c('visitorTotal', 'avgTemp', 'storm', 'snow', 'rain', 'cloudy', 'wind', 'weekend', 'holiday')]
  
  # auto min-max for scale
  train.colmins = apply(trainData,2,min)
  train.colmaxs = apply(testData,2,max)
  train.colmaxs['storm'] <- 1
  train.colmaxs['snow'] <- 1
  train.colmaxs['rain'] <- 1
  train.colmaxs['cloudy'] <- 1
  train.colmaxs['weekend'] <- 1
  train.colmaxs['holiday'] <- 1
  
  # scale data down to [0, 1]
  scaled.train = as.data.frame(scale(trainData,center = train.colmins, scale = train.colmaxs - train.colmins))
  scaled.test = as.data.frame(scale(testData,center = train.colmins, scale = train.colmaxs - train.colmins))
  
  n = names(scaled.train)
  f = as.formula(paste("visitorTotal~", paste(n[!n %in% "visitorTotal"], collapse = "+")))
  #print(scaled.train)
  ann1 <<- neuralnet(f, 
                     data = scaled.train,  
                     hidden=c(5, 3, 1), 
                     lifesign = 'full',
                     linear.output=T)
  
}

modifyDateRange <- function(startDate, endDate) {
  
  if(!is_empty(startDate) && !is_empty(endDate)) {
    
    isValidRange = nrow(newTableData[newTableData$date>=as.Date(startDate) & newTableData$date<=as.Date(endDate),])
    
    if(isValidRange > 0) {
      newDataByDate <- newTableData[newTableData$date>=as.Date(startDate) & newTableData$date<=as.Date(endDate),]
      
      # reset lm data
      lmData <- newDataByDate
      lmData$Fcloudy <- factor(newDataByDate$cloudy)
      lmData$Frain <- factor(newDataByDate$rain)
      lmData$Fsnow <- factor(newDataByDate$snow)
      lmData$Fstorm <- factor(newDataByDate$storm)
      lmData$Fweekend <- factor(newDataByDate$weekend)
      lmData$Fholiday <- factor(newDataByDate$holiday)
      lmData <<- lmData
      
      # reset rpart data
      rpartData <- newDataByDate
      rpartData$Fcloudy <- factor(newDataByDate$cloudy)
      rpartData$Frain <- factor(newDataByDate$rain)
      rpartData$Fsnow <- factor(newDataByDate$snow)
      rpartData$Fstorm <- factor(newDataByDate$storm)
      rpartData$Fweekend <- factor(newDataByDate$weekend)
      rpartData$Fholiday <- factor(newDataByDate$holiday)
      rpartData <<- rpartData
      
      # reset random forest data
      randomForestData <- newDataByDate
      
      randomForestData$Fcloudy <- factor(newDataByDate$cloudy)
      randomForestData$Frain <- factor(newDataByDate$rain)
      randomForestData$Fsnow <- factor(newDataByDate$snow)
      randomForestData$Fstorm <- factor(newDataByDate$storm)
      randomForestData$Fweekend <- factor(newDataByDate$weekend)
      randomForestData$Fholiday <- factor(newDataByDate$holiday)
      randomForestData <<- randomForestData
      
      # reset ANN data
      annData <<- newDataByDate
      
      baseData <<- newDataByDate
    }
  } 
}

# Preview Plot

generateBasePlot <- function() {
  plot(baseData$visitorTotal, 
       type='l', col='gray', xlab = 'Date', ylab = 'Visitors', xaxt='n')
}

plotPreviewLM <- function() {
  plm1 <<- predict(lm1, lmData)
  lines(plm1,col=brewer.pal(n = 8, name = "Paired")[2])
}

plotPreviewRPART <- function() {
  prpart1 <<- predict(rpart1, rpartData)
  lines(prpart1,col=brewer.pal(n = 8, name = "Paired")[4])
}

plotPreviewRandomForest <- function() {
  prf1 <<- predict(rf1, randomForestData)
  lines(prf1,col=brewer.pal(n = 8, name = "Paired")[6])
}

plotPreviewANN <- function() {
  croppedANNData = annData[,c('visitorTotal', 'avgTemp', 'storm', 'snow', 'rain', 'cloudy', 'wind', 'weekend', 'holiday')]
  
  # scale down [0, 1]
  data.colmins = apply(croppedANNData,2,min)
  data.colmaxs = apply(croppedANNData,2,max)
  data.colmaxs['storm'] <- 1
  data.colmaxs['snow'] <- 1
  data.colmaxs['rain'] <- 1
  data.colmaxs['cloudy'] <- 1
  data.colmaxs['weekend'] <- 1
  data.colmaxs['holiday'] <- 1
  
  scaled.data = as.data.frame(scale(croppedANNData, center = data.colmins, scale = data.colmaxs - data.colmins))
  
  pred.NN1 = compute(ann1, scaled.data[,c('avgTemp', 'storm', 'snow', 'rain', 'cloudy', 'wind', 'weekend', 'holiday')])
  
  # scale back
  pred.NN1_ = pred.NN1$net.result * (max(newTableData$visitorTotal) - min(newTableData$visitorTotal)) + min(newTableData$visitorTotal)
  
  lines(pred.NN1_, col = brewer.pal(n = 8, name = "Paired")[8])
  
}

# Predict and display
predictLM <- function(factoredPredictingCondition) {
  pred <- predict(lm1, factoredPredictingCondition)
  #print(pred)
  predictedVisitor <<- pred
}
predictRPART <- function(factoredPredictingCondition) {
  pred <- predict(rpart1, factoredPredictingCondition)
  #print(pred)
  predictedVisitor <<- pred
}
predictRandomForest <- function(factoredPredictingCondition) {
  pred <- predict(rf1, factoredPredictingCondition)
  #print(pred)
  predictedVisitor <<- pred
}
predictANN <- function(predictingCondition) {
  croppedANNData = newTableData[,c('visitorTotal', 'avgTemp', 'storm', 'snow', 'rain', 'cloudy', 'wind', 'weekend', 'holiday')]
  # scale down [0, 1]
  data.colmins = apply(croppedANNData,2,min)
  data.colmaxs = apply(croppedANNData,2,max)
  data.colmaxs['storm'] <- 1
  data.colmaxs['snow'] <- 1
  data.colmaxs['rain'] <- 1
  data.colmaxs['cloudy'] <- 1
  data.colmaxs['weekend'] <- 1
  data.colmaxs['holiday'] <- 1
  
  
  scaled.data = as.data.frame(scale(predictingCondition, center = data.colmins, scale = data.colmaxs - data.colmins))
  
  pred.NN1 = compute(ann1, scaled.data[,c('avgTemp', 'storm', 'snow', 'rain', 'cloudy', 'wind', 'weekend', 'holiday')])
  
  # scale back
  pred.NN1_ = pred.NN1$net.result * (max(newTableData$visitorTotal) - min(newTableData$visitorTotal)) + min(newTableData$visitorTotal)
  
  #print(pred.NN1_)
  predictedVisitor <<- pred.NN1_
}

# =========================================================
# End Highlight-1 
# =========================================================

# if no error occurs, proceed to Highlight-2

# Highlight-2 -> Create Models (Data is imported in Highlight-1)
# =========================================================
# Start Highlight-2
# =========================================================
generateLM()
generateRPART()
generateRandomForest()
generateANN()

# =========================================================
# End Highlight-2
# =========================================================


# if no error occurs, proceed to Plot Prediction Preview


# =========================================================
# Plot Prediction Preview - line by line
# =========================================================
startDate <<- '2016-10-10'    # define start date range
endDate <<- '2018-10-10'      # define end date range

modifyDateRange(startDate, endDate)   # modify date range in data

generateBasePlot()              #   plot real data
plotPreviewLM()                 #   plot Linear Regression prediction
plotPreviewRPART()              #   plot Recursive Partitioning prediction
plotPreviewRandomForest()       #   plot Random Forest prediction
plotPreviewANN()                #   plot Artificial Neural Network prediction

# =========================================================
# Use Models to Predict New Data
# =========================================================

# Define new Data
predictingCondition <- data.frame(            # define data
  visitorTotal = 0,                           # always 0
  avgTemp = 20,                               # average temperature
  storm = 0,                                  # 1 or 0
  snow = 0,                                   # 1 or 0
  rain = 0,                                   # 1 or 0
  cloudy = 0,                                 # 1 or 0
  wind = 2,                                   # wind speed 0 ... n
  weekend = 0,                                # 1 or 0
  holiday = 0                                 # 1 or 0
)


# Convert data format - run line by line
factoredPredictingCondition <- predictingCondition      
factoredPredictingCondition$Fcloudy <- factor(factoredPredictingCondition$cloudy, levels = c(0,1))
factoredPredictingCondition$Frain <- factor(factoredPredictingCondition$rain, levels = c(0,1))
factoredPredictingCondition$Fsnow <- factor(factoredPredictingCondition$snow, levels = c(0,1))
factoredPredictingCondition$Fstorm <- factor(factoredPredictingCondition$storm, levels = c(0,1))
factoredPredictingCondition$Fweekend <- factor(factoredPredictingCondition$weekend, levels = c(0,1))
factoredPredictingCondition$Fholiday <- factor(factoredPredictingCondition$holiday, levels = c(0,1))

# =========================================================
# Use New Data and Models to Predict
# =========================================================
plm <- predictLM(factoredPredictingCondition)
prpart <- predictRPART(factoredPredictingCondition)
prf <- predictRandomForest(factoredPredictingCondition)
pann <- predictANN(predictingCondition)


# =========================================================
# Prepare to plot graph
# =========================================================
H <- c(plm, prpart, prf, pann)
M <- c("Linear Regression", "Recursive Partitioning", "Random Forest", "Artificial Neural Network")
colPallete <- brewer.pal(n = 8, name = "Paired")
colPalleteSelection = c(colPallete[2], colPallete[4], colPallete[6], colPallete[8])

# Plot Graph
barP <- barplot(H, names.arg=M, xlab="Models", ylab="Prospective Visitors", 
                col= colPalleteSelection)
text(x = barP, y = H / 2,labels=as.integer(H),cex=1)


# =========================================================
# Model Description - Statistical Data
# =========================================================
summary(lm1)              # Linear Regression
summary(rpart1)           # Recursive Partitioning
rf1                       # Random Forest
ann1                      # Artificial Neural Network
plot(ann1)                # Plot ANN graph




