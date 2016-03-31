
# Sets the enviroment -----------------------------------------------------
setwd("C:\\zMachineLearningR\\code")
getwd()
# #

# 1 - Load the data and filter it ----
originalData <- read.csv2('.\\data\\Jan_2015_ontime.csv', sep = ",", header = TRUE, stringsAsFactors = FALSE)

# 1.1 - See a little about the data.
nrow(originalData) # 469,968 rows
ncol(originalData) # 24 columns
head(originalData, 3) # The first values
tail(originalData) # The last values

str(originalData) # General structure.

# 1.2 - Filter the list of data, a smaller sample will go faster.

airports <- c ('CLT', 'LAS', 'PHX', 'ATL', 'LAX', 'ORD', 'DFW', 'JFK', 'SFO')
filteredData <- subset(originalData, DEST %in% airports & ORIGIN %in% airports)
nrow(filteredData) # 32,716 rows
##

# 2 - Explore the data & Cleaning the data (Tidy data) ----
# - Try to eliminate any UNUSED variables (columns)
# - Eliminate DUPLICATED variables (columns)
# - Look for CORRELATED variables (columns)

head(filteredData)
tail(filteredData)

## 2.1 Erase the unused X Column.

filteredData$X <- NULL


## 2.2 Look for duplicated variables (columns) 

head(filteredData,5)

head(filteredData[c("UNIQUE_CARRIER", "CARRIER")])

# Look if they are all the same
mismatched <- filteredData[filteredData$UNIQUE_CARRIER != filteredData$CARRIER,]

nrow(mismatched) # <0 rows> (or 0-length row.names)

filteredData$UNIQUE_CARRIER <- NULL


## 2.3 Look for correlated variables (columns)

head(filteredData,5)

originCorrelated <- c("ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_AIRPORT_ID")
destCorrelated <- c("DEST_AIRPORT_SEQ_ID", "DEST_AIRPORT_ID")

# Origin is correlated
head(filteredData[originCorrelated])
cor(filteredData[originCorrelated])
filteredData$ORIGIN_AIRPORT_SEQ_ID <- NULL

# Dest is correlated
head(filteredData[destCorrelated])
cor(filteredData[destCorrelated])
filteredData$DEST_AIRPORT_SEQ_ID <- NULL

## 2.4 To build our model, we only need complete data.

onTimeData <- filteredData[!is.na(filteredData$ARR_DEL15) & filteredData$ARR_DEL15!="" & !is.na(filteredData$DEP_DEL15) & filteredData$DEP_DEL15!="",]
nrow(filteredData) # 32,716
nrow(onTimeData)   # 32,124
nrow(filteredData) - nrow(onTimeData)

### 2.5 Set the correct format

str(onTimeData)

onTimeData$DISTANCE <- as.integer(onTimeData$DISTANCE)
onTimeData$CANCELLED <- as.integer(onTimeData$CANCELLED)
onTimeData$DIVERTED <- as.integer(onTimeData$DIVERTED)

str(onTimeData)

onTimeData$ARR_DEL15 <- as.factor(onTimeData$ARR_DEL15)
onTimeData$DEP_DEL15 <-as.factor(onTimeData$DEP_DEL15)

str(onTimeData)

onTimeData$DEST_AIRPORT_ID <- as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_ID <- as.factor(onTimeData$ORIGIN_AIRPORT_ID)
onTimeData$DAY_OF_WEEK <- as.factor(onTimeData$DAY_OF_WEEK)
onTimeData$DEST <- as.factor(onTimeData$DEST)
onTimeData$ORIGIN <- as.factor(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK <- as.factor(onTimeData$DEP_TIME_BLK)
onTimeData$CARRIER <- as.factor(onTimeData$CARRIER)

str(onTimeData)

### 2.6 Basic data insight

# What day of the week are more delays?
plot(onTimeData$DAY_OF_WEEK, onTimeData$ARR_DEL15)


# What is the carriers delay performance?
plot(onTimeData$CARRIER, onTimeData$ARR_DEL15)
# "OO","SkyWest Airlines Inc. (2003 - )"
# "DL","Delta Air Lines Inc. (1960 - )"

# What is the carriers delay performance?
plot(onTimeData$DEST, onTimeData$ARR_DEL15)


plot(onTimeData$ARR_DEL15)

tapply(onTimeData$ARR_DEL15, onTimeData$ARR_DEL15, length)

(6460 / (nrow(onTimeData))) # 20% of delays

### By using ggplot2 

library(ggplot2)

ggplot(data = onTimeData , aes(x=ARR_DEL15, y=ARR_DEL15, fill=ARR_DEL15)) +
  geom_bar(stat="identity")

ggplot(data = onTimeData , aes(x=DAY_OF_WEEK, y=ARR_DEL15, fill=DAY_OF_WEEK)) +
  geom_bar(stat="identity")

ggplot(data = onTimeData , aes(x=DAY_OF_WEEK, y=ARR_DEL15)) +
  geom_bar(stat="identity")


##



# 3 - Use a Logistic Regression ML algorithm ----

#----------------------------------------------------------------
# Training the Algorithm
#----------------------------------------------------------------

# # 3.1 Set up ----

# install.packages('caret')

# Load caret
library(caret)

##


# # 3.2 Create the train and test data  ----

# Set random number seed for reproducability
# We need it to have the same results when spliting the data.
set.seed(122515)

# set the columns we are going to use to train algorithm
featureCols <- c("ARR_DEL15", "DAY_OF_WEEK", "CARRIER", "DEST","ORIGIN","DEP_TIME_BLK")

# Filtered version of onTimeData dataframe
onTimeDataFiltered <- onTimeData[,featureCols]

# Split the data in a "smart" way by randonly sampling it.
# This 70% should have enought delays flights

inputTrainRows <- createDataPartition(onTimeDataFiltered$ARR_DEL15, p=0.70, list=FALSE)

head(inputTrainRows)

##

# # 3.3 TRAIN your data -----

# The TRAINED data frame, it should be 70%
trainedData <- onTimeDataFiltered[inputTrainRows,]

# The TESTED data Frame, it sould be 30%
testData <- onTimeDataFiltered[-inputTrainRows,]

# Ensure the split is ok.
# trainedData should be 70%
nrow(trainedData)/(nrow(testData) + nrow(trainedData)) * 100
# testData Should be 30%
nrow(testData)/(nrow(testData) + nrow(trainedData)) * 100

## 


# # 3.4 Create the TRAIN prediction model

#  Logistic Regression
logisticRegressionModel <- train(ARR_DEL15 ~ ., data = trainedData, method = "glm", family = "binomial", trControl = trainControl(method="cv", number=10, repeats=10))

tail(logisticRegressionModel)
head(logisticRegressionModel)

##

#----------------------------------------------------------------
# Test the Model
#----------------------------------------------------------------

logisticRegressionPrediction <- predict(logisticRegressionModel, testData)

logRegConfMatrix <- confusionMatrix(logisticRegressionPrediction, testData[,"ARR_DEL15"])
logRegConfMatrix

## 


# # Use a Randon Forest ----

# It creates multiple decision trees and uses bagging to improve performance


#install.packages('randomForest')

#  load the random forest library into the current session
library(randomForest)

# THIS TAKES TIME
rfModel <- randomForest(trainedData[-1], trainedData$ARR_DEL15, proximity = TRUE, importance = TRUE)
rfModel

#   Random Forest
rfValidation <- predict(rfModel, testData)
#    Get detailed statistics of prediction versus actual via Confusion Matrix 
rfConfMat <- confusionMatrix(rfValidation, testData[,"ARR_DEL15"])
rfConfMat

## 



#install.packages('e1071', dependencies=TRUE)