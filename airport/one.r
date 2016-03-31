# install.packages("caret")

# Sets the enviroment
getwd()
setwd("C:\\Users\\gomezaa\\Documents\\GitHub\\IntroMLR\\airport")

# #
# 1 - Load the data into a data frame
originalData <- read.csv2('.\\data\\Jan_2015_ontime.csv',sep = ",", header = TRUE, stringsAsFactors = FALSE)

# #
# 2 - Explore the data.

nrow(originalData) # 469,968
ncol(originalData)
head(originalData)
tail(originalData)

# produce result summaries of the results of various model fitting functions
summary(originalData)
# Compactly display the internal structure of an R object
str(originalData)
# table uses the cross-classifying factors to build a contingency table 
table(originalData$DAY_OF_WEEK)
table(originalData$DEST)

# #
# 3 - Filter the data, set a smaller scope.
airports <- c('ATL','LAX', 'ORD', 'DFW', 'JFK', 'SFO', 'CLT', 'LAS', 'PHX')
filteredData <- subset(originalData, DEST %in% airports & ORIGIN %in% airports)

# Explore the new subset
table(filteredData$DEST)
nrow(filteredData)
nrow(originalData)

# # 
# 4 - Clean the data


head(filteredData)
tail(filteredData)
# Remove the column X
filteredData$X <- NULL

# #
# 4.1 Look for correlations
head(filteredData)

# Origin Correlation
oriDest <- c("ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_AIRPORT_ID")

filteredData[oriDest]

tail(filteredData[oriDest])

cor(filteredData[oriDest])


# DEST Correlation
cor(filteredData[c("DEST_AIRPORT_SEQ_ID", "DEST_AIRPORT_ID")])

# Drop the columns
filteredData$ORIGIN_AIRPORT_SEQ_ID <- NULL
filteredData$DEST_AIRPORT_SEQ_ID <- NULL


# # Mismatched, no loop required

mismatched <- filteredData[filteredData$CARRIER != filteredData$UNIQUE_CARRIER,]
nrow(mismatched)

# Drop the column
filteredData$UNIQUE_CARRIER <- NULL




### More to do
  


plot(originalData$DAY_OF_WEEK, originalData$ARR_DEL15)

# Delays per day
originalData$ARR_DEL15 <- as.factor(originalData$ARR_DEL15)

originalData$DEST <- as.factor(originalData$DEST)
originalData$DAY_OF_WEEK <- as.factor(originalData$DAY_OF_WEEK)
