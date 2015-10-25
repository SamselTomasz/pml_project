## start with getting data

train_data_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_data_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
dest_train_file <- "pml-training.csv"
dest_test_file <- "pml-testing.csv"
download.file(train_data_url, dest_train_file)
download.file(test_data_url, dest_test_file)

## load data to memory 
training <- read.csv("pml-training.csv", na.strings = c("NA", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA", ""))

## lets look at the data to decide how to clean it
dim(testing); dim(training)
names(training)

## load few librarys needed 
library(caret); library(rattle); library(rpart); library(rpart.plot)
library(randomForest); library(repmis)

## make sure we create reproducible reaserch
set.seed(12345)

## split training data to prevent overfiting
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
my_training <- training[inTrain, ]
my_testing <- training[-inTrain, ]
dim(my_training)
dim(my_testing)

## lets clean the data now, remove columns where NAs occured
my_training_2 <- my_training[, colSums(is.na(training)) == 0]

## remove first 7 columns as they have no predictive value
my_training_2 <- my_training_2[, -c(1:7)]





