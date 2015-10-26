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

## load few libraries needed 
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

##------------now first model - decision tree----------

## fit the first model: rpart
## mod_fit_1 <- rpart(classe ~ ., data=my_training_2, method="class")
mod_fit_2 <- train(classe ~ ., method="rpart", data=my_training_2)

## lets have a look how it perform
print(mod_fit_2$finalModel)
        ## nice :)
fancyRpartPlot(mod_fit_2$finalModel)

## predict and check accuracy
predictions_1 <- predict(mod_fit_2, newdata=my_testing)
confusionMatrix(predictions_1, my_testing$classe)
        ## very low accuracy, only 49%

## ------------ second model - random forests ---------

## fit the random forest model insetad, should be a bit better :)
## mod_fit_3 <- train(classe ~ ., method="rf", data=my_training_2, prox=TRUE, ntree=5)
## takes for ages to compute, over 8h... :\ missed assigment because of this..
mod_fit_3 <- randomForest(classe ~ ., data=my_training_2)
## much faster then caret package, although returns error on my machine when used
## with proximity=TRUE option. Not enought ram...

## do the prediction 
prediction_2 <- predict(mod_fit_3, newdata=my_testing)
confusionMatrix(prediction_2, my_testing$classe)
        ## excelent, 99.39% accuracy :)

## it is hard to plot random forest, the way i find usefull without doing proximity
## on the trees (which hangs my laptop dead) is this:
plot(mod_fit_3, log="y")
varImpPlot(mod_fit_3)

## predict answers for file submission
answers <- predict(mod_fit_3, newdata=testing)
answers

## --- assigment submission files ----
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(answers)
        ## worked :)



