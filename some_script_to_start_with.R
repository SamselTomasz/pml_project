## zaczynamy od pobrania plikow

train_data_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_data_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
dest_train_file <- "pml-training.csv"
dest_test_file <- "pml-testing.csv"
download.file(train_data_url, dest_train_file)
download.file(test_data_url, dest_test_file)

## zaladujmy dane do pamieci 
training <- read.csv("pml-training.csv", na.strings = c("NA", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA", ""))

## zerknijmy jak maja sie nasze dane, krotkie zerkniecie 
dim(testing); dim(training)

