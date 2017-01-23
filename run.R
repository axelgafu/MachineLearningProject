#install.packages("e1071")

library(caret)

DATA_RAW_TRAIN <- "data\\train.csv"
DATA_RAW_TEST <- "data\\test.csv"
J48_MODEL <- "model\\j48_train.rds"

isInRemoveSet <- function(x)
{
  grepl( "*kurtosis*|*skewness*|*stddev*|var_*|avg_*|amplitude_*|X|max_*|min_*", x)
}

reduceDimensions <- function(x, reduceDimFunction)
{
  x_colnames <- colnames(x)
  x_reducedColNames <- x_colnames[!reduceDimFunction(x_colnames)]
  
  x[, x_reducedColNames]
}

if(!file.exists(DATA_RAW_TRAIN))
{
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", DATA_RAW_TRAIN)
}

if(!file.exists(DATA_RAW_TEST))
{
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", DATA_RAW_TEST)
}

train <- read.csv( DATA_RAW_TRAIN )
train_reduced <- reduceDimensions(train, isInRemoveSet)

if(file.exists(J48_MODEL))
{
  j48_train <- train(classe~., method="J48", data=train_reduced)
} else
{
  j48_train <- readRDS( J48_MODEL )
}

test <- read.csv( DATA_RAW_TEST )
test_reduced <- reduceDimensions(test, isInRemoveSet)

test_pred <- predict( j48_train, test_reduced)
test_pred