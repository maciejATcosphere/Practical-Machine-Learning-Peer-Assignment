
# FIXME -> TEMP
setwd('/home/maciej/learning/Coursera/Machine-Learning/Assignment/')

library(caret)

data <- read.csv('data/training.csv')
submission_data <- read.csv('data/testing.csv')
    
# exclude X column
data <- data[,-c(1)]

# first split data to testing and training
inTrain <- createDataPartition(y=data$classe, p=0.6, list=FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]

# select only the columns for which we have enough data 
# 0.03 was infered from experimentation
not.na <- apply(training, 2 , function (col) { 1 - sum(is.na(col)) / nrow(training) }) > 0.03
training <- training[, not.na]

# check if there are any near zero vars
nzv <- nearZeroVar(training, saveMetrics=TRUE)
exclude_nzv <- rownames(subset(nzv, nzv == TRUE))
training <- training[ , -which(names(training) %in% exclude_nzv)]

# FIXME: maybe I could try pca to get rid of some of the variables?

# FIXME: try to select more reliable feature by perfoming some extra exploratory 
# data analysis 
# 1. Try to get rid of correlated variables
# 2. Try to understand impact of some of the variables on the final classification
# FIXME: try to find much more lightwieght predictor than random forest!

# random forest
# decision tree did very bad with accuracy of about 60%
modFit <- train(classe ~ ., data=training, method='rf')

# evaluation
predictions <- predict(modFit, newdata=testing)
confusionMatrix(predictions, testing$classe)

# generate submission code
# answers = rep("A", 20)
answers <- predict(modFit, newdata=submission_data)
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("./answers/problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
pml_write_files(answers)

