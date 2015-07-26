#####
# Load the needed libraries.
#####
library(randomForest)
#
library(data.table)
#
library(caret)
#
library(xtable)
#
library(doMC)
#
library(knitr)
#
library(ggplot2)
#
#
registerDoMC(cores = 7)
#####
# Download testing and training files.
#####
download.pml <- function() {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv")
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
    
}
#
#####
# Read the file.
#####
read.pml <- function(file) {
    fread(file, na.strings=c("#DIV/0!", ""))
}
#
#####
# Generate the report.
#####
build.report <- function() {
    knit2html("myproject.Rmd", "myindex.html")
}
#
#####
# Read the training file and do validation.
#####
#
raw.validation <- read.pml("pml-testing.csv")
#
raw.train <- read.pml("pml-training.csv")
#
#####
# Set the seed value
#####
set.seed(9)
#
#####
# Process the NA values
#####
na.cols <- raw.train[, sapply(.SD, function(x) any(is.na(x))) ]
#
#####
# Use only necessary information.
#####
drop.columns <- function(x) {
    x[,!c("V1", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window"),with=F]
}
##### 
# Do transformation of the features.
#####
transform.features <- function(x) {
    x[,classe:=factor(classe)]
}
#
#####
# Make sure only columns with values are processed.
#####
training.features <- drop.columns(raw.train[,eval(names(which(na.cols == F))),with=F])
#
#####
# Do the prediction processing.
#####
write.pml.predictions <- function(x) {
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
#
#####
# Submit the prediction.
#####
submit.prediction <- function(x, validation) {
    # 
    in.train <- createDataPartition(x$classe, p=.60, list=FALSE)
    #
    train <- x[in.train[,1]]
    #
    model.rf <- train(y=as.factor(train$classe), x=train[,!"classe",with=F], tuneGrid=data.frame(mtry=3), trControl=trainControl(method="none"), method="parRF")  
    #
    write.pml.predictions(predict(model.rf, newdata=drop.columns(validation[,eval(names(which(na.cols == F))[-60]),with=F])))
}
