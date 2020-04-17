library(ggplot2)
library(magrittr)
library(caret)

data1<-transfusion
data1$output <- as.factor(ifelse(data1$output == 1, "Yes", "No"))

#create index to split based on labels
index <- createDataPartition(data1$output, p=0.75, list = FALSE)

#Subset training set with index
training <- data1[index,]
#Subset test set with index
test <- data1[-index,]

trainx <- training[,names(training) != "output"]
preProcvalues <- preProcess(x = tranx,method = c("center","scale"))
preProcvalues 

set.seed(400)
control <- trainControl(method="repeatedcv",repeats=3)
knnfit <- train(output ~., data = training,method = "knn", trControl = control, preProcess = c("center","scale"), tuneLength = 20)

knnfit
