library(mlbench)
library(caret)
library(caretEnsemble)

#the transfusion dataset
data1<-transfusion
data1$output <- as.factor(ifelse(data1$output == 1, "Yes", "No"))

#bagging
control <- trainControl(method="repeatedcv",number=10,repeats=3)
seed <- 7
metric <- "Accuracy"
#bagged cart
set.seed(seed)
fit.treebag <- train(output~.,data=data1,method="treebag",metric=metric,trControl=control)

#random forest
fit.rf <-train(output ~.,data=data1,method="rf",metric=metric,trControl=control)
bagging_results <-resamples(list(treebag=fit.treebag,rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)

#stacking
control2 <- trainControl(method="repeatedcv",number=10,repeats=3,savePredictions=TRUE,classProbs=TRUE)
algorithimList <-c('lda','rpart','glm','knn','svmRadial')
set.seed(seed)
Models <- caretList(output~.,data=data1,trControl=control,methodList=algorithimList)
results<-resamples(Models)
summary(Models)
summary(results)
dotplot(results)

