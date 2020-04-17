library(mlbench)
library(caret)
library(caretEnsemble)

g <- rnorm(100000)
h <- rep(NA, 100000)

#the transfusion dataset
data1<-transfusion
data1$output <- as.factor(ifelse(data1$output == 1, "Yes", "No"))



# Start the clock!
ptm <- proc.time()

# Loop through the vector, adding one
for (i in 1:100000){
  h[i] <- g[i] + 1
}
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

# Stop the clock
proc.time() - ptm
summary(bagging_results)
dotplot(bagging_results)

#example stacking
#create submodels
control2 <- trainControl(method="repeatedcv",number=10,repeats=3,savePredictions=TRUE,classProbs=TRUE)
algorithimList <-c('lda','rpart','glm','knn','svmRadial')
set.seed(7)
models <- caretList(output~.,data=data1,trControl=control2,methodList=algorithimList)
results<-resamples(models)
summary(models)
summary(results)
dotplot(results)

#correlation between results
modelCor(results)
splom(results)

# Start the clock!
ptm <- proc.time()

#stack using treebag
set.seed(7)
stacktreebag <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE,
classProbs=TRUE)
stack.tb <- caretStack(models, method="treebag", metric="Accuracy", trControl=stacktreebag)
print(stack.tb)

# Stop the clock
proc.time() - ptm

#stack using knn
stackknn <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE,
classProbs=TRUE)
set.seed(7)
stack.knn <- caretStack(models, method="knn", metric="Accuracy", trControl=stackknn)
print(stack.knn)

# Stop the clock
proc.time() - ptm

# Start the clock!
ptm <- proc.time()

#stack using nb
set.seed(7)
stacknb <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE,
classProbs=TRUE)
stack.nb <- caretStack(models, method="nb", metric="Accuracy", trControl=stacknb)
print(stack.nb)

# Stop the clock
proc.time() - ptm

