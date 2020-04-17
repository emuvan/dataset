library(ggplot2)
library(magrittr)


data1<-wine1

data1 %>%ggplot(aes(x=Alcohol,y=Ash,color=output))+geom_point()

summary(data1)

summary(data1[c("Alcohol","Ash")])

library(class)

normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}

data_norm <- as.data.frame(lapply(data1[1:14],normalize))

summary(data_norm)


set.seed(1234)
ind <- sample(2,nrow(data1),replace=TRUE,prob=c(0.67,0.33))

data_train <- data1[ind==1,2:13]

head(data_train)

data_test <- data1[ind==2,2:13]

head(data_test)

data.trainLabels <- data1[ind==1,1]

print(data.trainLabels)

data.testLabel <- data1[ind==2,1]

print(data.testLabel)

data_pred <-knn(train = data_train,test = data_test,cl = data.trainLabels$Alcohol,k=3)

data_pred

data.testLabel <- data.frame(data.testLabel)

merge <- data.frame(data_pred,data.testLabel)

names(merge) <-c("predicted data","Observed data")

merge

library(caret)

index <- createDataPartition(data1$Alcohol, p=0.75, list=FALSE)

data.train <- data1[index,]

data.test <- data1[-index,]

names(getModelInfo())

library(class)
model_knn <- train(data.train[, 2:14], data.train$Alcohol, method='knn')

predictions<- predict.train(object = model_knn,data.test[,2:14],type = "raw")
table(predictions)

confusionMatrix(predictions, data.test$output)

model_knn <- train(data.train[,2:14], data.train$output, method='knn', preProcess =c("center","scale"))
predictions2<- predict.train(object = model_knn,data.test[,2:14],type = "raw")

confusionMatrix(predictions2, data.test$output)


library(caret)
library(e1071)

boot_train <- trainControl(method="boot",number = 100)

data1$Alcohol = factor(data1$Alcohol)

boot_model <- train(Alcohol~., data =data1, trControl=boot_train,method="nb")

print(boot_model)

train_model2 <- trainControl(method = "cv",number = 10)
grid <- expand.grid(.fL=c(0),.usekernel=c(FALSE))
model <- train(Alcohol~., data = data1, trControl=train_model2,method="nb",tuneGrid=grid)

train_model3 <- trainControl(method = "repeatedcv", number=10,repeats=3)
model2 <-train(Alcohol~., data = data1, trControl=train_model3,method="nb")
print(model2)

train_model4 <- trainControl(method="LOOCV")
model3 <-train(Alcohol~., data = data1, trControl=train_model4,method="nb")
print(model3)

set.seed(7)
library(mlbench)
correlationMatrix <-cor(data1[,2:14])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix,cutoff=0.5)
print(highlyCorrelated)


set.seed(7)
library(mlbench)
library(caret)
control <- trainControl(method = "repeatedcv", number=10,repeats=3)
data_norm$Alcohol = as.factor(data1$Alcohol)
model <- train(Alcohol~., data = data_norm,method="lvq",preProcess="scale",trControl=control)
importance <- varImp(model,scale=FALSE)
print(importance)
plot(importance)

set.seed(7)
library(mlbench)
library(caret)
control2 <-rfeControl(functions=rfFuncs, method="cv",number=10)
results <- rfe(data_norm[,1:13],data_norm[,14],sizes=c(1:13),rfeControl=control2)
print(results)                      
predictors(results)
plot(results,type=c("g","o"))

library(mlbench)
library(caret)
library(caretEnsemble)
control3 <-trainControl(method = "repeatedcv", number=10,repeats=3)
seed <-7
metric <- "Accuracy"

set.seed(seed)
fit.treebag <- train(Alcohol~., data = data_norm,method="treebag",metric=metric, trControl=control3)
fit.rf <- train(Alcohol~., data = data_norm,method="rf",metric=metric,trControl=control3)
bagging_results <- resamples(list(treebag=fit.treebag,rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)

control4 <-trainControl(method = "repeatedcv", number=10,repeats=3,
                        index = createFolds(data_norm$Alcohol, 10),
                        savePredictions=TRUE,
                        classProbs=TRUE)
algorithmList <- c('lda','rpart','glm','knn','svmRadial')
set.seed(seed)
data_norm$Alcohol= make.names(data_norm$Alcohol,unique=FALSE,allow_=TRUE)
#revalue(data_norm$Alcohol, c("1"="One", "2"="Two", "3"="Three"))
model4 <- caretList(Alcohol~.,
                    data=data_norm,
                    trControl=control4,
                    methodList=algorithmList)

stackControl <- trainControl(method="repeatedcv",number = 10,repeats=3,savePredictions=TRUE,classProbs=TRUE)
set.seed(seed)
stack.glm <- caretStack(model4="glm",metric="Accuracy",trControl=stackControl)
