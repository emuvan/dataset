library(ggplot2)
library(magrittr)

#the transfusion dataset
data1<-transfusion
data1$output <- as.factor(ifelse(data1$output == 1, "Yes", "No"))

#monetary vs Time
data1 %>%
  ggplot(aes(x=Monetary,y=Time, color=output))+ 
  geom_point()

#recency vs Frequency
data1 %>%
  ggplot(aes(x=Recency,y=Frequency,color=output))+
  geom_point()

#overview of the transfusion
summary(data1)

#refined summary transfusion
summary(data1[c("Monetary","Time")])

#normalize function
normalize <- function(x){
  num <- min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}

#normalize the transfusion deatasets
trans_normal <- as.data.frame(lapply(data1[1:4],normalize))

#summary transfusion "trans_normal"
summary(trans_normal)

set.seed(1234)
ind <- sample(2,nrow(data1),replace=TRUE,prob=c(0.67,0.33))

#compose training set
trans.training <- data1[ind==1, 1:4]

#inspect training set
head(trans.training)

#Compose test set
trans.test <- data1[ind==2,1:4]

#Inspect test set
head(trans.test)

#compose 'trans' training labels
trans.trainLabels <- data1[ind==1,5]

#inspect result
print(trans.trainLabels)

#compose 'iris test labels
trans.testLabels <- data1[ind==2,5]

#inspect result
print(trans.testLabels)

library(caret)
library(lattice)
library(class)
library(ISLR)
#build model
trans_pred <- knn(train = trans.training, test = trans.test,cl = trans.trainLabels$output,k=3)

#Inspect 'trans_pred'
trans_pred
#Put 'trans.testLabels' in a data frame
trans.testLabels <- data.frame(trans.testLabels)
#merge 'trans_pred' and 'trans.testLabels'
merge <- data.frame(trans_pred,trans.testLabels)

#specify column names for 'merge'
names(merge) <- c("predicted out", "Observed out")

#Inspect 'merge'
merge

#create index to split based on labels
index <- createDataPartition(data1$output, p=0.75, list = FALSE)

#Subset training set with index
trans.training <- data1[index,]
#Subset test set with index
trans.test <- data1[-index,]

prop.table(table(data1$output)) * 100
#Overview of algorithim supported by caret
names(getModelInfo())

# Start the clock!
ptm <- proc.time()

#Train a model
model_knn <- train(trans.training[, 1:4], trans.training$output, method = 'knn')

#predict the label of the test set
predictions<-predict.train(object=model_knn,trans.test[,1:4],type="raw")
# Stop the clock
proc.time() - ptm
head(predictions)
#evaluate the prediction
table(predictions)

#Confusion matrix
confusionMatrix(predictions, trans.test$output)


# Start the clock!
ptm <- proc.time()

#Train the model with preprocessing
model_knn <- train(trans.training[, 1:4], trans.training$output, method = 'knn',preProcess =c("center","scale"))

#Predict values
predictions<-predict.train(object=model_knn,trans.test[,1:4],type="raw")

# Stop the clock
proc.time() - ptm

#confusion matrix
confusionMatrix(predictions,trans.test$output)

