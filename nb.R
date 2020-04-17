library(caret)
library(klaR)

data1 <- transfusion
data1$output <- as.factor(ifelse(data1$output == 1, "Yes", "No"))
#data splitting 80 percent
split=0.80
trainIndex <- createDataPartition(data1$output, p=split, list=FALSE)
data_train <- data1[ trainIndex,]
data_test <- data1[-trainIndex,]

# Start the clock!
ptm <- proc.time()

# train a naive bayes model
model <- NaiveBayes(output~., data=data_train)
model
# make predictions
x_test <- data_test[,1:4]
y_test <- data_test[,5]
predictions <- predict(model, x_test)
# Stop the clock
proc.time() - ptm
confusionMatrix(predictions$class,y_test$output)

# load the library
library(caret)
# load the transfusion dataset
data1 <- transfusion
data1$output = factor(data1$output)
# define training control
train_control <- trainControl(method="boot", number=100)
# train the model
model <- train(output~., data=data1[], trControl=train_control, method="nb")
# summarize results
print(model)

# load the library
library(caret)
# load the transfusion dataset
data1 <- transfusion
data1$output = factor(data1$output)
# define training control
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(output~., data=data1, trControl=train_control, method="nb")
# summarize results
print(model)
