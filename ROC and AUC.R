library(ggplot2)
library(magrittr)
library(ROCR)
library(ISLR)
library(caret)

data1 <- transfusion
str(data1)

#logistic regression Model show the values
library(nnet)
mymodel <- multinom(output~., data = transfusion)

#miss classification and the confusionmatrix
p <- predict(mymodel,transfusion)
tab <- table(p, transfusion$output)
tab
#giving out the miss classification
1-sum(diag(tab))/sum(tab)
#giving out the right classification
sum(diag(tab))/sum(tab)

#overall result who transmitted blood
table(transfusion$output)


#creating prediction
pred <- predict(mymodel,transfusion,type = 'prob')
pred <- prediction(pred,transfusion$output)
eval <- performance(pred,"acc")
plot(eval)
abline(h=0.79, v=0.45)

#identify the best values
max <- which.max(slot(eval, "y.values")[[1]])
acc <- slot(eval, "y.values")[[1]][max]
cut <- slot(eval, "x.values")[[1]][max]
print(c(Accuracy=acc, Cutoff = cut))

#ROC curve plot
roc <- performance(pred, "tpr","fpr")
plot(roc,
     colorize=T,
     main = "ROC Curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity")
abline(a=0, b=1)

#AUC
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(.7, .4, auc, title = "AUC", cex = 0.5)

#precision vs. recall
RP.perf <- performance(pred, "prec", "rec")
plot (RP.perf)
