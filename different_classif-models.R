data = read.csv("~/Documents/covid/COVID_AI/paper/peerj/tests/cleveland_point_ddimer.csv")
data2 = read.csv("~/Documents/covid/COVID_AI/paper/peerj/tests/irish_pca_all.csv")


colnames(data)
####train a model with several classifiers
library(caret)
control <- trainControl(method="cv", number=5)
metric <- "Accuracy"


##CART
set.seed(7)
fit.cart1 <- train(as.factor(Clinical.Outcome)~ICAM.1+CXCL10+TNF.a+VCAM.1+G.CSF, data=data, method="rpart", metric=metric, trControl=control)
fit.cart2 <- train(as.factor(Outcome)~ICAM.1+CXCL10+TNF.a.2nd.gen+VCAM.1+G.CSF, data=data2, method="rpart", metric=metric, trControl=control)

##KNN
set.seed(7)
fit.knn <- train(as.factor(Clinical.Outcome)~ICAM.1+CXCL10+TNF.a+VCAM.1+G.CSF, data=data, method="knn", metric=metric, trControl=control)
fit.knn <- train(as.factor(Outcome)~ICAM.1+CXCL10+TNF.a.2nd.gen+VCAM.1+G.CSF, data=data2, method="knn", metric=metric, trControl=control)

##SVM
set.seed(7)
fit.svm <- train(as.factor(Clinical.Outcome)~ICAM.1+CXCL10+TNF.a+VCAM.1+G.CSF, data=data, method="svmRadial", metric=metric, trControl=control)
fit.svm <- train(as.factor(Outcome)~ICAM.1+CXCL10+TNF.a.2nd.gen+VCAM.1+G.CSF, data=data2, method="svmRadial", metric=metric, trControl=control)

##RF
set.seed(7)
fit.rf <- train(as.factor(Clinical.Outcome)~ICAM.1+CXCL10+TNF.a+VCAM.1+G.CSF, data=data, method="rf", metric=metric, trControl=control)
fit.rf <- train(as.factor(Outcome)~ICAM.1+CXCL10+TNF.a.2nd.gen+VCAM.1+G.CSF, data=data2, method="rf", metric=metric, trControl=control)

##summary of accuracy of each model
results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
