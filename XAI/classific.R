data = read.csv("~/Documents/covid/benchmark_paper/paper/old/supplementary materials/Supplementary Material S1.csv")

colnames(data)
####train a model with several classifiers
library(caret)
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#lasso as.factor(MOF)~Myeloperoxidase..MPO.+VCAM.1+IL.1b+VEGF.C+IL.17A+GM.CSF+ X.ANG.2.00+CCL2+IL.12.p70+Granzyme.B+sGP130,
#boruta as.factor(MOF)~PD.L1.B7.H1+IL.15+IL.6.2nd.gen+VCAM.1+IL.1ra+IL.1b+IL.10+CCL2,
#ig as.factor(MOF)~PD.L1.B7.H1+Granzyme.B+IL.15+ICAM.1+IL.1ra,


##CART
set.seed(7)
fit.cart1 <- train(as.factor(MOF)~PD.L1.B7.H1+Granzyme.B+IL.15+ICAM.1+IL.1ra,
                     data=data, method="rpart", metric=metric, trControl=control)
##KNN
set.seed(7)
fit.knn <- train(as.factor(MOF)~PD.L1.B7.H1+IL.15+IL.6.2nd.gen+VCAM.1+IL.1ra+IL.1b+IL.10+CCL2, 
                 data=data, method="knn", metric=metric, trControl=control)

##SVM
set.seed(7)
fit.svm <- train(as.factor(MOF)~PD.L1.B7.H1+Granzyme.B+IL.15+ICAM.1+IL.1ra,
                 data=data, method="svmRadial", metric=metric, trControl=control)

##RF
set.seed(7)
fit.rf <- train(as.factor(MOF)~PD.L1.B7.H1+IL.15+IL.6.2nd.gen+VCAM.1+IL.1ra+IL.1b+IL.10+CCL2,
                data=data, method="rf", metric=metric, trControl=control)








