#########################################################################################
##Random Forest


train<-read.csv("C://vc//data_set_moedas//data_process//train.csv", header=TRUE)
test<-read.csv("C://vc//data_set_moedas//data_process//test.csv", header=TRUE)
train$label<-as.factor(train$label)
test$label<-as.factor(test$label)

library(randomForest)
train.rf<-randomForest(label~.,train)
table(test$label,predict(train.rf,newdata=test[,-1]))
   

sum(diag(table(test$label,predict(train.rf,newdata=test[,-1]))))/nrow(test)




#########################################################################################
##Tree Learning

train<-read.csv("C://vc//data_set_moedas//data_process//old//train.csv", header=TRUE)
test<-read.csv("C://vc//data_set_moedas//data_process//old//test.csv", header=TRUE)

library(xgboost)
library(Matrix)

train.mx<-sparse.model.matrix(label~., train)
test.mx<-sparse.model.matrix(label~., test)

dtrain<-xgb.DMatrix(train.mx, label=train$label)
dtest<-xgb.DMatrix(test.mx, label=test$label)

train.gdbt<-xgb.train(params=list(objective="multi:softmax", num_class=51, eval_metric="mlogloss", 
eta=0.3, max_depth=5, subsample=1, colsample_bytree=0.5), data=dtrain, nrounds=70, watchlist=list(train=dtrain,test=dtest))

table(test$label,predict(train.gdbt,newdata=dtest))
   

sum(diag(table(test$label,predict(train.gdbt,newdata=dtest))))/nrow(test)



