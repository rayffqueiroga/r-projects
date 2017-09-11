##Deeplearning com menos filtros

library(h2o)
 
#start a local h2o cluster
local.h2o <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, nthreads=-1)

training <- read.csv("C://vc//data_set_moedas//data_process//train.csv", header=TRUE)
testing  <- read.csv("C://vc//data_set_moedas//data_process//test.csv", header=TRUE)


 
 # convert digit labels to factor for classification
training[,1]<-as.factor(training[,1])
 
# pass dataframe from inside of the R environment to the H2O instance
trData<-as.h2o(training)
tsData<-as.h2o(testing)

res.dl <- h2o.deeplearning(x = 2:785, y = 1, trData, activation = "Tanh", hidden=rep(160,5),epochs = 20)


#use model to predict testing dataset
pred.dl<-h2o.predict(object=res.dl, newdata=tsData[,-1])
pred.dl.df<-as.data.frame(pred.dl)

testing$label<-as.factor(testing$label)
table(test$label,pred.dl.df[,1])
 
