## Deeplearning com vários filtros

train<-read.csv("C://vc//data_set_moedas//data_process//train.csv", header=TRUE)
test<-read.csv("C://vc//data_set_moedas//data_process//test.csv", header=TRUE)

train$label<-as.factor(train$label)
test$label<-as.factor(test$label)

library(h2o)
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, nthreads=3)
trData<-as.h2o(train)
tsData<-as.h2o(test)

res.dl <- h2o.deeplearning(y = 1, training_frame = trData, 
			activation = "RectifierWithDropout",hidden=c(1024,1024,2048),epochs = 300, 
			adaptive_rate = FALSE, rate=0.01, rate_annealing = 1.0e-6,rate_decay = 1.0, 
			momentum_start = 0.5,momentum_ramp = 5000*18, momentum_stable = 0.99, input_dropout_ratio = 0.2,
			l1 = 1.0e-5,l2 = 0.0,max_w2 = 15.0, initial_weight_distribution = "Normal",initial_weight_scale = 0.01,
			nesterov_accelerated_gradient = T, loss = "CrossEntropy", fast_mode = T, diagnostics = T, ignore_const_cols = T,force_load_balance = T)

pred<-h2o.predict(res.dl,tsData[,-1])
pred.df<-as.data.frame(pred)
table(test$label,pred.df[,1])

sum(diag(table(test$label,pred.df[,1])))/nrow(test)
