train.newres<-train.res
train.newres[train.newres==3|train.newres==4|train.newres==5|train.newres==7]=10
train.newres[train.newres==1|train.newres==2]=0
train.newres[train.newres==6]=1
train.newres[train.newres==8]=2
whole2<-cbind(cont[[2]],cat,train.dummy,train.newres)
whole1<-cbind(cont[[2]],train.cat,train.dummy,train.newres)
sub1<-whole1[whole1$Response!=10,]
sub2<-whole2[whole2$Response!=10,]
sub2<-as.matrix(sub2)
require(xgboost)
dtrain <- xgb.DMatrix(sub2[1:30000,-255], label = sub2[1:30000,255])
dtest <- xgb.DMatrix(sub2[30001:40000,-255], label = sub2[30001:40000,255]) 
watchlist <- list(eval = dtest, train = dtrain)
param <- list(subsample=0.7,eta=0.5,gamma=0.5,max_depth = 4, silent = 1, objective = "multi:softmax",num_class=3)
bst <- xgb.train(param, dtrain, nrounds = 500, watchlist)#73%
sub3<-sub2[,c(1:11,255)]
dtrain <- xgb.DMatrix(sub3[1:30000,-12], label = sub3[1:30000,12])
dtest <- xgb.DMatrix(sub3[30001:40000,-12], label = sub3[30001:40000,12]) 
watchlist <- list(eval = dtest, train = dtrain)
param <- list(subsample=0.7,eta=0.5,gamma=0.5,max_depth = 4, silent = 1, objective = "multi:softmax",num_class=3)
bst <- xgb.train(param, dtrain, nrounds = 500, watchlist)#59%
