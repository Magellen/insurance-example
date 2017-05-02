train.newres<-train.res
train.newres[train.newres==3|train.newres==4|train.newres==5|train.newres==7]=0
train.newres[train.newres==1|train.newres==2]=1
train.newres[train.newres==6]=2
train.newres[train.newres==8]=3
whole2<-cbind(cont[[2]],cat,train.dummy,train.newres)
whole1<-cbind(cont[[2]],train.cat,train.dummy,train.newres)
sub1<-whole1[whole1$Response!=0,]
sub2<-whole2[whole2$Response!=0,]
require(softmaxreg)
softmax_model = softmaxReg(sub2[1:30000,-255], sub2[1:30000,255], hidden = c(10), maxit = 10, type = "class", algorithm = "nag", L2 = TRUE) 
summary(softmax_model)
yFitMat = softmax_model$fitted.values
yPred = predict(softmax_model, sub2[30001:40000,-255])
sum(yPred==sub2[30001:40000,255])/10000#70.53%
yPred2 = predict(softmax_model, sub2[1:30000,-255])
sum(yPred2==sub2[1:30000,255])/30000#71.63%
