data1<-cbind(simple1,train.res[1:10000,])
#C5
require(C50)#0.54
m<-C5.0(data1[1:10000,-16],data1[1:10000,16])
data1<-as.data.frame(data1)
data1$V16<-as.factor(data1$V16)
str(data1)
summary(m)
c5predict<-predict(m,data1[10001:15000,-16])
require(gmodels)
CrossTable(data1[10001:15000,7],c5predict)
str(c5predict)
sum(c5predict==data1[10001:15000,16])/5000
data1<-data1[,c(1:6,16)]
View(data1)
m<-C5.0(data1[1:10000,-7],data1[1:10000,7],trials = 10)
c5predict<-predict(m,data1[10001:15000,-7])
sum(c5predict==data1[10001:15000,7])/5000
table(train.res)
train.newres<-train.res
train.newres[train.newres==1|train.newres==2|train.newres==3|train.newres==4]=1
train.newres[train.newres==5|train.newres==6|train.newres==7]=2
train.newres[train.newres==8]=3
View(train.newres)
data1<-cbind(simple1[,1:6],train.newres[1:50000,])
data1<-as.data.frame(data1)
data1$V7<-as.factor(data1$V7)
m<-C5.0(data1[1:10000,-7],data1[1:10000,7],trials = 10)
c5predict<-predict(m,data1[10001:15000,-7])
CrossTable(data1[10001:15000,7],c5predict)
sum(c5predict==data1[10001:15000,7])/5000
c5predict0<-predict(m,data1[1:10000,-7])
sum(c5predict0==data1[1:10000,7])/10000
#NN
require(neuralnet)
m<-neuralnet(V7~PC1+PC2+PC3+PC4+PC5+PC6,data = data1,hidden = 5)
require(nnet)#0.74
m <- nnet(data1[1:10000,-7], data1[1:10000,7], size = 5, decay = 5e-4, maxit = 200) 
sum(predict(m,data1[10001:15000,-7])==data1[1:10000,7])/5000
#