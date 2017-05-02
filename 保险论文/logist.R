View(cont[[2]])
View(cat)
str(cat)
require(randomForest)
pooldata<-cbind(cont[[2]],cat)
train.newres<-train.res
train.newres[train.newres==1|train.newres==2|train.newres==3|train.newres==4]=1
train.newres[train.newres==5|train.newres==6|train.newres==7]=2
train.newres[train.newres==8]=3
train.newres<-as.data.frame(train.newres)
train.newres<-as.factor(train.newres$Response)
train.newres<-as.data.frame(train.newres)
m<-randomForest(x=pooldata[1:50000,],y=train.newres[1:50000,],importance = TRUE)

