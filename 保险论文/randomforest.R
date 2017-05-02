
require(randomForest)
m<-randomForest(x=sub1[1:30000,-114],y=sub1[1:30000,114])
p<-predict(m,sub1[1:30000,-114],type="response")
CrossTable(sub1[1:30000,114],p)
#70.6%
p<-predict(m,sub1[30001:40000,-114],type="response")
CrossTable(sub1[30001:40000,114],p)
#71.26