library(glmnet)
dim(sub2)
model<-glmnet(sub2[1:30000,1:254],sub2[1:30000,255],family="multinomial")
pre<-predict(model,newx = sub2[30001:40000,1:254],type="response",s=0.01)
sub2[30001:40000,255]
pre1<-numeric(10000)
for (i in 1:10000)
{
  pre1[i]<-which.max(pre[i,1:3,1])-1
}
CrossTable(sub2[30001:40000,255],pre1)
