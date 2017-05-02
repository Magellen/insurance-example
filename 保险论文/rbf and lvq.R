train.newres<-train.res
train.newres[train.newres==3|train.newres==4|train.newres==5|train.newres==7]=0
train.newres[train.newres==1|train.newres==2]=1
train.newres[train.newres==6]=2
train.newres[train.newres==8]=3
whole2<-cbind(cont[[2]],cat,train.dummy,train.newres)
whole1<-cbind(cont[[2]],train.cat,train.dummy,train.newres)
sub1<-whole1[whole1$Response!=0,]
sub2<-whole2[whole2$Response!=0,]
sub1$Response<-as.factor(sub1$Response)
sub2$Response<-as.factor(sub2$Response)
#RSNNS
require(RSNNS)
#irisValues <- iris[,1:4] 
#irisTargets <- decodeClassLabels(iris[,5]) 
insvalue<-sub2[,-255]
instarget<-decodeClassLabels(sub2[,255])
#iris <- splitForTrainingAndTest(irisValues, irisTargets, ratio=0.15)
#iris <- normTrainingAndTestSet(iris)
ins<-splitForTrainingAndTest(insvalue, instarget, ratio=0.25)
#ins<-normTrainingAndTestSet(ins)
model <- mlp(ins$inputsTrain, ins$targetsTrain, size=10, learnFuncParams=c(0.1), maxit=5000, inputsTest=ins$inputsTest, targetsTest=ins$targetsTest)
summary(model)
weightMatrix(model)
extractNetInfo(model)
# 5 node 0.55
plotIterativeError(model)
predictions <- predict(model,ins$inputsTest)
#confusionMatrix(iris$targetsTrain,fitted.values(model)) 
#confusionMatrix(iris$targetsTest,predictions)
confusionMatrix(ins$targetsTest,predictions)#69.7% 10 node
ltrain<-sub2[1:1000,-255]
ltarget<-sub2[1:1000,255]
ltest<-sub2[30001:40000,-255]
ltarget2<-sub2[30001:40000,255]
ltrain<-scale(ltrain)
ltest<-scale(ltest)
model <- rbfDDA(ins$inputsTrain, ins$targetsTrain)
p<-predict(model,ins$inputsTest)
confusionMatrix(ins$targetsTest,p)
#example
data(iris)
iris <- iris[sample(1:nrow(iris),length(1:nrow(iris))),1:ncol(iris)] 
irisValues <- iris[,1:4] 
irisTargets <- decodeClassLabels(iris[,5])
iris <- splitForTrainingAndTest(irisValues, irisTargets, ratio=0.15)
iris <- normTrainingAndTestSet(iris)
model<- rbfDDA(iris$inputsTrain, iris$targetsTrain)
summary(model) 
plotIterativeError(model)
p<-predict(model,iris$inputsTest)
confusionMatrix(iris$targetsTest,p)
