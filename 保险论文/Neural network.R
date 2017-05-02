View(cont[[2]])
View(cat1)
str(catweight)
head(catweight)
contweight
cat.var.names <- c(paste("Product_Info_", c(1,5:7), sep=""), paste("Employment_Info_", c(3,5), sep=""),
                   paste("InsuredInfo_", c(1:2,4:7), sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   paste("Medical_History_", c(3:9,11:14, 16:23, 25:31, 33:41), sep=""))
train.cat <- train[, cat.var.names]
train.cat <- as.data.frame(lapply(train.cat, factor))
cat.pro<-train.cat[,1:4]
cat.employ<-train.cat[,5:6]
cat.ininfo<-train.cat[,7:12]
cat.inhis<-train.cat[,13:19]
cat.medhis<-train.cat[,20:54]
cat.medinfo<-train.dummy
require(dummies)
dummycat<-function(x)
{
  new<-data.frame()
  new1<-data.frame()
  length<-dim(x)[2]
  new<-dummy(x[,1])
  for(i in 2:length)
  {
    new1<-dummy(x[,i])
    new<-cbind(new,new1)
  }
  return(new)
}
cat.pro<-dummycat(cat.pro)
cat.employ<-dummycat(cat.employ)
cat.ininfo<-dummycat(cat.ininfo)
cat.inhis<-dummycat(cat.inhis)
cat.medhis<-dummycat(cat.medhis)
cat<-cbind(cat.pro,cat.employ,cat.ininfo,cat.inhis,cat.medhis,cat.medinfo)
weight<-apply(cat,2,mean)
cat1<-scale(cat,scale = weight)
View(cat)
length(catweight)
length(weight)
catweight2<-catweight/weight
str(contweight)
str(catweight2)
poolweight<-c(contweight,catweight2)
str(poolweight)
pooldata<-cbind(cont[[2]],cat)
str(pooldata)
str(train.newres)
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
model <- mlp(ins$inputsTrain, ins$targetsTrain, size=10, learnFuncParams=c(0.1), maxit=500, inputsTest=ins$inputsTest, targetsTest=ins$targetsTest)
summary(model)
weightMatrix(model)
extractNetInfo(model)
# 5 node 0.55
plotIterativeError(model)
predictions <- predict(model,ins$inputsTest)
#confusionMatrix(iris$targetsTrain,fitted.values(model)) 
#confusionMatrix(iris$targetsTest,predictions)

confusionMatrix(ins$targetsTrain,fitted.values(model)) #68.4% 10 node
confusionMatrix(ins$targetsTest,predictions)#69.8% 10 node

