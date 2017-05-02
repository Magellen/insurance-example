#data reduction pca
pca<-prcomp(sub2[,-255])
screeplot(pca,type="l",npcs = 100)
sub3<-pca$x[,1:10]

sub3<-cbind(sub3,Response=sub2[,255])
sub3<-as.data.frame(sub3)
sub3$Response<-as.factor(sub3$Response)
View(sub3)
#c5
require(C50)
m<-C5.0(x=sub3[1:30000,-11],y=sub3[1:30000,11],trials = 100)
#74.1%
p<-predict(m,sub3[30001:40000,-11],type="class")
require(gmodels)
CrossTable(sub3[30001:40000,11],p)#61.9%
#randomforest
require(randomForest)
m<-randomForest(x=sub3[1:30000,-11],y=sub3[1:30000,11])

p<-predict(m,sub3[30001:40000,-11],type="response")
CrossTable(sub3[30001:40000,11],p)
#62.9%
#xgboost
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
pca<-prcomp(sub2[,-255])
screeplot(pca,type="l",npcs = 100)
sub3<-pca$x[,1:10]

sub3<-cbind(sub3,Response=sub2[,255])
require(xgboost)
sub3<-as.matrix(sub3)
dtrain <- xgb.DMatrix(sub3[1:30000,-11], label = sub3[1:30000,11])
dtest <- xgb.DMatrix(sub3[30001:40000,-11], label = sub3[30001:40000,11]) 
watchlist <- list(eval = dtest, train = dtrain)
param <- list(subsample=0.7,eta=0.5,gamma=0.5,max_depth = 4, silent = 1, objective = "multi:softmax",num_class=3)
bst <- xgb.train(param, dtrain, nrounds = 500, watchlist)#73%
#NN
require(RSNNS)
#irisValues <- iris[,1:4] 
#irisTargets <- decodeClassLabels(iris[,5]) 
insvalue<-sub3[,-11]
instarget<-decodeClassLabels(sub3[,11])
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
model <- rbfDDA(ins$inputsTrain, ins$targetsTrain)
p<-predict(model,ins$inputsTest)
confusionMatrix(ins$targetsTest,p)
#softmax
pca<-prcomp(sub2[,-255])
screeplot(pca,type="l",npcs = 100)
sub3<-pca$x[,1:10]

sub3<-cbind(sub3,Response=sub2[,255])
sub3<-as.data.frame(sub3)
sub3$Response<-as.factor(sub3$Response)
View(sub3)
require(softmaxreg)
softmax_model = softmaxReg(sub3[1:30000,-11], sub3[1:30000,11], hidden = c(10), maxit = 1, type = "class", algorithm = "nag", L2 = TRUE) 
summary(softmax_model)
yFitMat = softmax_model$fitted.values
yPred = predict(softmax_model, sub3[30001:40000,-11])
sum(yPred==sub2[30001:40000,255])/10000#70.53%
