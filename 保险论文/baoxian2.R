train <- read.table("train.csv", sep=",", header=TRUE)
cat.var.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   "Family_Hist_1", paste("Medical_History_", c(2:14, 16:23, 25:31, 33:41), sep=""))
cont.var.names <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
                    "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
                    "Family_Hist_5")
disc.var.names <- c("Medical_History_1", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
                    paste("Medical_Keyword_", 1:48, sep=""))
train.cat <- train[, cat.var.names]
train.cont <- train[, cont.var.names]
train.disc <- train[, disc.var.names]
train.cat <- as.data.frame(lapply(train.cat, factor))
train.res <- data.frame(Response=train$Response)
str(train.cont)
str(train.disc)
str(train.cat)
summary(train.disc)
train.dummy <- train.disc[,c(-1,-2,-3,-4)]
str(train.dummy)
train.cont <- train.cont[,c(-1,-9)]
str(train.cont)
cont<-list()
cont[[1]]<-matrix()
cont[[2]]<-data.frame()
cont[[3]]<-numeric(3)
cont1<-train.cont [,1:4]
cont2<-train.cont[,5:7]
cont3<-train.cont[,8:11]
pca11<-princomp(cont1,cor = T)
cont[[3]][1]<-1/(pca11$sdev[1]^2)
require(mice)
imp<-mice(cont2,m=2,seed=12345)
cont21<-complete(imp,action=1)
cont22<-complete(imp,action=2)
cont23<-(cont21+cont22)/2
pca12<-princomp(cont23,cor = T)
cont[[3]][2]<-1/(pca12$sdev[1]^2)
imp<-mice(cont3,m=2,seed=1234)
cont31<-complete(imp,action=1)
cont32<-complete(imp,action=2)
cont33<-(cont31+cont32)/2
pca13<-princomp(cont33,cor = T)
cont[[3]][3]<-1/(pca13$sdev[1]^2)
cont[[3]]
newcont1<-scale(cont1)
newcont2<-scale(cont23)
newcont3<-scale(cont33)
cont[[2]]<-cbind(newcont1,newcont2,newcont3)
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
dim(cat.pro)[2]
dim(cat.employ)[2]
dim(cat.ininfo)[2]
dim(cat.inhis)[2]
dim(cat.medhis)[2]
dim(cat.medinfo)[2]
dim(cat)
weight<-apply(cat,2,mean)
cat1<-scale(cat,scale = weight)
contweight<-c(rep(cont[[3]][1],4),rep(cont[[3]][2],3),rep(cont[[3]][3],4))
q<-c(rep(4,9),rep(2,4),rep(6,13),rep(7,20),rep(35,101),rep(48,48))
length(weight)
length(q)
cat11<-cat[,1:9]
cat12<-cat[,10:13]
cat13<-cat[,14:26]
cat14<-cat[,27:46]
cat15<-cat[,47:147]
cat16<-cat[,148:195]
catpca<-NULL
pca21<-prcomp(cat11)
catpca[1]<-pca21$sdev[1]^2
pca22<-prcomp(cat12)
catpca[2]<-pca22$sdev[1]^2
pca23<-prcomp(cat13)
catpca[3]<-pca23$sdev[1]^2
pca24<-prcomp(cat14)
catpca[4]<-pca24$sdev[1]^2
pca25<-prcomp(cat15)
catpca[5]<-pca25$sdev[1]^2
pca26<-prcomp(cat16)
catpca[6]<-pca26$sdev[1]^2
catpca
lamda<-c(rep(catpca[1],9),rep(catpca[2],4),rep(catpca[3],13),rep(catpca[4],20),rep(catpca[5],101),rep(catpca[6],48))
catweight<-weight/(q*lamda)
train<-as.matrix(cbind(cont[[2]],cat1))
coeff<-c(contweight,catweight)
pool<-t(t(train)*coeff)
trainpool<-pool[1:50000,]
poolpca1<-prcomp(trainpool)
screeplot(poolpca1,npcs = 100,type="line")
simple1<-poolpca1$x[,1:15]
require(lle)
calc_k( trainpool[1:1000,], 15, 1, 50, FALSE, TRUE, 8 )
simple2 <- lle( X=trainpool[1:10000,], m=15, k=40, reg=2, ss=FALSE, id=TRUE, v=0.9 ) 
llesimple<-simple2
simple2<-llesimple$Y
subsimple1<-simple1[1:10000,]
data1<-cbind(subsimple1,train.res[1:10000,])
data2<-cbind(simple2,train.res[1:10000,])
simple3 <- lle( X=trainpool[1:10000,], m=6, k=40, reg=2, ss=FALSE, id=TRUE, v=0.9 ) 
