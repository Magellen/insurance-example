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
cat.var.names <- c(paste("Product_Info_", c(1,5:7), sep=""), paste("Employment_Info_", c(3,5), sep=""),
                   paste("InsuredInfo_", c(1:2,4:7), sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   paste("Medical_History_", c(3:9,11:14, 16:23, 25:31, 33:41), sep=""))
train.cat <- train[, cat.var.names]
train.cat <- as.data.frame(lapply(train.cat, factor))
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
require(C50)
m<-C5.0(x=sub1[1:30000,-114],y=sub1[1:30000,114],trials = 100)
#74.1%
p<-predict(m,sub1[30001:40000,-114],type="class")
require(gmodels)
CrossTable(sub1[30001:40000,114],p)
#70.34%

