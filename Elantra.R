str(Elantra)

# spliting data into training and test
train<-subset(Elantra,Year<=2012)
test<-subset(Elantra,Year>2012)

# linear reg model with all variables
M1=lm(ElantraSales~.,data=train)
summary(M1)
#R-squared:  0.4592,	Adjusted R-squared:  0.3473 

#converting month into factor
train$MonthFactor<-as.factor(train$Month)
levels(train$MonthFactor)<-c("Jan", "Feb", "Mar", "Apr", "May", 
                       "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")

test$MonthFactor<-as.factor(test$Month)
levels(test$MonthFactor)<-c("Jan", "Feb", "Mar", "Apr", "May", 
                       "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")

# realigning factors variables
levels(train$MonthFactor)<-levels(test$MonthFactor)

# linear reg model with factor months
M2=lm(ElantraSales~Unemployment+Queries+CPI_energy+CPI_all+MonthFactor,data=train)
summary(M2)
#R-squared:  0.8193,	Adjusted R-squared:  0.6837
test$pred<-predict(M2,newdata=test)
SSE=sum((test$pred-test$ElantraSales)^2)
SST=sum((mean(train$ElantraSales)-test$ElantraSales)^2)
Rsq<-1-(SSE/SST)
Rsq
#0.7426902

# checking correlation in variables; as CPI_energy has a positive coefficient
cor(train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])


# linear reg model removing queries as most insignificant and high corelation
M3=lm(ElantraSales~Unemployment+CPI_energy+CPI_all+MonthFactor,data=train)
summary(M3)
#R-squared:  0.818,	Adjusted R-squared:  0.6967
test$pred2<-predict(M3,newdata=test)
SSE=sum((test$pred2-test$ElantraSales)^2)
SST=sum((mean(train$ElantraSales)-test$ElantraSales)^2)
Rsq<-1-(SSE/SST)
Rsq
#0.7280232 ; model rejected

#randon forest
library(randomForest)
str(train)
mtry <- tuneRF(train[-3], train[,3],ntreeTry=100)
mtry
M4<-randomForest(ElantraSales~Unemployment+Queries+CPI_energy+CPI_all+MonthFactor,data=train,mtry=2,nodesize=5,ntree=1000,importance=TRUE)
varImpPlot(M4,main="Model 4 Variable Importance",col="blue",pch=19)
#reinforces all features important
test$pred4<-predict(M4,newdata=test)
SSE=sum((test$pred4-test$ElantraSales)^2)
SST=sum((mean(train$ElantraSales)-test$ElantraSales)^2)
Rsq<-1-(SSE/SST)
Rsq
# 0.4508474 model rejected


library(ggplot2)

# Monthly sales prediction
monthlyavg<-tapply(test$pred,test$MonthFactor,mean)
monthlyavg<-as.data.frame(monthlyavg)
monthlyavg$month<-c("Jan", "Feb", "Mar", "Apr", "May", 
                    "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
monthlyavg$DataType<-"Test"
data1<-monthlyavg
data1

monthlyavg<-tapply(train$ElantraSales,train$MonthFactor,mean)
monthlyavg<-as.data.frame(monthlyavg)
monthlyavg$month<-c("Jan", "Feb", "Mar", "Apr", "May", 
                    "Jun", "Jul","Aug","Sep","Oct","Nov","Dec")
monthlyavg$DataType<-"Train"
data2<-monthlyavg
data2

data=rbind(data1,data2)
data

ggplot(data,aes(x=month,y=monthlyavg,group=DataType))+
  geom_line(aes(colour=DataType,size=8))+
  ylab("Sales") + 
  xlab("Month of the Year") +
  ggtitle("Average Monthly Sales")