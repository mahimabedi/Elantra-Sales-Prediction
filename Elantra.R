str(elantra)

# spliting data into training and test
train<-subset(elantra,Year<=2012)
test<-subset(elantra,Year>2012)

# linear reg model with all variables
M1=lm(ElantraSales~.,data=train)
summary(M1)

#converting month into factor
train$MonthFactor<-as.factor(train$Month)
test$MonthFactor<-as.factor(test$Month)

# linear reg model with factor months
M2=lm(ElantraSales~Unemployment+Queries+CPI_energy+CPI_all+MonthFactor,data=train)
summary(M2)

# checking correlation in variables; as CPI_energy has a positive coefficient
cor(train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

# linear reg model removing queries as most insignificant and high corelation
M3=lm(ElantraSales~Unemployment+CPI_energy+CPI_all+MonthFactor,data=train)
summary(M3)

# predictions on test
test$pred<-predict(M3,newdata=test)

# evaluating predictions
SSE=sum((test$pred-test$ElantraSales)^2)
SST=sum((mean(train$ElantraSales)-test$ElantraSales)^2)
Rsq<-1-(SSE/SST)
Rsq

# seasonal sales prediction
plot(test$Month,test$pred,pch=19,col="blue",ylab="Sales Prediction",xlab="Month of the year",main="Seasonal Sales Prediction")

