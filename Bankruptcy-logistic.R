install.packages("corrplot")
library('corrplot')

bankrupcy.data <- read.csv("C:/Users/abhil/Downloads/bankruptcy (2).csv", header = T)
summary(bankrupcy.data$DLRSN)
hist(bankrupcy.data$DLRSN)



subset <- sample(nrow(bankrupcy.data), nrow(bankrupcy.data) * 0.8)
bankrupcy.train = bankrupcy.data[subset, ]
bankrupcy.test = bankrupcy.data[-subset, ]
str(bankrupcy.train)
corrplot(cor(bankrupcy.train),method = 'color')

bankrupcy.train$CUSIP=as.numeric(bankrupcy.train$CUSIP)
bankrupcy.test$CUSIP=as.numeric(bankrupcy.test$CUSIP)


pairs(bankrupcy.train[,-c(1,3)])
bankrupcy.glm <- glm( DLRSN ~ .-FYEAR-CUSIP, family = binomial, bankrupcy.train)
bankrupcy.glm.step=step(bankrupcy.glm,direction = 'both')#backward is default or step function
#Step:  AIC=2368.56
#DLRSN ~ R1 + R2 + R3 + R4 + R6 + R7 + R8 + R9 + R10
bankrupcy.glm.step=step(bankrupcy.glm,k=log(nrow(bankrupcy.train)),direction = 'both')
bankrupcy.glm.aicModel= glm( DLRSN ~ .-FYEAR-CUSIP-R5, family = binomial, bankrupcy.train)
summary(bankrupcy.glm.aicModel)
#Step:  AIC=2420.27
#DLRSN ~ R2 + R3 + R6 + R7 + R8 + R9 + R10
bankrupcy.glm.bicModel = glm( DLRSN ~ .-FYEAR-CUSIP-R1-R4-R5, family = binomial, bankrupcy.train)
summary(bankrupcy.glm.bicModel)
BIC(bankrupcy.glm.bicModel)
AIC(bankrupcy.glm.bicModel)
BIC(bankrupcy.glm.aicModel)
AIC(bankrupcy.glm.aicModel)


#insample AIC model
prob.aicModel.insample=predict(bankrupcy.glm.aicModel,type = 'response')
pred.aicModel.insample=predict(bankrupcy.glm.aicModel,type='response')>1/16
pred.aicModel.insample=as.numeric(pred.aicModel.insample) #vector with 0 & 1
mytable=table(bankrupcy.train$DLRSN,pred.aicModel.insample,dnn=c('True','Predicted')) #arguement function,dnn means dimension names
mytable

misrate=((mytable[1,2]+mytable[2,1])/sum(mytable[,]))
misrate

#insample bic model
prob.bicModel.insample=predict(bankrupcy.glm.bicModel,type = 'response')
pred.bicModel.insample=predict(bankrupcy.glm.bicModel,type='response')>1/16
pred.bicModel.insample=as.numeric(pred.bicModel.insample) #vector with 0 & 1
mytable12=table(bankrupcy.train$DLRSN,pred.bicModel.insample,dnn=c('True','Predicted')) #arguement function,dnn means dimension names
mytable12

list2=ifelse(credit.train$Y!= pred.glml.insample,1,0) #logical comparision
mean(list2)
misrate=((mytable12[1,2]+mytable12[2,1])/sum(mytable12[,]))
misrate

#out sample AIC model
prob.aicModel.outsample=predict(bankrupcy.glm.aicModel,bankrupcy.test,type = 'response')
pred.aicModel.outsample=predict(bankrupcy.glm.aicModel,bankrupcy.test,type='response')>1/16
pred.aicModel.outsample=as.numeric(pred.aicModel.outsample)
mytable2=table(bankrupcy.test$DLRSN,pred.aicModel.outsample,dnn=c('True','Predicted'))
mytable2
misrate2=(mytable2[1,2]+mytable2[2,1])/sum(mytable2[,])
misrate2 #symmetric cost information

#out sample BIC model
prob.bicModel.outsample=predict(bankrupcy.glm.bicModel,bankrupcy.test,type = 'response')
pred.bicModel.outsample=predict(bankrupcy.glm.bicModel,bankrupcy.test,type='response')>1/16
pred.bicModel.outsample=as.numeric(pred.bicModel.outsample)
mytable3=table(bankrupcy.test$DLRSN,pred.bicModel.outsample,dnn=c('True','Predicted'))
mytable3
misrate3=(mytable3[1,2]+mytable3[2,1])/sum(mytable3[,])
misrate3 #symmetric cost information

#ROC AIC model out sample
roc.plot(bankrupcy.train$DLRSN=='1',threshold = seq(0.1,0.9, 0.1),prob.aicModel.insample)
roc.plot(bankrupcy.train$DLRSN=='1',threshold = seq(0.1,0.9, 0.1),prob.aicModel.insample)$roc.vol


#ROC BIC model in sample
roc.plot(bankrupcy.train$DLRSN=='1',threshold = seq(0.1,0.9, 0.1),prob.bicModel.insample)
roc.plot(bankrupcy.train$DLRSN=='1',threshold = seq(0.1,0.9, 0.1),prob.bicModel.insample)$roc.vol


#ROC AIC model out sample
library(verification)
roc.plot(bankrupcy.test$DLRSN=='1',threshold = seq(0.1,0.9, 0.1),prob.aicModel.outsample)
roc.plot(bankrupcy.test$DLRSN=='1',threshold = seq(0.1,0.9, 0.1),prob.aicModel.outsample)$roc.vol


#ROC BIC model in sample
roc.plot(bankrupcy.test$DLRSN=='1',threshold = seq(0.1,0.9, 0.1),prob.bicModel.outsample)
roc.plot(bankrupcy.test$DLRSN=='1',threshold = seq(0.1,0.9, 0.1),prob.bicModel.outsample)$roc.vol




