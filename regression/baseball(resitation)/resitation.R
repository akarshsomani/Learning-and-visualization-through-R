NBA=read.csv("C:/Users/Akars/Documents/R project/regression/baseball(resitation)/NBA_train.csv")
str(NBA)
table(NBA$W,NBA$Playoffs)
NBA$PTSdiff= NBA$PTS- NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
winsReg=lm(W~PTSdiff, data=NBA)
summary(winsReg)
#W=41+0.0326*ptsDiff>=42(42 assumed by seeing the table)
#ptsdiff=3.67

pointsReg=lm(PTS~X2PA + X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK,data=NBA)
summary(pointsReg)

pointsReg$residuals
SSE=sum(pointsReg$residuals^2)
SSE
RMSE=sqrt(SSE/nrow(NBA))#root mean square error
RMSE
mean(NBA$PTS)

pointsReg2=lm(PTS~X2PA + X3PA+FTA+AST+ORB+DRB+STL+BLK,data=NBA)#without turnover as it was nopt significant
summary(pointsReg2)

pointsReg3=lm(PTS~X2PA + X3PA+FTA+AST+ORB+STL+BLK,data=NBA)#without DRB as it was nopt significant
summary(pointsReg3)

pointsReg4=lm(PTS~X2PA + X3PA+FTA+AST+ORB+STL,data=NBA)#without BLK as it was nopt significant
summary(pointsReg4)

SSE_4=sum(pointsReg4$residuals^2)
SSE_4
RMSE_4=sqrt(SSE_4/nrow(NBA))#root mean square error
RMSE
#hence it is a simpler and Better Model

NBA_test=read.csv("C:/Users/Akars/Documents/R project/regression/baseball(resitation)/NBA_test.csv")
pointsPrediction= predict(pointsReg4, newdata = NBA_test)

SSE=sum((pointsPrediction-NBA_test$PTS)^2)
SST=sum((mean(NBA$PTS)-NBA_test$PTS)^2)
R2=1-(SSE/SST)
R2
RMSE=sqrt(SSE/nrow(NBA_test))
RMSE
