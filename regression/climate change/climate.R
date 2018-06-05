#Creating Our First Model 

climate=read.csv("C:/Users/Akars/Documents/R project/regression/climate change/climate_change.csv")
summary(climate)
climate_train=subset(climate, Year<=2006 )

model1=lm(data=climate_train, Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols)
#Enter the model R2 (the "Multiple R-squared" value):
#Which variables are significant in the model? We will consider a variable signficant only if the p-value is below 0.05.
summary(model1)

#Understanding the Model 

#however, the regression coefficients of both the N2O and CFC-11 variables are negative, indicating that increasing atmospheric concentrations of either of these two compounds is associated with lower global temperatures.
#ans-all of the gas concentration variables reflect human development - N2O and CFC.11 are correlated with other variables in the data set
#Compute the correlations between all the variables in the training set. Which of the following independent variables is N2O highly correlated with 
cor(climate_train)
#Which of the following independent variables is CFC.11 highly correlated with?

#Simplifying the Model 

model2=lm(data=climate_train, Temp~MEI+N2O+TSI+Aerosols)
#Enter the coefficient of N2O in this reduced model:
summary(model2)

#Automatically Building the Model 

# R provides a function, step, that will automate the procedure of trying different combinations of variables to find a good compromise of model simplicity and R2
#Which of the variable(s) were eliminated from the full model by the step function?
newmodel=step(model1)
summary(newmodel)

#Testing on Unseen Data 

climate_test=subset(climate, Year>2006 )
summary(climate_test)
pred=predict(newmodel,newdata=climate_test)
summary(pred)
#calculate temperature predictions for the testing data set, using the predict function.
SSE = sum((pred - climate_test$Temp)^2)
SST = sum( (mean(climate_train$Temp) - climate_test$Temp)^2)
R2 = 1 - SSE/SST
R2
