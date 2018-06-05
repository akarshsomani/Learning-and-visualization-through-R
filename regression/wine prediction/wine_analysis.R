#the below 4 models are training models

wine=read.csv("C:/Users/Akars/Documents/R project/regression/wine prediction/wine.csv")
str(wine)
summary(wine)
#way to make linear model
model1=lm(Price~AGST,data=wine)
model1$residuals
SSE=sum(model1$residuals^2)
cat("SSE= ", SSE)#printing command
#what if we include 2 variables
model2=lm(Price~AGST+HarvestRain, data=wine)
summary(model2)
SSE=sum(model2$residuals^2)
cat("SSE= ",SSE)
#what if we include all variables
model3=lm(Price~AGST+HarvestRain+WinterRain+Age+FrancePop, data=wine)
summary(model3)
SSE=sum(model3$residuals^2)
cat("SSE= ",SSE)
#more Pr means insignificant so we will neglect the most insignificant variable to reach the highest accuracy.
#see the summary table for Pr

#model after removing the most insignificant variable
model4=lm(Price~AGST+HarvestRain+WinterRain+Age, data=wine)
#So why didn't we keep FrancePopulation instead of Age?
#Well, we expect Age to be significant.Older wines are typically more expensive,so Age makes more intuitive sense in our model.
summary(model4)
SSE=sum(model4$residuals^2)
cat("SSE= ",SSE)

#inbuilt function to find correlation
cor(wine$WinterRain,wine$Price)
cor(wine$Age,wine$FrancePop)#shows highly negative correlated


cor(wine)#for all correaltion, check the rowname and coloumn name for finding correlation in the table

#Now due to the possibility of multicollinearity,
#you always want to remove the insignificant variables
#one at a time.

#There is no definitive cut-off value,for what makes a correlation too high.
#But typically, a correlation greater than 0.7 or less than -0.7 is cause for concern.
#If you look back at all of the correlations we computed for our data set, you can see that it doesn't look like we have any other highly-correlated independent variables.
#So we'll stick with model4 for the rest of this lecture, the model that uses AGST, HarvestRain, WinterRain, and Age as the independent variables.



wineTest=read.csv("C:/Users/Akars/Documents/R project/regression/wine prediction/wine_test.csv")
predictTest= predict(model4, newdata = wineTest)
predictTest#it is approx good
#check for r^2
SSE=sum((wineTest$Price-predictTest)^2)
SST=sum((wineTest$Price-mean(wine$Price))^2)
rr=1-SSE/SST
rr#r^2 is also significant
