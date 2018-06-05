#Dataset size

pisaTrain =read.csv("C:/Users/Akars/Documents/R project/regression/reading test scores/pisa2009train.csv")
pisaTest =read.csv("C:/Users/Akars/Documents/R project/regression/reading test scores/pisa2009test.csv")
#How many students are there in the training set?
summary(pisaTrain)
#Using tapply() on pisaTrain, what is the average reading test score of males?
tapply(pisaTrain$readingScore,pisaTrain$male, mean)
#Which variables are missing data in at least one observation in the training set?
summary(pisaTrain)
#to remove observations with any missing value from pisaTrain and pisaTest:
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

#Factor variables 

#Factor variables are variables that take on a discrete set of values. This is an unordered factor because there isn't any natural ordering between the levels. An ordered factor has a natural ordering between the levels (an example would be the classifications "large," "medium," and "small").
#Which of the variables is an ordered factor with at least 3 levels? 
#Male only has 2 levels (1 and 0). There is no natural ordering between the different values of raceeth, so it is an unordered factor.
#Meanwhile, we can order grades (8, 9, 10, 11, 12), so it is an ordered factor.
pisaTrain$raceeth
#Which binary variables will be included in the regression model?
#We create a binary variable for each level except the reference level, so we would create all these variables except for raceethWhite. 

#Building a model 

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
#What is the Multiple R-squared value of lmScore on the training set?
lmscore=lm(readingScore~.,data=pisaTrain)
summary(lmscore)
#What is the training-set root-mean squared error (RMSE) of lmScore?
RMSE=sqrt(mean(lmscore$residuals^2))
RMSE
#Consider two students A and B. They have all variable values the same, except that student A is in grade 11 and student B is in grade 9. What is the predicted reading score of student A minus the predicted reading score of student B?
#The coefficient 29.54 on grade is the difference in reading score between two students who are identical other than having a difference in grade of 1. Because A and B have a difference in grade of 2, the model predicts that student A has a reading score that is 2*29.54 larger. 
#What is the meaning of the coefficient associated with variable raceethAsian?
#Predicted difference in the reading score between an Asian student and a white student who is otherwise identical 

#Predicting on unseen data 

#What is the range between the maximum and minimum predicted reading score on the test set?
predTest=predict(lmscore, newdata = pisaTest)
summary(predTest)
summary(pisaTest)
#What is the sum of squared errors (SSE) and RMSE of lmScore on the testing set?
sqrt(mean((na.omit(predTest-pisaTest$readingScore))^2))
#What is the predicted test score used in the baseline model? Remember to compute this value using the training set and not the test set.
baseline = mean(pisaTrain$readingScore) 
#What is the sum of squared errors of the baseline model on the testing set? HINT: We call the sum of squared errors for the baseline model the total sum of squares (SST).
sum((baseline-pisaTest$readingScore)^2)
