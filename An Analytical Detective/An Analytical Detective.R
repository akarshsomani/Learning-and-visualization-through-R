#Loading the Data 

mvt=read.csv("C:/Users/Akars/Downloads/mvtWeek1.csv")
#How many rows of data (observations) are in this dataset?
str(mvt)
##Using the "max" function, what is the maximum value of the variable "ID"?
maximumID=max(mvt$ID)
print(maximumID)
#What is the minimum value of the variable "Beat"?
minimumBEAT=min(mvt$Beat)
print(minimumBEAT)
#How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
#How many observations have a LocationDescription value of ALLEY?
summary(mvt)


#Understanding Dates in R 

#In what format are the entries in the variable Date?
print(mvt$Date[1])
#What is the month and year of the median date in our dataset? Enter your answer as "Month Year", without the quotes.
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
print(summary(DateConvert))
#In which month did the fewest motor vehicle thefts occur?
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
print(table(mvt$Month))
#In which weekday did the fewest motor vehicle thefts occur?
print(table(mvt$Weekday))
#Each observation in the dataset represents a motor vehicle theft, and the Arrest variable indicates whether an arrest was later made for this theft. Which month has the largest number of motor vehicle thefts for which an arrest was made?
print(table(mvt$Arrest,mvt$Month))

#Visualizing Crime Trends 
library(ggplot2)
#let's make a histogram of the variable Date.
#hist(mvt$Date, breaks=100) or
ggplot(data= mvt, aes(x=Date,col="red"))+geom_histogram(bins=100)
#Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half of the time period? 
ggplot(data=mvt, aes(y=Date,x=Arrest))+geom_boxplot()
#boxplot(mvt$Date ~ mvt$Arrest)
#For what proportion of motor vehicle thefts in 2001 was an arrest made? 
ggplot(data=mvt, aes(x=Date, fill=Arrest))+geom_histogram(bins=100)
print(table(mvt$Year,mvt$Arrest))

#Popular Locations 

#Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category?
print(sort(table(mvt$LocationDescription)))
#How many observations are in Top5?
Top5 = subset(mvt, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")#it will only consider the given rest will be ignored
print(str(Top5))
#One of the locations has a much higher arrest rate than the other locations. Which is it? 
Top5$LocationDescription = factor(Top5$LocationDescription)
print(table(Top5$LocationDescription,Top5$Arrest))
#On which day of the week do the most motor vehicle thefts at gas stations happen?
#On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
print(table(Top5$LocationDescription,Top5$Weekday))

