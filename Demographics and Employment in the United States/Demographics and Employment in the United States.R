#Loading and Summarizing the Dataset 

CPS=read.csv("C:/Users/Akars/Documents/R project/Demographics and Employment in the United States/CPSData.csv")
# what is the most common industry of employment?
# What proportion of interviewees are citizens of the United States?
#Which variables have at least one interviewee with a missing (NA) value?
summary(CPS)
str(CPS)
#Which state has the fewest interviewees?
#Which state has the largest number of interviewees?
sort(table(CPS$State))
# For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
table(CPS$Hispanic,CPS$Race)

#Evaluating Missing Values 

#we try to identify if there is a pattern in the missing values in the dataset.
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))#yes, there is a pattern
table(CPS$Citizenship, is.na(CPS$Married))
#How many states had all interviewees living in a non-metropolitan area?
table(CPS$State, is.na(CPS$MetroAreaCode))
#Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))
#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
#Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all interviewees were non-metropolitan?
tapply(is.na(CPS$MetroAreaCode),CPS$State,mean) 

#Integrating Metropolitan Area Data - When analyzing a variable stored by a numeric code, we will often want to convert it into the values the codes represent. 
#To do this, we will use a dictionary, which maps the the code to the actual value of the variable. We have provided dictionaries MetroAreaCodes.csv and CountryCodes.csv, which respectively map MetroAreaCode and CountryOfBirthCode into their true values.

MetroAreaCode=read.csv("C:/Users/Akars/Documents/R project/Demographics and Employment in the United States/MetroAreaCodes.csv")
CountryOfBirthCode=read.csv("C:/Users/Akars/Documents/R project/Demographics and Employment in the United States/CountryCodes.csv")
#How many observations (codes for metropolitan areas) are there in MetroAreaMap?
summary(MetroAreaCode)
summary(CountryOfBirthCode)
#What is the name of the variable that was added to the data frame by the merge() operation?
CPS = merge(CPS, MetroAreaCode, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)# Note that all of these interviewees would have been removed from the merged data frame if we did not include the all.x=TRUE parameter.
#The first two arguments determine the data frames to be merged (they are called "x" and "y", respectively, in the subsequent parameters to the merge function). 
#by.x="MetroAreaCode" means we're matching on the MetroAreaCode variable from the "x" data frame (CPS), while by.y="Code" means we're matching on the Code variable from the "y" data frame (MetroAreaMap). 
#Finally, all.x=TRUE means we want to keep all rows from the "x" data frame (CPS), even if some of the rows' MetroAreaCode doesn't match any codes in MetroAreaMap (for those familiar with database terminology, this parameter makes the operation a left outer join instead of an inner join).
summary(CPS)
str(CPS)
#Which of the following metropolitan areas has the largest number of interviewees?
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))
# the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race=="Asian",CPS$MetroArea,mean))
#determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma.
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))#To get mean (and related functions, like sum) to ignore missing values, you can pass the parameter na.rm=TRUE

#Integrating Country of Birth Data 

CPS = merge(CPS, CountryOfBirthCode, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
#How many interviewees have a missing value for the new country of birth variable?
summary(CPS)
str(CPS)
#Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Country))
#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States? 
#For this computation, don't include people from this metropolitan area who have a missing country of birth.
table(CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country!="United States")
#Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India?
(table(CPS$MetroArea,CPS$Country=="India"))
#In Brazil?
(table(CPS$MetroArea,CPS$Country=="Brazil"))
