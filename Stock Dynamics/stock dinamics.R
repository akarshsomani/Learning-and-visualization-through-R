#Reading the csv file
IBM=read.csv("C:/Users/Akars/Documents/R project/Stock Dynamics/IBMStock.csv")
GE=read.csv("C:/Users/Akars/Documents/R project/Stock Dynamics/GEStock.csv")
ProcterGamble=read.csv("C:/Users/Akars/Documents/R project/Stock Dynamics/ProcterGambleStock.csv")
CocaCola=read.csv("C:/Users/Akars/Documents/R project/Stock Dynamics/CocaColaStock.csv")
Boeing=read.csv("C:/Users/Akars/Documents/R project/Stock Dynamics/BoeingStock.csv")

#Summary Statistics 

#Date as demanded in the question
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
#Our five datasets all have the same number of observations. How many observations are there in each data set? 
str(IBM)
#What is the earliest year in our datasets?
#What is the mean stock price of IBM over this time period?
#What is the latest year in our datasets?
summary(IBM)
#What is the minimum stock price of General Electric (GE) over this time period?
summary(GE)
#What is the maximum stock price of Coca-Cola over this time period?
summary(CocaCola)
#What is the median stock price of Boeing over this time period?
summary(Boeing)
#What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)

#Visualizing Stock Dynamics-Let's plot the stock prices to see if we can visualize trends in stock prices during this time period

library(ggplot2)
#plot the Date on the x-axis and the StockPrice on the y-axis, for Coca-Cola
ggplot(data=CocaCola, aes(x=Date,y=StockPrice))+geom_line()
#In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more? 
#In the time period shown in the plot, which stock generally has lower values?
df <- data.frame(x=CocaCola$Date,y1= CocaCola$StockPrice, y2=ProcterGamble$StockPrice)
ggplot(df, aes(x, y = value, color = variable)) + 
  geom_line(aes(y = y1, col = "CocaCola")) + 
  geom_line(aes(y = y2, col = "ProcterGamble"))#then we can compair the price drop

#Visualizing Stock Dynamics-Let's take a look at how the stock prices changed from 1995-2005 for all five companies

#Which stock reaches the highest value in the time period 1995-2005?
#In the last two years of this time period (2004 and 2005) which stock seems to be performing the best, in terms of increasing stock price?
df <- data.frame(x=CocaCola$Date[301:432],y1= CocaCola$StockPrice[301:432], y2=ProcterGamble$StockPrice[301:432],y3=IBM$StockPrice[301:432],y4=Boeing$StockPrice[301:432],y5=GE$StockPrice[301:432])
ggplot(df, aes(x, y = value, color = variable)) + 
  geom_line(aes(y = y1, col = "CocaCola")) + 
  geom_line(aes(y = y2, col = "ProcterGamble"))+ 
  geom_line(aes(y = y3, col = "IBM"))+ 
  geom_line(aes(y = y4, col = "Boeing"))+ 
  geom_line(aes(y = y5, col = "GE"))

#Monthly Trends 

#In which months has IBM historically had a higher stock price (on average)?
mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean)
#General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this?
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean)
#For the months of December and January, every company's average stock is higher in one month and lower in the other. In which month are the stock prices lower?