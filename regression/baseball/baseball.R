baseball= read.csv("C:/Users/Akars/Documents/R project/regression/baseball/baseball.csv")

moneyball=subset(baseball, Year<2002)

moneyball$RD=moneyball$RS-moneyball$RA
plot(moneyball$RD, moneyball$W)
winregs=lm(data=moneyball,W ~ RD)
summary(winregs)
#win=80.88137 + 0.105766*RD  (from the summary)

runsreg=lm(data=moneyball, RS ~ OBP+SLG)
summary(runsreg)
#as we can see that -ve batting average still the r-squared is good which is less significant.

teamRank = c(1,2,3,3,4,4,4,4,5,5)

wins2012 = c(94,88,95,88,93,94,98,97,93,94)
cor(teamRank,wins2012)

wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2013)

