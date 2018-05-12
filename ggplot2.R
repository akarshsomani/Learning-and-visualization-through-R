?iris
#scatter plot
plot(iris$Sepal.Length,iris$Petal.Length)
#modify labels
plot(iris$Sepal.Length,iris$Petal.Length,xlab = "Sepel Length", ylab="Petal Length",main = "sepal VS petal Length")
#add color
plot(iris$Sepal.Length,iris$Petal.Length,xlab = "Sepel Length", ylab="Petal Length",main = "sepal VS petal Length",col="blue", pch=16)
#histogram
hist(iris$Sepal.Width)
hist(iris$Sepal.Width,col="aquamarine")
#boxplot
boxplot(iris$Sepal.Length~iris$Species,col="burlywood")

library(ggplot2)
ggplot(data=iris)#it will give a blank plot
ggplot(data=iris,aes(y=Sepal.Length,x=Petal.Length))
ggplot(data=iris,aes(y=Sepal.Length,x=Petal.Length))+geom_point()#it will give a scatter plot
#Aesthetics
ggplot(data=iris,aes(y=Sepal.Length,x=Petal.Length,col=Species))+geom_point()
ggplot(data=iris,aes(y=Sepal.Length,x=Petal.Length,shape=Species))+geom_point()
ggplot(data=iris,aes(y=Sepal.Length,x=Petal.Length,shape=Species,col=Species))+geom_point()

houses= read.csv("C:/Users/Akars/Downloads/HousePrice.csv")
print(houses)
#histogram
ggplot(data=houses,aes(x=Price))+geom_histogram()
ggplot(data=houses,aes(x=Price))+geom_histogram(bins = 50)
ggplot(data=houses,aes(x=Price))+geom_histogram(bins = 50,fill="palegreen4")
ggplot(data=houses,aes(x=Price))+geom_histogram(bins = 50,fill="palegreen4",col="green")
#position
ggplot(data=houses,aes(x=Price,fill=Brick))+geom_histogram(bins=50)
ggplot(data=houses,aes(x=Price,fill=Brick))+geom_histogram(bins=50,position="fill")#it will give proportion
ggplot(data=houses,aes(x=Price,fill=Brick))+geom_histogram(bins=50,position="identity")#it will give proportion

#barplot
ggplot(data=houses,aes(x=Bedrooms))+geom_bar()
ggplot(data=houses,aes(x=Bedrooms, fill=Brick))+geom_bar()
ggplot(data=houses,aes(x=Bedrooms, fill=Neighborhood))+geom_bar(position="fill")#fill should only have true or false data or string data


#Frequency_polygon
ggplot(data=houses, aes(x=Price))+geom_freqpoly()
ggplot(data=houses, aes(x=Price))+geom_freqpoly(bins=60)
ggplot(data=houses, aes(x=Price, col=Brick))+geom_freqpoly(bins=60)
ggplot(data=houses, aes(x=Price, col=Neighborhood))+geom_freqpoly(bins=60)

#boxplot
ggplot(data=houses, aes(y=Price,x=factor(Bedrooms)))+geom_boxplot()
ggplot(data=houses, aes(y=Price,x=factor(Bedrooms),fill=factor(Bedrooms)))+geom_boxplot()
ggplot(data=houses, aes(y=Price,x=factor(Bedrooms),fill=factor(Offers)))+geom_boxplot()
ggplot(data=houses, aes(y=Price,x=factor(Bedrooms),fill=factor(Neighborhood)))+geom_boxplot()

#smooth_line
ggplot(data=houses, aes(y=Price,x=SqFt))+geom_smooth(se=F)#se=f is to remove error
ggplot(data=houses, aes(y=Price,x=SqFt,col=Neighborhood))+geom_smooth(se=F)
ggplot(data=houses, aes(y=Price,x=SqFt,col=Brick))+geom_smooth(se=F)

##method="lm" lm=linear model
ggplot(data=houses, aes(y=Price,x=SqFt))+geom_point()+geom_smooth(method="lm",se=F)
ggplot(data=houses, aes(y=Price,x=SqFt,col=Brick))+geom_point()+geom_smooth(method="lm",se=F)

#Faceting-to divide the graph in different curves to lessen the crowd in one curve
ggplot(data=houses, aes(y=Price,x=SqFt))+geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~Brick)
ggplot(data=houses, aes(y=Price,x=SqFt,col=Brick))+geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~Brick)
ggplot(data=houses, aes(y=Price,x=SqFt,col=Brick))+geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~Bedrooms)

#Theme-1
library(scales)
obj1 = ggplot(data=houses, aes(y=Price,x=factor(Bedrooms),fill=factor(Bedrooms)))+geom_boxplot()
obj1+ labs(title="Price WRT Room", x="Rooms", fill="Rooms")->obj2
obj2+theme(panel.background = element_rect(fill="palegreen1"))->obj3
obj3+theme(plot.title= element_text(hjust=0.5,face="bold",colour="cadetblue"))->obj4
obj4+scale_y_continuous(labels=dollar)->obj5
obj1
obj2
obj3
obj4
obj5

#Theme-2
g1=ggplot(data=houses, aes(y=Price,x=SqFt,col=Brick))+geom_point()+geom_smooth(method="lm",se=F)
g2=g1+theme(panel.background = element_rect(fill="lemonchiffon2"))
g3=g2+theme(legend.background = element_rect(fill = "lightcyan4"))
g4=g3+theme(plot.background = element_rect(fill="lightcoral"))
g1
g2
g3
g4

#Theme-3
p1=ggplot(data=houses, aes(x=Price, col=Brick))+geom_freqpoly(size=1,bins=60)+scale_x_continuous(labels = dollar)
p2=p1+theme(panel.background = element_rect(fill = "peachpuff"))
p3=p2+labs(title="Frequency polyfon of price")
p4=p3+theme(plot.title = element_text(hjust = 0.5),plot.background = element_rect("aquamarine4"))
p1
p2
p3
p4
