getwd()

cereal<-read.csv("cereal.csv")

head(cereal)

summary(cereal)

#barplot of calories in cereal
caloriesTable<-table(cereal$calories)
barplot(caloriesTable, main="Barplot for Calories in Cereals",xlab="# of Calories",ylab="# of Cereals",border="grey", col=c("green","green","green","green","yellow","yellow","yellow","red","red","red","red"))

#boxplot of sugars in cereal
boxplot(cereal$sugars, main="Sugars in Cereal", ylab="Amount of Sugars", col=c("lightblue"))

#scatter plot of calories vs sugars
plot(cereal$calories, cereal$sugars, xlab="Calories", ylab="Sugars", main="Calories vs Sugars", col=c("purple"))
