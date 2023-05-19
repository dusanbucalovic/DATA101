getwd()

moody<-read.csv("moody2022_new.csv")

head(moody)

summary(moody)

mosaicplot(moody$GRADE~moody$TEXTING_IN_CLASS, main = "Amount of Texting vs Grades", ylab="Amount of Texting", xlab="Grade", col=c("red","green","yellow","blue"))

mosaicplot(moody$GRADE~moody$DOZES_OFF, main = "Dozing off vs Grades", ylab="Dozes off", xlab="Grade", col=c("red","green","yellow","blue"))

plot(moody$SCORE, moody$PARTICIPATION, xlab = "Score", ylab = "Participation", main = "Score vs Participation", col=c("purple"))

