getwd()

death<-read.csv("CausesOfDeath_France_2001-2008.csv")

head(death)

summary(death)

ICD10<-table(death$ICD10)

ICD10

boxplot(death$TIME, main="Timeframe of Data", ylab="Year", cex.main=1.5, cex.ylab=1.5)
boxplot(death$TIME ~ death$Value)
