moody <- read.csv("M2022train.csv")

moody$Grade <- as.factor(moody$Grade)
moody$Major <- as.factor(moody$Major)
moody$Seniority <- as.factor(moody$Seniority)

summary(moody[moody$Grade == 'A',])
summary(moody[moody$Grade == 'B',])
summary(moody[moody$Grade == 'C',])
summary(moody[moody$Grade == 'D',])
summary(moody[moody$Grade == 'F',])


mean.a <- mean(moody[moody$Grade=='A', ]$Score)
mean.a
mean.b <- mean(moody[moody$Grade=='B', ]$Score)
mean.b
mean.c <- mean(moody[moody$Grade=='C', ]$Score)
mean.c
mean.d <- mean(moody[moody$Grade=='D', ]$Score)
mean.d
mean.f <- mean(moody[moody$Grade=='F', ]$Score)
mean.f


#----------------------------------------#
mean.cs.a <- mean(moody[moody$Grade=='A' & moody$Major=='CS', ]$Score)
mean.cs.a
mean.cs.b <- mean(moody[moody$Grade=='B' & moody$Major=='CS', ]$Score)
mean.cs.b
mean.cs.c <- mean(moody[moody$Grade=='C' & moody$Major=='CS', ]$Score)
mean.cs.c
mean.cs.d <- mean(moody[moody$Grade=='D' & moody$Major=='CS', ]$Score)
mean.cs.d
mean.cs.f <- mean(moody[moody$Grade=='F' & moody$Major=='CS', ]$Score)
mean.cs.f

#----
mean.cs.a.fresh <- mean(moody[moody$Grade=='A' & moody$Major=='CS' & moody$Seniority=='Freshman', ]$Score)
mean.cs.a.fresh
mean.cs.b.fresh <- mean(moody[moody$Grade=='B' & moody$Major=='CS' & moody$Seniority=='Freshman', ]$Score)
mean.cs.b.fresh
mean.cs.c.fresh <- mean(moody[moody$Grade=='C' & moody$Major=='CS' & moody$Seniority=='Freshman', ]$Score)
mean.cs.c.fresh
mean.cs.d.fresh <- mean(moody[moody$Grade=='D' & moody$Major=='CS' & moody$Seniority=='Freshman', ]$Score)
mean.cs.d.fresh
mean.cs.f.fresh <- mean(moody[moody$Grade=='F' & moody$Major=='CS' & moody$Seniority=='Freshman', ]$Score)
mean.cs.f.fresh

#----
mean.cs.a.soph <- mean(moody[moody$Grade=='A' & moody$Major=='CS' & moody$Seniority=='Sophomore', ]$Score)
mean.cs.a.soph
mean.cs.b.soph <- mean(moody[moody$Grade=='B' & moody$Major=='CS' & moody$Seniority=='Sophomore', ]$Score)
mean.cs.b.soph
mean.cs.c.soph <- mean(moody[moody$Grade=='C' & moody$Major=='CS' & moody$Seniority=='Sophomore', ]$Score)
mean.cs.c.soph
mean.cs.d.soph <- mean(moody[moody$Grade=='D' & moody$Major=='CS' & moody$Seniority=='Sophomore', ]$Score)
mean.cs.d.soph
mean.cs.f.soph <- mean(moody[moody$Grade=='F' & moody$Major=='CS' & moody$Seniority=='Sophomore', ]$Score)
mean.cs.f.soph

#----
mean.cs.a.jr <- mean(moody[moody$Grade=='A' & moody$Major=='CS' & moody$Seniority=='Junior', ]$Score)
mean.cs.a.jr
mean.cs.b.jr <- mean(moody[moody$Grade=='B' & moody$Major=='CS' & moody$Seniority=='Junior', ]$Score)
mean.cs.b.jr
mean.cs.c.jr <- mean(moody[moody$Grade=='C' & moody$Major=='CS' & moody$Seniority=='Junior', ]$Score)
mean.cs.c.jr
mean.cs.d.jr <- mean(moody[moody$Grade=='D' & moody$Major=='CS' & moody$Seniority=='Junior', ]$Score)
mean.cs.d.jr
mean.cs.f.jr <- mean(moody[moody$Grade=='F' & moody$Major=='CS' & moody$Seniority=='Junior', ]$Score)
mean.cs.f.jr

#----
mean.cs.a.sr <- mean(moody[moody$Grade=='A' & moody$Major=='CS' & moody$Seniority=='Senior', ]$Score)
mean.cs.a.sr
mean.cs.b.sr <- mean(moody[moody$Grade=='B' & moody$Major=='CS' & moody$Seniority=='Senior', ]$Score)
mean.cs.b.sr
mean.cs.c.sr <- mean(moody[moody$Grade=='C' & moody$Major=='CS' & moody$Seniority=='Senior', ]$Score)
mean.cs.c.sr
mean.cs.d.sr <- mean(moody[moody$Grade=='D' & moody$Major=='CS' & moody$Seniority=='Senior', ]$Score)
mean.cs.d.sr
mean.cs.f.sr <- mean(moody[moody$Grade=='F' & moody$Major=='CS' & moody$Seniority=='Senior', ]$Score)
mean.cs.f.sr

#----------------------------------------#



range <- tapply(moody$Score,moody$Grade, range)
range


boxplot(moody$Score ~ moody$Grade, main = "Scores vs Grades", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

mosaicplot(factor(moody$Seniority,levels = c("Freshman", "Sophomore", "Junior", "Senior")) ~ moody$Grade, main = "Grades vs Seniority", xlab = "Seniority", ylab = "Grade", col = c("gray", "light blue"))

mosaicplot(moody$Major ~ moody$Grade, main = "Grades vs Major", xlab = "Major", ylab = "Grade", col = c("gray", "light blue"))

par(mfrow = c(2, 2))

econ <- subset(moody, moody$Major == "Economics")
boxplot(econ$Score ~ econ$Grade, main = "Scores vs Grades for Econ", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

cs <- subset(moody, moody$Major == "CS")
boxplot(cs$Score ~ cs$Grade, main = "Scores vs Grades for CS", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))


psych <- subset(moody, moody$Major == "Pyschology")
#boxplot(psych$Score ~ psych$Grade, main = "Scores vs Grades for Psych", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

stats <- subset(moody, moody$Major == "Statistics")
boxplot(stats$Score ~ stats$Grade, main = "Scores vs Grades for Stats", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))


mosaicplot(factor(econ$Seniority, levels = c("Freshman", "Sophomore", "Junior", "Senior")) ~ econ$Grade, main = "Grade vs Seniority for Econ", xlab = "Seniority", ylab = "Grade", col = c("gray", "light blue"))

mosaicplot(factor(cs$Seniority, levels = c("Freshman", "Sophomore", "Junior", "Senior")) ~ cs$Grade, main = "Grade vs Seniority for CS", xlab = "Seniority", ylab = "Grade", col = c("gray", "light blue"))

#mosaicplot(factor(psych$Seniority, levels = c("Freshman", "Sophomore", "Junior", "Senior")) ~ psych$Grade, main = "Grade vs Seniority for Psych", xlab = "Seniority", ylab = "Grade", col = c("gray", "light blue"))

fresh <- subset(moody, moody$Seniority == "Freshman")
boxplot(fresh$Score ~ fresh$Grade, main = "Scores vs Grades for Freshmen", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

soph <- subset(moody, moody$Seniority == "Sophomore")
boxplot(soph$Score ~ soph$Grade, main = "Scores vs Grades for Sophomores", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

junior <- subset(moody, moody$Seniority == "Junior")
boxplot(junior$Score ~ junior$Grade, main = "Scores vs Grades for Juniors", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

senior <- subset(moody, moody$Seniority == "Senior")
boxplot(senior$Score ~ senior$Grade, main = "Scores vs Grades for Seniors", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

cs.senior <- subset(cs, cs$Seniority == "Senior")
boxplot(cs.senior$Score ~ cs.senior$Grade, main = "Scores vs Grades for CS Seniors", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

cs.junior <- subset(cs, cs$Seniority == "Junior")
boxplot(cs.junior$Score ~ cs.junior$Grade, main = "Scores vs Grades for CS Juniors", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

cs.soph <- subset(cs, cs$Seniority == "Sophomore")
boxplot(cs.soph$Score ~ cs.soph$Grade, main = "Scores vs Grades for CS Sophomores", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

cs.fresh <- subset(cs, cs$Seniority == "Freshman")
boxplot(cs.fresh$Score ~ cs.fresh$Grade, main = "Scores vs Grades for CS Freshman", xlab = "Grade", ylab = "Score", col = c("gray", "light blue"))

#----------------------------------------#
mean.psych.a <- mean(moody[moody$Grade=='A' & moody$Major=='Psychology', ]$Score)
mean.psych.a
mean.psych.b <- mean(moody[moody$Grade=='B' & moody$Major=='Psychology', ]$Score)
mean.psych.b
mean.psych.c <- mean(moody[moody$Grade=='C' & moody$Major=='Psychology', ]$Score)
mean.psych.c
mean.psych.d <- mean(moody[moody$Grade=='D' & moody$Major=='Psychology', ]$Score)
mean.psych.d
mean.psych.f <- mean(moody[moody$Grade=='F' & moody$Major=='Psychology', ]$Score)
mean.psych.f

#----------------------------------------#
mean.stats.a <- mean(moody[moody$Grade=='A' & moody$Major=='Statistics', ]$Score)
mean.stats.a
mean.stats.b <- mean(moody[moody$Grade=='B' & moody$Major=='Statistics', ]$Score)
mean.stats.b
mean.stats.c <- mean(moody[moody$Grade=='C' & moody$Major=='Statistics', ]$Score)
mean.stats.c
mean.stats.d <- mean(moody[moody$Grade=='D' & moody$Major=='Statistics', ]$Score)
mean.stats.d
mean.stats.f <- mean(moody[moody$Grade=='F' & moody$Major=='Statistics', ]$Score)
mean.stats.f
#----------------------------------------#
mean.econ.a <- mean(moody[moody$Grade=='A' & moody$Major=='Economics', ]$Score)
mean.econ.a
mean.econ.b <- mean(moody[moody$Grade=='B' & moody$Major=='Economics', ]$Score)
mean.econ.b
mean.econ.c <- mean(moody[moody$Grade=='C' & moody$Major=='Economics', ]$Score)
mean.econ.c
mean.econ.d <- mean(moody[moody$Grade=='D' & moody$Major=='Economics', ]$Score)
mean.econ.d
mean.econ.f <- mean(moody[moody$Grade=='F' & moody$Major=='Economics', ]$Score)
mean.econ.f



train <- moody[sample(1:nrow(moody)), ]
training <- train[1:100,]
testing <- train[101:nrow(train),]

myprediction <- training
decision <- rep("F", nrow(myprediction))

decision[myprediction$Major == "CS" & myprediction$Seniority == "Senior" & myprediction$Score >= 92] <- "A"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Senior" & myprediction$Score >= 84] <- "B"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Senior" & myprediction$Score >= 70] <- "C"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Senior" & myprediction$Score >= 54] <- "D"

decision[myprediction$Major == "CS" & myprediction$Seniority == "Junior" & myprediction$Score >= 92] <- "A"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Junior" & myprediction$Score >= 84] <- "B"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Junior" & myprediction$Score >= 64] <- "C"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Junior" & myprediction$Score >= 54] <- "D"

decision[myprediction$Major == "CS" & myprediction$Seniority == "Sophomore" & myprediction$Score >= 93] <- "A"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Sophomore" & myprediction$Score >= 82] <- "B"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Sophomore" & myprediction$Score >= 70] <- "C"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Sophomore" & myprediction$Score >= 74] <- "D"

decision[myprediction$Major == "CS" & myprediction$Seniority == "Freshman" & myprediction$Score >= 85] <- "A"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Freshman" & myprediction$Score >= 71] <- "B"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Freshman" & myprediction$Score >= 48] <- "C"
decision[myprediction$Major == "CS" & myprediction$Seniority == "Freshman" & myprediction$Score >= 34] <- "D"

decision[myprediction$Major == "Economics" & myprediction$Score >= 87] <- "A"
decision[myprediction$Major == "Economics" & myprediction$Score >= 63] <- "B"
decision[myprediction$Major == "Economics" & myprediction$Score >= 51] <- "C"
decision[myprediction$Major == "Economics" & myprediction$Score >= 26] <- "D"

decision[myprediction$Major == "Pyschology" & myprediction$Score >= 83] <- "A"
decision[myprediction$Major == "Psychology" & myprediction$Score >= 59] <- "B"
decision[myprediction$Major == "Psychology" & myprediction$Score >= 48] <- "C"
decision[myprediction$Major == "Psychology" & myprediction$Score >= 24] <- "D"

decision[myprediction$Major == "Statistics" & myprediction$Score >= 92] <- "A"
decision[myprediction$Major == "Statistics" & myprediction$Score >= 72] <- "B"
decision[myprediction$Major == "Statistics" & myprediction$Score >= 58] <- "C"
decision[myprediction$Major == "Statistics" & myprediction$Score >= 37] <- "D"

myprediction$Grade <- decision
error <- mean(training$Grade != myprediction$Grade)
error


testSNoGrade <- read.csv("M2022testSNoGrade.csv")
mysubmission <- read.csv("M2022submission.csv")
mypredictiontest <- testSNoGrade
decisiontest <- rep("F",nrow(mypredictiontest))

decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Senior" & myprediction$Score >= 92] <- "A"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Senior" & myprediction$Score >= 84] <- "B"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Senior" & myprediction$Score >= 70] <- "C"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Senior" & myprediction$Score >= 54] <- "D"

decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Junior" & myprediction$Score >= 92] <- "A"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Junior" & myprediction$Score >= 84] <- "B"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Junior" & myprediction$Score >= 64] <- "C"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Junior" & myprediction$Score >= 54] <- "D"

decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Sophomore" & myprediction$Score >= 93] <- "A"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Sophomore" & myprediction$Score >= 82] <- "B"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Sophomore" & myprediction$Score >= 70] <- "C"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Sophomore" & myprediction$Score >= 74] <- "D"

decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Freshman" & myprediction$Score >= 85] <- "A"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Freshman" & myprediction$Score >= 71] <- "B"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Freshman" & myprediction$Score >= 48] <- "C"
decisiontest[myprediction$Major == "CS" & myprediction$Seniority == "Freshman" & myprediction$Score >= 34] <- "D"

decisiontest[myprediction$Major == "Economics" & myprediction$Score >= 87] <- "A"
decisiontest[myprediction$Major == "Economics" & myprediction$Score >= 63] <- "B"
decisiontest[myprediction$Major == "Economics" & myprediction$Score >= 51] <- "C"
decisiontest[myprediction$Major == "Economics" & myprediction$Score >= 26] <- "D"

decisiontest[myprediction$Major == "Pyschology" & myprediction$Score >= 83] <- "A"
decisiontest[myprediction$Major == "Psychology" & myprediction$Score >= 59] <- "B"
decisiontest[myprediction$Major == "Psychology" & myprediction$Score >= 48] <- "C"
decisiontest[myprediction$Major == "Psychology" & myprediction$Score >= 24] <- "D"

decisiontest[myprediction$Major == "Statistics" & myprediction$Score >= 92] <- "A"
decisiontest[myprediction$Major == "Statistics" & myprediction$Score >= 72] <- "B"
decisiontest[myprediction$Major == "Statistics" & myprediction$Score >= 58] <- "C"
decisiontest[myprediction$Major == "Statistics" & myprediction$Score >= 37] <- "D"
mypredictiontest$Grade <- decisiontest
mypredictiontest$Grade = mysubmission$Grade
mysubmission$Grade <- decisiontest


file <-read.csv("M2022submission.csv")
file$Grade <- decisiontest

write.csv(mysubmission, file = "mysubmission2.csv", row.names=FALSE)





