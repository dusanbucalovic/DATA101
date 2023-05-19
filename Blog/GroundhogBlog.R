groundhog <- read.csv("archive.csv")
summary(groundhog)
head(groundhog)

yes.shadow <- subset(groundhog, groundhog$Punxsutawney.Phil == "Full Shadow")
no.shadow <- subset(groundhog, groundhog$Punxsutawney.Phil == "No Shadow")

yes.temp <- yes.shadow$March.Average.Temperature..Pennsylvania.
no.temp <- no.shadow$March.Average.Temperature..Pennsylvania.

barplot(no.temp, main="Average March Temperatures vs No Shadow", ylab="Temperatures", xlab="Different years", col=c("light green"))
barplot(yes.temp, main="Average March Temperatures vs Full Shadow", ylab="Temperatures", xlab="Different years", col=c("light blue"))



hist(groundhog$March.Average.Temperature..Pennsylvania., breaks = 50)

mu <- mean(groundhog$March.Average.Temperature..Pennsylvania.)
sd <- sd(groundhog$March.Average.Temperature..Pennsylvania.)
groundhog[,ncol(groundhog)] <- ((groundhog$March.Average.Temperature..Pennsylvania. - mu)/sd)


hist(yes.temp)
hist(no.temp)

mean.yes.temp <- mean(yes.temp)
mean.no.temp <- mean(no.temp)


sd.yes.temp <- sd(yes.temp)
sd.no.temp <- sd(no.temp)

n.yes.temp <- length(yes.temp)
n.no.temp <- length(no.temp)

z <- (mean.no.temp - mean.yes.temp)/sqrt(sd.no.temp^2/n.no.temp + sd.yes.temp^2/n.yes.temp)

plot(x=seq(from = -10, to= 10, by=0.1),
     y=dnorm(seq(from = -10, to= 10,  by=0.1),mean=0),
     type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=z, col='red')
z
p <- 1-pnorm(z)
p


mod <- subset(groundhog, select=c(10,2))

yes.data <- subset(groundhog, groundhog$Punxsutawney.Phil == "Full Shadow")
no.data <- subset(groundhog, groundhog$Punxsutawney.Phil == "No Shadow")

cat1.shadow <- yes.data$groundhog$March.Average.Temperature..Pennsylvania.
cat2.shadow <- no.data$groundhog$March.Average.Temperature..Pennsylvania.

mean.yes <- mean(cat1.shadow)
sprintf("%f mean value of full shadow appearences", mean.yes)
mean.no <- mean(cat2.shadow)
sprintf("%f mean value of no shadow appearences", mean.no)

x <- PermutationTestSecond::Permutation(mod, "Punxsutawney.Phil", "March.Average.Temperature..Pennsylvania.", 10000, "Full Shadow", "No Shadow")
sprintf("%f p-value using permutation_test", x)

print("Hence, we do fail to reject our null hypothesis -> mean score of low budget movies are higher than mean of high budget movies, so that means a higher budget doesn't necessarily mean higher score")
