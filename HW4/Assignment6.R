getwd()

movies <- read.csv("Movies2022F-4.csv")

summary(movies)

head(movies)

hist(movies$imdb_score, breaks = 50)

mu <- mean(movies$imdb_score)
sd <- sd(movies$imdb_score)
movies[,ncol(movies)] <- ((movies$imdb_score - mu)/sd)

movies.cat1 <- subset(movies, movies$Budget == "Low")
movies.cat2 <- subset(movies, movies$Budget == "High")

cat1.scores <- movies.cat1$imdb_score
cat2.scores <- movies.cat2$imdb_score

hist(cat1.scores)
hist(cat2.scores)

mean.cat1 <- mean(cat1.scores)
mean.cat2 <- mean(cat2.scores)


sd.cat1 <- sd(cat1.scores)
sd.cat2 <- sd(cat2.scores)

n.cat1 <- length(cat1.scores)
n.cat2 <- length(cat2.scores)

z <- (mean.cat1 - mean.cat2)/sqrt(sd.cat1^2/n.cat1 + sd.cat2^2/n.cat2)

plot(x=seq(from = -10, to= 10, by=0.1),
     y=dnorm(seq(from = -10, to= 10,  by=0.1),mean=0),
     type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=z, col='red')
p <- 1-pnorm(z)
p


mod <- subset(movies, select=c(3,5))

low.data <- subset(movies, movies$Budget == "Low")
high.data <- subset(movies, movies$Budget == "High")

low.budget <- low.data$imdb_score
high.budget <- high.data$imdb_score

mean.low <- mean(low.budget)
sprintf("%f mean value of low budget movies", mean.low)
mean.high <- mean(high.budget)
sprintf("%f mean value of high budget movies", mean.high)

x <- PermutationTestSecond::Permutation(mod, "Budget", "imdb_score", 10000, "High", "Low")
sprintf("%f p-value using permutation_test", x)

print("Hence, we do fail to reject our null hypothesis -> mean score of low budget movies are higher than mean of high budget movies, so that means a higher budget doesn't necessarily mean higher score")







