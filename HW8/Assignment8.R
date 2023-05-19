market <- read.csv("Minimarket.csv")

summary(market)

mean1 = mean(market[market$BREAD==0, ]$BUTTER)
mean2 = mean(market[market$BREAD==1, ]$BUTTER)
mean1
mean2

mod <- subset(market, select=c(1,2))

x <- PermutationTestSecond::Permutation(mod, "BREAD", "BUTTER",1000,"1", "0")
sprintf("%f p-value using permutation_test", x)