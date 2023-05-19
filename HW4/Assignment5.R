getwd()

movies <- read.csv("Movies2022F-4.csv")

summary(movies)

head(movies)

movies1<-tapply(movies$imdb_score,movies$Budget,mean)
movies1 

barplot(movies1,col = "cyan",xlab = "Budget", ylab = "Mean Score",main = "Mean score vs Budget",las = 2, cex.names = 0.75)


movies2<-tapply(movies$imdb_score,movies$genre,mean)
movies2 

barplot(movies2,col = "maroon",xlab = "Genre", ylab = "Mean Score",main = "Mean score vs Genre",las = 2, cex.names = 0.75)


movies3<-tapply(movies$imdb_score,movies$Gross,mean)
movies3 

barplot(movies3,col = "grey",xlab = "Gross", ylab = "Mean Score",main = "Mean score vs Gross",las = 2, cex.names = 0.75)


barplot(movies1,col = "cyan",xlab = "Budget", ylab = "Mean Score",main = "Mean score vs Budget",las = 2, cex.names = 0.75)


#mean
mean.movies <- mean(movies$imdb_score)
mean.movies

#z score
zeta <- ((mean.movies)/sd(movies$imdb_score))
 
#plot
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=zeta, col='red')
 
#get p
p = 1-pnorm(zeta)
p

zeta













