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