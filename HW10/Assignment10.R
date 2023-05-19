library(readr)
install.packages(rpart)
moody <- read.csv('M2022train.csv')
summary(moody)
library(rpart)
library(rpart.plot)

tree <- rpart(Grade ~ Score+Seniority+Major,data=moody)
tree <- rpart(Grade ~ Score+Seniority+Major,data=moody, control = rpart.control(minbucket=10))
rpart.plot(tree)
tree <- rpart(Grade ~ Score+Seniority+Major,data=moody,control = rpart.control(minbucket=60, minsplit=60))
rpart.plot(tree)
tree <- rpart(Grade ~ Score+Seniority+Major,data=moody,control = rpart.control(minsplit=400))
rpart.plot(tree)
tree <- rpart(Grade ~ Score+Seniority+Major,data=moody,control = rpart.control(minbucket=100))
rpart.plot(tree)

file <- predict(tree, newmoody=moody ,type="class")
moody$predict <- file
error <- mean(moody$Grade == moody$predict)
error

View(moody)
devtools::install_github("devanshagr/CrossValidation")
CrossValidation::cross_validate(moody, tree, 5, 0.7)

test<- read.csv('M2022testSNoGrade.csv')
submission <- read.csv('M2022submission.csv')
prediction<- predict(tree, newdata=test ,type="class")
submission$Grade <- prediction
write.csv(submission, file = "mysubmission.csv",row.names=FALSE)