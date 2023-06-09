library(readr)
data <- read.csv('Earnings_Train2022-1.csv')
View(data)
library(rpart)
library(rpart.plot)

tree <- rpart(Earnings ~ Number_Of_Parking_Tickets+Number_Of_Credits+Graduation_Year+Major+Number_Of_Professional_Connections+GPA,data=data)
rpart.plot(tree)
pred =  predict(tree, newdata = data)
mean((pred-data$Earnings)^2)


#Then I tried Rpart 
library(rpart)
library(rpart.plot)
tree <- rpart(Earnings ~Major+Number_Of_Professional_Connections,data=data)
rpart.plot(tree)
# Most important factors are major, # professional connections
pred =  predict(tree, newdata = data)
mean((pred-data$Earnings)^2)
#MSE = 93,603.13



s = data[data$Major == "STEM",]
h = data[data$Major == "Humanities",]
p = data[data$Major == "Professional",]
v = data[data$Major == "Vocational",]

plot(b$Earnings~b$GPA)
plot(s$Earnings~s$GPA)
plot(h$Earnings~h$GPA)
plot(p$Earnings~p$GPA)


model.slm <- lm(Earnings~ GPA, data = s)
predS =  predict(model.slm, newdata = s)
mean((predS-s$Earnings)^2) 

model.hlm <- lm(Earnings~ GPA, data = h)
predH =  predict(model.hlm, newdata = h)
mean((predH-h$Earnings)^2) 

model.plm <- lm(Earnings~ GPA, data = p)
predP =  predict(model.plm, newdata = p)
mean((predP-p$Earnings)^2) 

model.vlm <- lm(Earnings~ GPA, data = v)
predV =  predict(model.vlm, newdata = v)
mean((predV-v$Earnings)^2) 

#Cross Validation 

for (val in c(1:iteration)){
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  
  #TRAIN
  other <- train[train$Major == "Other",]
  other$squared <- other$Number_Of_Professional_Connections^2
  bEven <- train[train$Major == "Buisness" & train$Graduation_Year %% 2 == 0,]
  bOdd <- train[train$Major == "Buisness" & train$Graduation_Year %% 2 != 0,]
  s = train[train$Major == "STEM",]
  h = train[train$Major == "Humanities",]
  p = train[train$Major == "Professional",]
  v = train[train$Major == "Vocational",]
  
  model.lm <- lm(Earnings~ Number_Of_Professional_Connectionssquared, data = other)
  model.bEven <- lm(Earnings~ GPA, data = bEven)
  model.bOdd <- lm(Earnings~ GPA, data = bOdd)
  model.slm <- lm(Earnings~ GPA, data = s)
  model.hlm <- lm(Earnings~ GPA, data = h)
  model.plm <- lm(Earnings~ GPA, data = p)
  model.vlm <- lm(Earnings~ GPA, data = v)
  
  
  #TEST
  other <- test[test$Major == "Other",]
  other
  other$squared <- other$Number_Of_Professional_Connections^2
  bEven <- test[test$Major == "Buisness" & test$Graduation_Year %% 2 == 0,]
  bOdd <- test[test$Major == "Buisness" & test$Graduation_Year %% 2 != 0,]
  s = test[test$Major == "STEM",]
  h = test[test$Major == "Humanities",]
  p = test[test$Major == "Professional",]
  v = test[test$Major == "Vocational",]
  
  
  predBEven =  predict(model.bEven, newdata = bEven)
  predBOdd =  predict(model.bOdd, newdata = bOdd)
  predOther =  predict(model.lm, newdata = other)
  predS =  predict(model.slm, newdata = s)
  predH =  predict(model.hlm, newdata = h)
  predP =  predict(model.plm, newdata = p)
  predV =  predict(model.vlm, newdata = v)
  
  predictCol <- rep(0,nrow(test))
  predictCol[test$Major == "Humanities"] = predH
  predictCol[test$Major == "Professional"] = predP
  predictCol[test$Major == "STEM"] = predS
  predictCol[test$Major == "Vocational"] = predV
  predictCol[test$Major == "Other"] = predOther
  predictCol[test$Major == "Buisness" & test$Graduation_Year %% 2 == 0] = predBEven
  predictCol[test$Major == "Buisness" & test$Graduation_Year %% 2 != 0] = predBOdd
  MSE <- mean((predictCol-test$Earnings)^2)
  all_errors <- c(all_errors, MSE)
}
print(all_errors)
print(mean(all_errors))


#Final Models
other <- data[data$Major == "Other",]
other$squared <- other$Number_Of_Professional_Connections^2
bEven <- data[data$Major == "Buisness" & data$Graduation_Year %% 2 == 0,]
bOdd <- data[data$Major == "Buisness" & data$Graduation_Year %% 2 != 0,]
s = data[data$Major == "STEM",]
h = data[data$Major == "Humanities",]
p = data[data$Major == "Professional",]
v = data[data$Major == "Vocational",]

model.lm <- lm(Earnings~ Number_Of_Professional_Connectionssquared, data = other)
model.bEven <- lm(Earnings~ GPA, data = bEven)
model.bOdd <- lm(Earnings~ GPA, data = bOdd)
model.slm <- lm(Earnings~ GPA, data = s)
model.hlm <- lm(Earnings~ GPA, data = h)
model.plm <- lm(Earnings~ GPA, data = p)
model.vlm <- lm(Earnings~ GPA, data = v)

#Predict on Final Data
test <- read.csv('Earnings_Test_Students-1.csv')

other <- test[test$Major == "Other",]
other$squared <- other$Number_Of_Professional_Connections^2
bEven <- test[test$Major == "Buisness" & test$Graduation_Year %% 2 == 0,]
bOdd <- test[test$Major == "Buisness" & test$Graduation_Year %% 2 != 0,]
s = test[test$Major == "STEM",]
h = test[test$Major == "Humanities",]
p = test[test$Major == "Professional",]
v = test[test$Major == "Vocational",]

predBEven =  predict(model.bEven, newdata = bEven)
predBOdd =  predict(model.bOdd, newdata = bOdd)
predOther =  predict(model.lm, newdata = other)
predS =  predict(model.slm, newdata = s)
predH =  predict(model.hlm, newdata = h)
predP =  predict(model.plm, newdata = p)
predV =  predict(model.vlm, newdata = v)

predictCol <- rep(0,nrow(data))
predictCol[data$Major == "Professional"] = predP
predictCol[data$Major == "STEM"] = predS
predictCol[data$Major == "Vocational"] = predV
predictCol[data$Major == "Other"] = predOther
predictCol[data$Major == "Buisness" & data$Graduation_Year %% 2 == 0] = predBEven
predictCol[data$Major == "Buisness" & data$Graduation_Year %% 2 == 0] = predBOdd

#sub <- read.csv('earning_submisson.csv')
#sub$Earnings <- predictCol
#write.csv(sub, file = "mysubmission.csv",row.names=FALSE)