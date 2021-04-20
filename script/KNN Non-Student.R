library(ISLR)
data <- Default

library(dplyr)
#Taking only the students data (7056 observations)
data <- filter(data, student == "No")

#deleting the student column
data <- select(data, -student)

#Standardizing the data
data.normal <- scale(data[,-1])

#Splitting the data into train data and test data
library(caTools)
set.seed(1001)
sample <- sample.split(data$default, SplitRatio = 0.7)

"Since we allocate 70% of the data into the train dataset, it will contain
7056 * 0.7 = 4939.2 observations. Rounded to 4939. The test dataset will contain
7056 - 4939 = 2117 observations"
train <- data[sample,]
test <- data[!sample,]

rm(sample)

#Finding the best k value
error <- NULL
library(class)
for (i in c(1:20)){
  prediction <- knn(train[,2:3], test[,2:3], train$default, k = i)
  error <- append(error, mean(prediction != test$default))
}

#Plotting the error rate
k <- c(1:20)
error.rate <- data.frame(cbind(k,error))

rm(i,k,prediction,error)

ggplot(error.rate, aes(k, error)) +
  geom_line() +
  ggtitle("Non Student K and Error Rate") +
  theme_bw()

"We can see that the best k value is k = 7 with error rate of 2.83%. This value
is only slightly better than other k values. It could be that
this error rate is due to simply luck."

#Proceeding with k = 7
prediction <- knn(train[,2:3], test[,2:3], train$default, k = 7)
nonstudent.result <- data.frame(cbind(Default = test$default, Predicted = prediction))

#Changing the values to be more describing
mean(nonstudent.result$Default == nonstudent.result$Predicted)
for (i in c(1:883)){
  for (j in c(1:2)){
    if (nonstudent.result[i,j] == 1){
      nonstudent.result[i,j] <- "No"
    } else{
      nonstudent.result[i,j] <- "Yes"
    }
  }
}

rm(i,j, prediction)

#How many times do KNN get it right?
sum(nonstudent.result$Default == nonstudent.result$Predicted)
sum(nonstudent.result$Default != nonstudent.result$Predicted)

"The KNN was correct 2057 times out of 2117 attempts (97.16% accuracy)"