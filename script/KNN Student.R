library(ISLR)
data <- Default

library(dplyr)
#Taking only the students data (2944 observations)
data <- filter(data, student == "Yes")

#deleting the student column
data <- select(data, -student)

#Standardizing the data
data.normal <- scale(data[,-1])

#Splitting the data into train data and test data
library(caTools)
set.seed(1001)
sample <- sample.split(data$default, SplitRatio = 0.7)

"Since we allocate 70% of the data into the train dataset, it will contain
2944 * 0.7 = 2060.8 observations. Rounded to 2061. The test dataset will contain
2944 - 2061 = 883 observations"
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
  ggtitle("Student K and Error Rate") +
  theme_bw()

"We can see that the best k value is k = 14 with error rate of 3.96%. This value
is only slightly better than k = 8, 11, 12, 13, 15, 16, etc. It could be that
this error rate is due to simply luck."

#Proceeding with k = 14
prediction <- knn(train[,2:3], test[,2:3], train$default, k = 14)
student.result <- data.frame(cbind(Default = test$default, Predicted = prediction))

#Changing the values to be more describing
mean(student.result$Default == student.result$Predicted)
for (i in c(1:883)){
  for (j in c(1:2)){
    if (student.result[i,j] == 1){
      student.result[i,j] <- "No"
    } else{
      student.result[i,j] <- "Yes"
    }
  }
}

rm(i,j, prediction)

#How many times do KNN get it right?
sum(student.result$Default == student.result$Predicted)
sum(student.result$Default != student.result$Predicted)

"The KNN was correct 847 times out of 883 attempts (95.92% accuracy)"