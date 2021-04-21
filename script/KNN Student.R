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
train <- data.normal[sample,]
test <- data.normal[!sample,]

train.default <- data[sample,1]
test.default <- data[!sample,1]

rm(sample)

#Finding the best k value
error <- NULL
library(class)
for (i in c(1:20)){
  prediction <- knn(train, test, train.default, k = i)
  error <- append(error, mean(prediction != test.default))
}

#Plotting the error rate
k <- c(1:20)
error.rate <- data.frame(cbind(k,error))
write.csv(error.rate, file = "Student Error Rate.csv")

rm(i,k,prediction,error)

library(ggplot2)
ggplot(error.rate, aes(k, error)) +
  geom_line() +
  ggtitle("Student K and Error Rate") +
  theme_bw()

"We can see that the best k value is k = 7"

#Proceeding with k = 7
prediction <- knn(train, test, train.default, k = 7)
student.result <- data.frame(cbind(Default = test.default, Predicted = prediction))

attach(student.result)
#Changing the values to be more describing
mean(Default == Predicted)
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
sum(Default == Predicted)
sum(Default != Predicted)

"The KNN was correct 849 times out of 883 attempts (96.14% accuracy)"

confusion.student <- matrix(nrow = 2, ncol = 2)
row.names(confusion.student) <- c("Predicted: Yes", "Predicted: No")
colnames(confusion.student) <- c("Actual: Yes", "Actual: No")
                                 
attach(student.result)
confusion.student[1,1] <- sum(Default == "Yes" & "Yes" == Predicted)
confusion.student[1,2] <- sum(Default == "No" & "Yes" == Predicted)
confusion.student[2,1] <- sum(Default == "Yes" & "No" == Predicted)
confusion.student[2,2] <- sum(Default == "No" & "No" == Predicted)

detach()
detach()

confusion.student <- as.data.frame(confusion.student)
write.csv(confusion.student, file = "Confusion Matrix - Student.csv")
