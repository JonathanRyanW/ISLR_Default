library(ISLR)
data <- Default

#Taking only the students data (7056 observations)
library(dplyr)
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
write.csv(error.rate, file = "NonStudent Error Rate.csv")

rm(i,k,prediction,error)

library(ggplot2)
ggplot(error.rate, aes(k, error)) +
  geom_line() +
  ggtitle("Non Student K and Error Rate") +
  theme_bw()

"We can see that the best k value is k = 7 with error rate of 2.83%. This value
is only slightly better than other k values. It could be that
this error rate is due to simply luck."

#Proceeding with k = 7
prediction <- knn(train, test, train.default, k = 7)
nonstudent.result <- data.frame(cbind(Default = test.default, Predicted = prediction))

attach(nonstudent.result)

#Changing the values to be more describing
mean(Default == Predicted)

for (i in c(1:2117)){
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
sum(Default == Predicted)
sum(Default != Predicted)

"The KNN was correct 2061 times out of 2117 attempts (97.35% accuracy)"

confusion.nonstudent <- matrix(nrow = 2, ncol = 2)
row.names(confusion.nonstudent) <- c("Predicted: Yes", "Predicted: No")
colnames(confusion.nonstudent) <- c("Actual: Yes", "Actual: No")

attach(nonstudent.result)
confusion.nonstudent[1,1] <- sum(Default == "Yes" & Predicted == "Yes")
confusion.nonstudent[1,2] <- sum(Default == "No" & Predicted == "Yes")
confusion.nonstudent[2,1] <- sum(Default == "Yes" & Predicted == "No")
confusion.nonstudent[2,2] <- sum(Default == "No" & Predicted == "No")

detach()
detach()

confusion.nonstudent <- as.data.frame(confusion.nonstudent)
write.csv(confusion.nonstudent, file = "Confusion Matrix - NonStudent.csv")