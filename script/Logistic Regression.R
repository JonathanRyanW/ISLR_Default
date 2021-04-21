library(ISLR)
data <- Default

#Splitting the data into train and test data
library(caTools)
set.seed(1001)
sample <- sample.split(data$default, SplitRatio = 0.7)

train <- data[sample,]
test <- data[!sample,]
rm(sample)

model <- glm(default ~ ., family = "binomial"(link = "logit"), data = train)
summary(model)

"Based on the summary income doesnt predict default at all. The p value is very
large (0.8039). We can see this in the scatter plot between income and balance
with points colored by the default category. We can see that as balance increase
people do become more likely to default while changes in income doesnt seem to
affect default probability"

"Another interesting property is that the coefficient for the studentYes is
negative which means that if someone is a student the less likely they are to
default. However while doing our EDA we noticed that the probability for a
student to default is actually higher than non-student. Provided that we do not
know their income. There is no paradox. This phenomenon is called confounding.
These 2 finds means: If we do not have any information about
a student and a non-student, chances are the student is the one who is more
likely to default. But if we know that their level of income and credit card
balance is the same then the non-student is the one who is more likely to
default.

This happens because there is a correlation between student variable and balance
variable. We know that the higher the balance value the more likely someone will
default. But we also know that students are more likely to have higher balance!
This means that even though students are less likely to default than a non student
with the same income and balance, overall students are more likely to default
because they tend to have higher balance! This can be seen in the boxplot
between balance and student variable."

prediction <- predict(model, test[,2:4], type = "response")

for (i in c(1:3000)){
  if (prediction[i] < 0.5){
    prediction[i] <- 1
  } else{
    prediction[i] <- 2
  }
}
rm(i)

result <- data.frame(cbind(Default = test$default, Prediction = prediction))
rm(prediction)

sum(result$Default == result$Prediction)
mean(result$Default == result$Prediction)

"Our Logistic model successfully predicted the correct default category 97.63%
of the time (2929 out of 3000 attempts). The KNN model in contrast successfully
predicted default category 847 + 2057 = 2904 out of 3000 attempts
(97% accuracy). We can see that the logistic model only beat the KNN by a
meager 0.63%. This is nothing at all."

confusion.logistic <- matrix(nrow = 2, ncol = 2)
row.names(confusion.logistic) <- c("Predicted: Yes", "Predicted: No")
colnames(confusion.logistic) <- c("Actual: Yes", "Actual: No")

attach(result)
confusion.logistic[1,1] <- sum(Default == 2 & Prediction == 2)
confusion.logistic[1,2] <- sum(Default == 1 & Prediction == 2)
confusion.logistic[2,1] <- sum(Default == 2 & Prediction == 1)
confusion.logistic[2,2] <- sum(Default == 1 & Prediction == 1)

detach()

confusion.logistic <- as.data.frame(confusion.logistic)
write.csv(confusion.logistic, file = "Confusion Matrix - NonStudent.csv")