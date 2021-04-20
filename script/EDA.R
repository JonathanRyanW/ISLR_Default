library(ISLR)
data <- Default

#Checking for NAs
sapply(data, function(x){sum(is.na(x))})
"There is no NA in the data."

#How many people defaulted?
sum(default$default == "Yes")
sum(default$default == "Yes" & default$student == "Yes")
sum(default$default == "Yes" & default$student == "No")

"333 out of 10000 people defaulted (3.33%). Out of those 333 people 127 are
students and 206 are students"

#What is the probability of default based on student status?
sum(default$student == "Yes")
sum(default$student == "No")

"2944 people are students and 7056 are not students. Which means that the
default probability is 127/2944 = 4.31% for students and 206/7056 = 2.91% for
non-students. Students have larger probability of being defaulted provided that
we do not know their income or credit card balance."

stud <- c("Yes", "Yes", "No", "No")
def <- c("Yes", "No", "Yes", "No")
probability <- NULL

library(dplyr)

for (i in c(1:4)){
  probability <- append(probability,
                        mean(filter(data, student == stud[i])$default == def[i]))
}

write.csv(data.frame(Student = stud, Default = def, Chance = round(probability*100,2)),
          file = "probabilities.csv")
rm(def, stud, probability, i)

#Finding the correlation between income and balance
cor(default$balance, default$income)
"The correlation between the balance and income is -0.1522434. This is a weak
correlation"

#Plotting balance and income
library(ggplot2)
ggplot(data) +
  geom_point(aes(x = balance, y = income, col = default)) +
  theme_bw()

#Building box plots between balance and student
ggplot(data, aes(y = balance, x = student)) +
  geom_boxplot(aes(fill = student)) +
  ylab("Credit Card Balance ($)") +
  xlab("Student?") +
  theme_bw()

"This plot shows that students have more credit card balance than non-students."

#Building box plots between income and student
ggplot(data, aes(y = income, x = student)) +
  geom_boxplot(aes(fill = student)) +
  ylab("Income ($)") +
  xlab("Student?") +
  theme_bw()

"This plot shows that students generally have lower income than non-students.
This is very logical since students are most likely not yet working or only
work part time to get some extra cash."

#Building box plots between income and default
ggplot(data, aes(y = income, x = default)) +
  geom_boxplot(aes(fill = default)) +
  ylab("Income ($)") +
  xlab("Default?") +
  theme_bw()
"There seems to be no connection between the income of a person and the
probability that they defaulted. At least we can't see any difference most of
the time"

#Building box plots between balance and default
ggplot(data, aes(y = balance, x = default)) +
  geom_boxplot(aes(fill = default)) +
  ylab("Credit Card Balance ($)") +
  xlab("Default?") +
  theme_bw()
"So people who defaulted have in general bigger CC balance"