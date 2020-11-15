library('e1071')
library('DescTools')

train_data <- read.csv('data/train.csv', stringsAsFactors = T, na.strings = c('NA', ''))
test_data <- read.csv('data/test.csv', stringsAsFactors = T, na.strings = c('NA', ''))

colSums(is.na(train_data))
colSums(is.na(test_data))

test_ids <- test_data$PassengerId

train_data <- subset(train_data, select = -c(PassengerId, Name, Cabin, Ticket, SibSp, Parch))
test_data <- subset(test_data, select = -c(PassengerId, Name, Cabin, Ticket, SibSp, Parch))
train_data$Survived <- as.factor(train_data$Survived)


fill_age_na <- function(some_data) {
  some_data$Age[is.na(some_data$Age)] <- median(some_data$Age, na.rm = T)
  return(some_data)
}


fill_fare_na <- function(some_data) {
  some_data$Fare[is.na(some_data$Fare)] <- median(some_data$Fare, na.rm = T)
  return(some_data)
}

train_data <- fill_age_na(train_data)
test_data <- fill_age_na(test_data)
test_data <- fill_fare_na(test_data)
train_data$Embarked[is.na(train_data$Embarked)] <- Mode(train_data$Embarked, na.rm = T)

bayes_clsfr <- naiveBayes(Survived ~ ., 
                          data = train_data)

predicted <- predict(bayes_clsfr, test_data)

result <- data.frame(PassengerId = test_ids, Survived = as.integer(predicted))
write.csv(result,'output/submission.csv', row.names=F)
