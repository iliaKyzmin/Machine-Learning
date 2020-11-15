library('e1071')
library('DescTools')

train_data <- read.csv('data/train.csv', stringsAsFactors = T, na.strings = c('NA', ''))
test_data <- read.csv('data/test.csv', stringsAsFactors = T, na.strings = c('NA', ''))

colSums(is.na(train_data))
colSums(is.na(test_data))

test_ids <- test_data$PassengerId

train_data <- subset(train_data, select = -c(PassengerId, Name, Cabin, Ticket, SibSp, Parch, Fare, Embarked))
test_data <- subset(test_data, select = -c(PassengerId, Name, Cabin, Ticket, SibSp, Parch, Fare, Embarked))
train_data$Survived <- as.factor(train_data$Survived)


fill_age_na <- function(some_data) {
  some_data$Age[is.na(some_data$Age)] <- mean(some_data$Age, na.rm = T)
  return(some_data)
}

train_data <- fill_age_na(train_data)
test_data <- fill_age_na(test_data)

bayes_clsfr <- naiveBayes(Survived ~ ., 
                          data = train_data)

predicted <- predict(bayes_clsfr, test_data)

result <- data.frame(PassengerId = test_ids, Survived = as.integer(as.character(predicted)))
write.csv(result,'output/submission.csv', row.names=F)
