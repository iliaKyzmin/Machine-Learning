library(tree)
library(maptree)
library(DescTools)

train_data <- read.csv('data/train.csv', stringsAsFactors = T, na.strings = c('NA', ''))
test_data <- read.csv('data/test.csv', stringsAsFactors = T, na.strings = c('NA', ''))

colSums(is.na(train_data))
colSums(is.na(test_data))

test_ids <- test_data$PassengerId

train_data <- subset(train_data, select = -c(PassengerId, Name, Cabin, Ticket))
test_data <- subset(test_data, select = -c(PassengerId, Name, Cabin, Ticket))
train_data$Survived <- factor(train_data$Survived)

fill_age_na <- function(some_data) {
  ages_nas <- is.na(some_data$Age)
  
  for (ind in 1:length(ages_nas)) {
    if (!ages_nas[ind]) next
    p_class <- some_data[ind, ]$Pclass
    sex <- some_data[ind, ]$Sex
    med <- median(some_data[some_data$Pclass == p_class & some_data$Sex == sex & !ages_nas, ]$Age)
    some_data[ind, ]$Age <- med
  }
  
  return(some_data)
}


fill_fare_na <- function(some_data) {
  fares_nas <- is.na(some_data$Fare)
  
  for (ind in 1:length(fares_nas)) {
    if (!fares_nas[ind]) next
    p_class <- some_data[ind, ]$Pclass
    med <- median(some_data[some_data$Pclass == p_class & !fares_nas, ]$Fare)
    some_data[ind, ]$Fare <- med
  }
  
  return(some_data)
}

train_data <- fill_age_na(train_data)
test_data <- fill_age_na(test_data)
test_data <- fill_fare_na(test_data)
train_data$Embarked[is.na(train_data$Embarked)] <- Mode(train_data$Embarked, na.rm = T)


model <- tree(Survived ~ ., data = train_data)
predicted <- apply(predict(model, test_data), 1, function(row) {which.max(row) - 1})
draw.tree(model, cex = 0.7)

result <- data.frame(PassengerId = test_ids, Survived = predicted)
write.csv(result,'output/submission.csv', row.names=FALSE)
