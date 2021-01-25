library(adabag)
library(mlbench)

data("Glass")
all_data <- Glass

all_data <- all_data[order(runif(nrow(all_data))), ]
nt <- as.integer(nrow(all_data) * 0.7)
train_data <- all_data[1:nt, ]
test_data <- all_data[(nt + 1):nrow(all_data), ]

create_model_and_get_error <- function(i) {
  model <- bagging(Type ~ ., data = train_data, mfinal = i, maxdepth = 5)
  predicted_test <- predict.bagging(model, test_data)
  predicted_train <- predict.bagging(model, train_data)
  return(list(predicted_test$error, predicted_train$error))
}

trees_num <- seq(1, 201, 10)
errors <- lapply(trees_num, create_model_and_get_error)

error_test <- sapply(errors, function(elem) elem[[1]])
error_train <- sapply(errors, function(elem) elem[[2]])

plot(trees_num, error_test, 
     xlab = 'Number of trees', ylab= 'Error', 
     col = 'blue', ylim = c(0.05, 0.5))
points(trees_num, error_train, col = 'red')
legend("topright", 
       legend=c("test", "train"),
       col=c("blue", "red"),
       pch=c(1, 1))