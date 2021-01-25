library(adabag)
library(mlbench)

data("Vehicle")
all_data <- Vehicle

all_data <- all_data[order(runif(nrow(all_data))), ]
nt <- as.integer(nrow(all_data) * 0.7)
train_data <- all_data[1:nt, ]
test_data <- all_data[(nt + 1):nrow(all_data), ]

create_model_and_get_error <- function(i) {
  model <- boosting(Class ~ ., data = train_data, mfinal = i, maxdepth = 5)
  predicted <- predict.boosting(model, test_data)
  return(predicted$error)
}

trees_num <- seq(1, 301, 10)
errors <- sapply(trees_num, create_model_and_get_error)

plot(trees_num, errors, xlab = 'Number of trees', ylab= 'Error')
