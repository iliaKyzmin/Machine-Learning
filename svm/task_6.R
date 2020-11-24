library(e1071)
library(MLmetrics)

train_data <- read.table('data/svmdata6.txt', sep = '\t', header = T, stringsAsFactors = T)

get_error <- function(eps) {
  svmModel <- svm(train_data$X, 
                  train_data$Y, 
                  type = "eps-regression", 
                  cost = 1, 
                  kernel = "radial", 
                  epsilon = eps,
                  cross = 1)
  preds <- predict(svmModel, train_data$X)
  return(MSE(preds, train_data$Y))
}

epsilons <- seq(0.05, 1.5, by = 0.05)
errors <- sapply(epsilons, get_error)

plot(epsilons, errors, xlab = 'eps', ylab = 'error')