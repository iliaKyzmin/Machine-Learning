library(e1071)
library(MLmetrics)

train_data <- read.table('data/svmdata6.txt', sep = '\t', header = T, stringsAsFactors = T)

get_error <- function(eps) {
  svm_model <- svm(train_data$X, 
                   train_data$Y, 
                   type = "eps-regression", 
                   cost = 1, 
                   kernel = "radial", 
                   epsilon = eps,
                   cross = 1)
  preds <- predict(svm_model, train_data$X)
  
  points(train_data$X[svm_model$index], train_data$Y[svm_model$index], col = "red")
  lines(train_data$X, preds, col = "dodgerblue", lwd = 2) 
  lines(train_data$X, preds + svm_model$epsilon, col = "cyan")
  lines(train_data$X, preds - svm_model$epsilon, col = "cyan")
  
  return(MSE(preds, train_data$Y))
}

epsilons <- seq(0.0, 0.4, by = 0.02)
errors <- sapply(epsilons, get_error)

plot(epsilons, errors, xlab = 'eps', ylab = 'error', 'l')


plot_model <- function(eps) {
  svm_model <- svm(train_data$X, 
                   train_data$Y, 
                   type = "eps-regression", 
                   cost = 1, 
                   kernel = "radial", 
                   epsilon = eps,
                   cross = 1)
  
  plot(train_data$X, train_data$Y)
  preds <- predict(svm_model, train_data$X)
  
  points(train_data$X[svm_model$index], train_data$Y[svm_model$index], col = "red")
  lines(train_data$X, preds, col = "dodgerblue", lwd = 2) 
  lines(train_data$X, preds + svm_model$epsilon, col = "cyan")
  lines(train_data$X, preds - svm_model$epsilon, col = "cyan")
}

plot_model(0.08)
plot_model(0.4)
