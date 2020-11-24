library(e1071)

train_data <- read.table('data/svmdata2.txt', sep = '\t', header = T, stringsAsFactors = T)
test_data <- read.table('data/svmdata2test.txt', sep = '\t', header = T, stringsAsFactors = T)


find_c_with_zero_error <- function(data_train, data_test) {
  error <- 1.
  eps <- 1e-10
  cost <- 1
  
  while (error > eps) {
    svm_clsfr <- svm(Colors ~ ., 
                     data = data_train,
                     type = "C-classification",
                     cost = cost,
                     kernel = "linear")
    pred <- predict(svm_clsfr, data_test)
    tbl <- table(pred, data_test$Color)
    
    error <- 1. - sum(diag(tbl)) / nrow(data_test)
    cost <- cost + 1
  }
  return(cost - 1)
}

print(paste("Train minimal C - ", find_c_with_zero_error(train_data, train_data)))
print(paste("Test minimal C - ", find_c_with_zero_error(train_data, test_data)))
