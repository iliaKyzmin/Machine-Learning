library(e1071)

train_data <- read.table('data/svmdata3.txt', sep = '\t', header = T, stringsAsFactors = T)
test_data <- read.table('data/svmdata3test.txt', sep = '\t', header = T, stringsAsFactors = T)

find_optimal_kernel <- function(kernel) {
  svm_clsfr <- svm(Colors ~ ., 
                   data = train_data,
                   type = "C-classification",
                   cost = 1,
                   kernel = kernel)
  
    pred <- predict(svm_clsfr, test_data)
    tbl <- table(pred, test_data$Color)
    return(1. - sum(diag(tbl)) / nrow(test_data))
}

print(paste("Polynomial - ", find_optimal_kernel("polynomial")))
print(paste("Radial - ", find_optimal_kernel("radial")))
print(paste("Sigmoid - ", find_optimal_kernel("sigmoid")))


get_pol_error <- function(degree) {
  svm_clsfr <- svm(Colors ~ ., 
                   data = train_data,
                   type = "C-classification",
                   cost = 1,
                   kernel = "polynomial",
                   degree = degree)
  
  pred <- predict(svm_clsfr, test_data)
  tbl <- table(pred, test_data$Color)
  return(1. - sum(diag(tbl)) / nrow(test_data))
}

for (deg in 1:20) {
  print(paste('Degree ', deg, ' - ', get_pol_error(deg)))
}
