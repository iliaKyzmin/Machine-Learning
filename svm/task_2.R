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
    tbl <- table(pred, data_test$Colors)
    
    error <- 1. - sum(diag(tbl)) / nrow(data_test)
    cost <- cost + 1
  }
  return(cost - 1)
}

c_train_zero <- find_c_with_zero_error(train_data, train_data)
c_test_zero <- find_c_with_zero_error(train_data, test_data)
print(paste("Train minimal C - ", c_train_zero))
print(paste("Test minimal C - ", c_test_zero))



plot_clsfr <- function(cost) {
  svm_clsfr <- svm(Colors ~ ., 
                   data = train_data,
                   type = "C-classification",
                   cost = cost,
                   kernel = "linear")
  
  area_pallete <- function(n = 3) {
    colors <- rainbow(n)
    colors[1:2] <- c("#e2fee2", "pink")
    return(colors) 
  }
  
  plot(svm_clsfr, 
       train_data,
       grid = 250,
       symbolPalette = c("darkgreen", "red"),
       color.palette = area_pallete)
}


plot_clsfr(c_train_zero)
plot_clsfr(c_test_zero)
