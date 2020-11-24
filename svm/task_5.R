library(e1071)

train_data <- read.table('data/svmdata5.txt', sep = '\t', header = T, stringsAsFactors = T)
test_data <- read.table('data/svmdata5test.txt', sep = '\t', header = T, stringsAsFactors = T)

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

get_radial_error <- function(gamma) {
  svm_clsfr <- svm(Colors ~ ., 
                   data = train_data,
                   type = "C-classification",
                   cost = 1,
                   kernel = "radial",
                   gamma = gamma)
  
  pred_test <- predict(svm_clsfr, test_data)
  tbl_test <- table(pred_test, test_data$Color)
  
  pred_train <- predict(svm_clsfr, train_data)
  tbl_train <- table(pred_train, train_data$Color)
  return( c(1. - sum(diag(tbl_train)) / nrow(train_data), 1. - sum(diag(tbl_test)) / nrow(test_data)) )
}

get_formatted <- function(dec, prec = 3) {
  format(round(dec, prec), nsmall = prec)
}

for (gamma in seq(1, 100, 1)) {
  error <- get_radial_error(gamma)
  print(paste('Gamma ', get_formatted(gamma, 2), 
              ' - Train: ', get_formatted(error[1]), 
              '   Test: ', get_formatted(error[2])))
}


svm_clsfr_overfitted <- svm(Colors ~ ., 
                            data = train_data,
                            type = "C-classification",
                            cost = 1,
                            kernel = "radial",
                            gamma = 94)

area_pallete <- function(n = 3) {
  colors <- rainbow(n)
  colors[1:2] <- c("#e2fee2", "pink")
  return(colors) 
}

plot(svm_clsfr_overfitted, 
     train_data,
     grid = 250,
     symbolPalette = c("darkgreen", "red"),
     color.palette = area_pallete)


svm_clsfr_ok <- svm(Colors ~ ., 
                    data = train_data,
                    type = "C-classification",
                    cost = 1,
                    kernel = "radial",
                    gamma = 1)

plot(svm_clsfr_ok, 
     train_data,
     grid = 250,
     symbolPalette = c("darkgreen", "red"),
     color.palette = area_pallete)
