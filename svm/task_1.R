library(e1071)

train_data <- read.table('data/svmdata1.txt', sep = '\t', header = T, stringsAsFactors = T)
test_data <- read.table('data/svmdata1test.txt', sep = '\t', header = T, stringsAsFactors = T)

svm_clsfr <- svm(Color ~ ., 
                 data = train_data,
                 type = "C-classification",
                 cost = 1,
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

print(paste("Support vectors number: ", sum(svm_clsfr$nSV)))

pred_train <- predict(svm_clsfr, train_data)
tbl_train <- table(pred_train, train_data$Color)
print(paste("Train error: ", 1. - sum(diag(tbl_train)) / nrow(train_data)))

pred_test <- predict(svm_clsfr, test_data)
tbl_test <- table(pred_test, test_data$Color)
print(paste("Test error: ", 1. - sum(diag(tbl_test)) / nrow(test_data)))
