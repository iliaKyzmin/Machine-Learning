library(DAAG)
library(e1071)
library(tree)
library(maptree)
library(Metrics)

data(nsw74psid1)

set.seed(12345)
all_data <- nsw74psid1[order(runif(nrow(nsw74psid1))), ]
nt <- as.integer(nrow(all_data) * 0.8)
train_data <- all_data[1:nt, ]
test_data <- all_data[(nt + 1):nrow(all_data), ]

tree_model <- tree(re78 ~ ., data = train_data)
svm_model <- svm(re78 ~ . , 
                 data = train_data, 
                 type = "eps-regression")
reg_model <- lm(re78 ~ ., train_data)


count_error <- function(model, test_data) {
  predicted <- predict(model, test_data)
  return(mse(predicted, test_data$re78))
}

draw.tree(tree_model, cex = 0.7)

print(paste0('Tree error - ', count_error(tree_model, test_data)))
print(paste0('SVM error - ', count_error(svm_model, test_data)))
print(paste0('Regression error - ', count_error(reg_model, test_data)))
