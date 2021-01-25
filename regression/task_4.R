library('MASS')
library('Metrics')
library('datasets')

reg_data <- longley
f = lm(Employed ~ ., data = reg_data)
summary(f)$r.squared

# delete Population feature
reg_data <- reg_data[, -5]

set.seed(12345)
# randomize and split data into test and train parts
reg_data <- reg_data[order(runif(nrow(reg_data))), ]

nt <- as.integer(nrow(reg_data) * 0.5)
train_data <- reg_data[1:nt, ]
test_data <- reg_data[(nt + 1):nrow(reg_data), ]

lambdas <- sapply(0:25, function (i) {10^(-3 + 0.2 * i)})
error_train <- numeric(26)
error_test <- numeric(26)

for (i in 0:25) {
  f <- lm.ridge(Employed ~ ., data = train_data, lambda = lambdas[i + 1])
  y_train <- as.matrix(cbind(const=1, train_data[, -6])) %*% coef(f)
  y_test <- as.matrix(cbind(const=1, test_data[, -6])) %*% coef(f)
  error_train[i + 1] <- mse(y_train, train_data$Employed)
  error_test[i + 1] <- mse(y_test, test_data$Employed)
}

plot(lambdas, error_train, 'o', xlab = 'lambda', ylab = 'train error')
plot(lambdas, error_test, 'o', xlab = 'lambda', ylab = 'test error')
