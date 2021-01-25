library(kknn)

all_data <- read.table('data/pima-indians-diabetes.data', sep = ',')
all_data$V9 <- as.factor(all_data$V9)


train_knn <- function(distance) {
  model <- train.kknn(V9 ~ ., data = all_data, kmax = 50, 
                      kernel = c("biweight", "triangular", "triweight", "cos", "inv", "gaussian",
                                 "optimal", "rectangular", "rank", "epanechnikov"),
                      distance = distance)
  
  b_p = model[["best.parameters"]]
  print(model[["MISCLASS"]][b_p$k, b_p$kernel])
  return(b_p)
}

best_params <- sapply(1:5, train_knn)

model <- train.kknn(V9 ~ ., data = all_data, kmax = 50, 
                    kernel = c("inv", "rectangular", "triweight", "cos", 
                               "gaussian", "optimal"),
                    distance = 1)
plot(model)

set.seed(12345)

all_data <- all_data[order(runif(nrow(all_data))), ]
nt <- as.integer(nrow(all_data) * 0.8)
train_data <- all_data[1:nt, ]
test_data <- all_data[(nt + 1):nrow(all_data), ]

model <- kknn(V9 ~ ., train_data, test_data, k = 27, kernel = 'inv', distance = 1)
predicted <- fitted(model)
print(1 - sum(diag(table(predicted, test_data$V9))) / nrow(test_data))