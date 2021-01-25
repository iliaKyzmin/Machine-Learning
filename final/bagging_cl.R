library(adabag)

set.seed(12345)

all_data <- read.table('data/pima-indians-diabetes.data', sep = ',')
all_data$V9 <- as.factor(all_data$V9)

all_data <- all_data[order(runif(nrow(all_data))), ]
nt <- as.integer(nrow(all_data) * 0.7)
train_data <- all_data[1:nt, ]
test_data <- all_data[(nt + 1):nrow(all_data), ]



get_errors <- function(params) {
  errors <- numeric(nrow(params))
  for (i in 1:nrow(params)) {
      model <- bagging(V9 ~ ., data = train_data, 
                       mfinal = params$mfinal[i], 
                       maxdepth = params$maxdepth[i])
      predicted <- predict.bagging(model, test_data)
      errors[i] <- predicted$error
  }
  return(errors)
}

params <- expand.grid(mfinal = seq(25, 500, 25),
                      maxdepth = c(5, 10, 15, 20, 25, 30))
errors <- get_errors(params)
print(params[which.min(errors), ])