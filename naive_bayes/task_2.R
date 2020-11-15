library('e1071')

set.seed(12345)

x1_first  <- rnorm(50, mean = 10, sd = 4)
x1_last   <- rnorm(50, mean = 20, sd = 3)
x2_first  <- rnorm(50, mean = 14, sd = 4)
x2_last   <- rnorm(50, mean = 18, sd = 3)

par(mfrow = c(2, 2))
hist(x1_first, freq = F, 
     xlab = 'X1, first 50',
     main = 'Histogram of X1, first 50')
hist(x1_last, freq = F, 
     xlab = 'X1, last 50',
     main = 'Histogram of X1, last 50')
hist(x2_first, freq = F, 
     xlab = 'X2, first 50',
     main = 'Histogram of X2, first 50')
hist(x2_last, freq = F, 
     xlab = 'X2, last 50',
     main = 'Histogram of X2, last 50')

all_data <- data.frame(X1 = c(x1_first, x1_last), 
                       X2 = c(x2_first, x2_last), 
                       class = c(rep('-1', 50), rep('1', 50)), 
                       stringsAsFactors = T)

rnd_data <- all_data[order(runif(nrow(all_data))), ]
nt <- as.integer(nrow(rnd_data) * 0.8)
train_data <- rnd_data[1:nt, ]
test_data <- rnd_data[(nt + 1):nrow(rnd_data), ]

bayes_clsfr <- naiveBayes(class ~ ., 
                          data = train_data)
predicted <- predict(bayes_clsfr, test_data)
tbl <- table(predicted, test_data$class)
(tbl[1, 1] + tbl[2, 2]) / nrow(test_data)
