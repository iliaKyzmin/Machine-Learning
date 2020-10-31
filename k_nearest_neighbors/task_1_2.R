library('kknn')
library('kernlab')

set.seed(100)

get_accuracy <- function(train_data, all_test_data, clss, perc) {
  rnd_data <- all_test_data[order(runif(nrow(all_test_data))), ]
  
  nt <- as.integer(nrow(rnd_data) * perc)
  test_data <- rnd_data[1:nt, ]
  
  knn_fit <- fitted(kknn(as.formula(paste(clss, '~ .')), train_data, 
                         test_data, k = 13, kernel = "triangular", distance = 2))
  
  tbl <- table(test_data[[clss]], knn_fit)
  (tbl[1] + tbl[4]) / nrow(test_data)
}


get_mean_accuracy <- function(train_data, all_test_data, clss, perc, times = 10) {
  mean(sapply(1:times, function(time) get_accuracy(train_data, all_test_data, clss, perc)))
}


plot_test_sample_accuracy <- function(all_data, train_data_size, clss) {
  all_data <- all_data[order(runif(nrow(all_data))), ]
  
  # split into test and train 
  train_data <- all_data[1:train_data_size, ]
  
  all_test_data <- all_data[(train_data_size + 1):nrow(all_data), ]
  
  proportion <- seq(0.1, 1.0, by = 0.1)
  accuracy <- sapply(proportion, function(perc) get_mean_accuracy(train_data, all_test_data, clss, perc))
  
  plot(proportion * nrow(all_test_data), accuracy, type = 'l', xlab = 'test sample size', ylab = 'accuracy')
}

ttt_data <- read.table('data/Tic_tac_toe.txt', sep = ',', stringsAsFactors = T)
data(spam)
plot_test_sample_accuracy(ttt_data, 500, 'V10')
plot_test_sample_accuracy(spam, 3000, 'type')
