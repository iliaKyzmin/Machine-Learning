library('kknn')
library('kernlab')

set.seed(100)

get_accuracy <- function(all_train_data, test_data, clss, perc) {
  rnd_data <- all_train_data[order(runif(nrow(all_train_data))), ]

  nt <- as.integer(nrow(rnd_data) * perc)
  train_data <- rnd_data[1:nt, ]
  
  knn_clsfr <- train.kknn(as.formula(paste(clss, '~ .')), train_data, kmax = 15, 
                          kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), 
                          distance = 1)
  
  tbl <- table(predict(knn_clsfr, test_data), test_data[[clss]])
  (tbl[1] + tbl[4]) / nrow(test_data)
}


get_mean_accuracy <- function(all_train_data, test_data, clss, perc, times = 10) {
  mean(sapply(1:times, function(time) get_accuracy(all_train_data, test_data, clss, perc)))
}


plot_train_sample_accuracy <- function(all_data, clss) {
  all_data <- all_data[order(runif(nrow(all_data))), ]
  
  # split into test and train 
  nt_init <- as.integer(nrow(all_data) * 0.8)
  all_train_data <- all_data[1:nt_init, ]
  test_data <- all_data[(nt_init + 1):nrow(all_data), ]
  
  proportion <- seq(0.1, 1.0, by = 0.1)
  accuracy <- sapply(proportion, function(perc) get_mean_accuracy(all_train_data, test_data, clss, perc))

  plot(proportion * nrow(all_train_data), accuracy, type = 'l', xlab = 'train sample size', ylab = 'accuracy')
}

ttt_data <- read.table('data/Tic_tac_toe.txt', sep = ',', stringsAsFactors = T)
data(spam)
plot_train_sample_accuracy(ttt_data, 'V10')
plot_train_sample_accuracy(spam, 'type')
