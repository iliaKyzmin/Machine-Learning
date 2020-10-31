library(kknn)

# load data 
data("glass")
glass <- glass[-1]

k_kernel_test <- function(all_data) {
  clsfr <- train.kknn(Type ~ ., 
                      data = glass, 
                      kmax = 15, 
                      kernel = c("rectangular", "triangular", "epanechnikov", "optimal"),
                      distance = 2)
  plot(clsfr)
  
}

distance_test <- function(all_data, dist) {
  
  clsfr <- train.kknn(Type ~ ., 
                      all_data, 
                      kmax = 15, 
                      kernel = c("rectangular", "triangular", "epanechnikov", "optimal"), 
                      distance = dist)
  
  best_params = clsfr[["best.parameters"]]
  clsfr[["MISCLASS"]][best_params$k, best_params$kernel]
}


k_kernel_test(glass)


distances <- 1:8
misclass <- sapply(distances, function(dist) distance_test(glass, dist))

plot(distances, misclass, xlab = 'distance', ylab = 'misclassification')
