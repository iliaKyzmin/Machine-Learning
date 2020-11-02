library(kknn)

# load data 
data("glass")
glass <- glass[-1]

k_kernel_test <- function(all_data) {
  clsfr <- train.kknn(Type ~ ., 
                      data = all_data, 
                      kmax = 15, 
                      kernel = c("rectangular", "triangular", "epanechnikov", "optimal"),
                      distance = 1)
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


get_type <- function(all_data, sample) {
  fit_data <- fitted(kknn(Type ~ .,
                          all_data,
                          sample,
                          kernel = "optimal",
                          distance = 1))
  
  as.character(fit_data)
}

smpl <- data.frame("RI" = c(1.516),
                   "Na" = c(11.7),
                   "Mg" = c(1.01),
                   "Al" = c(1.19),
                   "Si" = c(72.29),
                   "K"  = c(0.43),
                   "Ca" = c(11.44),
                   "Ba" = c(0.02),
                   "Fe" = c(0.1),
                   "Type" = c(1))

print(as.integer(get_type(glass, smpl)))

for (feature_ind in 1:(ncol(glass) - 1)) {
  print(paste(colnames(glass)[feature_ind], get_type(glass[-feature_ind], smpl[-feature_ind]), sep = " - "))
}
