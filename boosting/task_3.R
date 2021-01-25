library(dplyr)
library(mlbench)
library(adabag)
library(pryr)
library(rpart)

weighted_knn <- function(elem, target, train_data, k, weights) {
  train_data$dists <- apply(train_data[, colnames(train_data) != target], 1, 
                            function(row) sum((row - elem)^2))
  
  train_data$weights <- weights
  nearest_neighbors <- slice_min(train_data, order_by = dists, n = k)
  classes <- unique(nearest_neighbors[[target]])
  
  count_freqs <- function(clss, n_n) {
    sum(n_n[n_n[[target]] == clss, ]$weights)
  }
  classes_freqs <- sapply(classes, function(clss) {count_freqs(clss, nearest_neighbors)})
  return(sample(classes[classes_freqs == max(classes_freqs)], 1))
}

train_boosting <- function(target, train_data, k = 13, iters = 5) {
  n <- nrow(train_data)
  
  weights <- rep(1 / n, n)
  classifiers <- list()
  alphas <- numeric(iters)
  
  for (t in 1:iters) {
    classifier <- partial(weighted_knn, target = target, 
                          train_data = train_data, k = k, 
                          weights = weights, .lazy = F)
    predicted <- apply(train_data[, colnames(train_data) != target], 1, 
                       function(row) classifier(row))
    error <- sum(sapply(1:n, function(i) 
      {if (predicted[i] != train_data[[target]][i]) weights[i] else 0.}))
    
    classifiers[[t]] <- classifier
    alphas[t] <- 0.5 * log((1. - error) / error)
    for (i in 1:n) {
        weights[i] <- weights[i] * exp(alphas[t] * (if (predicted[i] == train_data[[target]][i]) -1 else 1))
    }
    for (i in 1:n) {
      weights[i] <- weights[i] / sum(weights)
    }
  }
  
  return(list(classifiers, alphas))
}

predict_boosting <- function(model, test_data) {
  predict_for_one <- function(classifiers, alphas, elem) {
    predictions <- sapply(classifiers, function(classifier) classifier(elem))
    classes <- unique(predictions)
    w <- sapply(classes, function(clss) {sum(alphas[predictions == clss])})
    return(sample(predictions[w == max(w)], 1))
  }
  
  apply(test_data, 1, function(row) predict_for_one(model[[1]], model[[2]], row))
}

get_test_train_data <- function(all_data) { 
  n <- nrow(all_data)
  nt <- as.integer(n * 0.7)
  all_data <- all_data[order(runif(n)), ]
  train_data <- all_data[1:nt, ]
  test_data <- all_data[(nt + 1):n, ]
  return(list(train_data, test_data))
}

get_knn_predicted <- function(train_data, test_data, target, iters = 11) {
  model <- train_boosting(target, train_data, k = 15, iters = iters)
  return(predict_boosting(model, test_data[, colnames(test_data) != target]))
}

get_tree_predicted <- function(train_data, test_data, target) {
  model <- rpart(as.formula(paste(target, '~ .')), data = train_data, maxdepth = 5)
  return(predict(model, test_data, type = 'class'))
}

get_error <- function(predicted, test_data, target) {
  return(1 - sum(diag(table(test_data[[target]], predicted))) / nrow(test_data))
}



# Glass dataset
data("Glass")
glass_data <- Glass
tt_data <- get_test_train_data(glass_data)
knn_predicted <- get_knn_predicted(tt_data[[1]], tt_data[[2]], 'Type', 11)
print(paste0('Knn boosting error - ', get_error(knn_predicted, tt_data[[2]], 'Type')))

tree_predicted <- get_tree_predicted(tt_data[[1]], tt_data[[2]], 'Type')
print(paste0('Single tree error - ', get_error(tree_predicted, tt_data[[2]], 'Type')))



# Vehicle dataset
data("Vehicle")
vehicle_data <- Vehicle
tt_data <- get_test_train_data(vehicle_data)
knn_predicted <- get_knn_predicted(tt_data[[1]], tt_data[[2]], 'Class', 11)
print(paste0('Knn boosting error - ', get_error(knn_predicted, tt_data[[2]], 'Class')))

tree_predicted <- get_tree_predicted(tt_data[[1]], tt_data[[2]], 'Class')
print(paste0('Single tree error - ', get_error(tree_predicted, tt_data[[2]], 'Class')))
