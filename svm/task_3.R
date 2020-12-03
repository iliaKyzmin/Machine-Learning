library(e1071)

train_data <- read.table('data/svmdata3.txt', sep = '\t', header = T, stringsAsFactors = T)

plot_clsfr <- function(clsfr) {
  
  area_pallete <- function(n = 3) {
    colors <- rainbow(n)
    colors[1:2] <- c("#e2fee2", "pink")
    return(colors) 
  }

  plot(clsfr, 
      train_data,
      grid = 250,
      symbolPalette = c("darkgreen", "red"),
      color.palette = area_pallete)
}

find_optimal_kernel <- function(kernel) {
  svm_clsfr <- svm(Colors ~ ., 
                   data = train_data,
                   type = "C-classification",
                   cost = 1,
                   kernel = kernel)
  
    pred <- predict(svm_clsfr, train_data)
    tbl <- table(pred, train_data$Color)
    
    
    plot_clsfr(svm_clsfr)
    return(1. - sum(diag(tbl)) / nrow(train_data))
}

print(paste("Polynomial - ", find_optimal_kernel("polynomial")))
print(paste("Radial - ", find_optimal_kernel("radial")))
print(paste("Sigmoid - ", find_optimal_kernel("sigmoid")))


get_pol_error <- function(degree) {
  svm_clsfr <- svm(Colors ~ ., 
                   data = train_data,
                   type = "C-classification",
                   cost = 1,
                   kernel = "polynomial",
                   degree = degree)
  
  pred <- predict(svm_clsfr, train_data)
  tbl <- table(pred, train_data$Color)
  return(1. - sum(diag(tbl)) / nrow(train_data))
}

for (deg in 1:10) {
  print(paste('Degree ', deg, ' - ', get_pol_error(deg)))
}
