reg_data <- read.csv('data/reglab2.txt', sep = '\t')
for (j in 1:4) {
  combination <- combn(reg_data[, 2:5], j)
  for(i in 1:length(combination[1,])) {
    if(j == 1)
      tmp <- data.frame(reg_data$y, combination[1, i])
    if(j == 2) 
      tmp <- data.frame(reg_data$y, combination[1, i], combination[2, i])
    if(j == 3)
      tmp <- data.frame(reg_data$y, combination[1, j], combination[2, i], combination[3, i])
    if(j == 4)
      tmp <- data.frame(reg_data$y, combination[1, i], combination[2, i], combination[3, i], combination[4, i])
    
    res <- lm(tmp$reg_data.y ~., data = tmp)
    print(sum((tmp$reg_data.y - res$fitted.values)^2))
  }
}