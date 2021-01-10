library(tree)
library(maptree)

data("Glass")
all_data <- Glass[, -1]

model <- tree(Type ~ ., data = all_data)
print(predict(model, data.frame(RI = 1.516, 
                                Na = 11.7, 
                                Mg = 1.01, 
                                Al = 1.19 ,
                                Si = 72.59, 
                                K = 0.43, 
                                Ca = 11.44, 
                                Ba = 0.02, 
                                Fe = 0.1)))
