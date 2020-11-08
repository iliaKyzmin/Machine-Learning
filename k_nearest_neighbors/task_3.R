library(kknn)

train_data <- read.table('data/svmdata4.txt', sep = '\t', header = T, stringsAsFactors = T)
test_data <- read.table('data/svmdata4test.txt', sep = '\t', header = T, stringsAsFactors = T)


plot(train_data$X1, 
     train_data$X2, pch=21, 
     bg=c("green","red") [unclass(train_data$Colors)],  
     main="Train data")

clsfr <- train.kknn(Colors ~ ., 
                    data = train_data, 
                    kmax = 15, 
                    kernel = c("rectangular", "triangular", "epanechnikov", "optimal"),
                    distance = 2)

print(clsfr[["best.parameters"]])
predicted <- predict(clsfr, test_data)
tbl <- table(predicted, test_data$Colors)
print(paste("Accuracy - ", as.character((tbl[1, 1] + tbl[2, 2]) / nrow(test_data))))

