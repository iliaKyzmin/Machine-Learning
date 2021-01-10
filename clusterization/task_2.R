library(cluster)

cl1 <- cbind(matrix(rnorm(100, mean = 0, sd = 10), ncol = 1),
             matrix(rnorm(100, mean = 0, sd = 0.5), ncol = 1))
cl2 <- cbind(matrix(rnorm(100, mean = 0, sd = 10), ncol = 1),
             matrix(rnorm(100, mean = 4, sd = 0.5), ncol = 1))
cl3 <- cbind(matrix(rnorm(100, mean = 0, sd = 10), ncol = 1),
             matrix(rnorm(100, mean = 8, sd = 0.5), ncol = 1))
df <- rbind(cl1, cl2, cl3)
plot(df)


clusters1 <- clara(df, 
                   k = 3,
                   metric = "euclidean",
                   stand = F)
plot(df, col = clusters1$cluste)

clusters2 <- clara(df, 
                   k = 3,
                   metric = "euclidean",
                   stand = T)
plot(df, col = clusters2$cluste)



clusters3 <- clara(df, 
                   k = 3,
                   metric = "euclidean",
                   stand = T)
plot(df, col = clusters3$cluste)

clusters4 <- clara(df, 
                   k = 3,
                   metric = "manhattan",
                   stand = T)
plot(df, col = clusters4$cluste)
