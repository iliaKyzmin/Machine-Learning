library(dplyr)

all_data <- read.table('data/pima-indians-diabetes.data', sep = ',')
all_data <- all_data[, -9]

cl <- hclust(dist(scale(all_data)))
dendr <- as.dendrogram(cl)
plot(dendr)