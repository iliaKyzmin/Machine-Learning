library(Rtsne)

all_data <- read.table('data/pima-indians-diabetes.data', sep = ',')
tsne_data <- Rtsne(all_data)
plot(tsne_data$Y, col=c('red', 'green')[1 + all_data$V9])
