# Unsupervised Learning
# PCA and t-SNE

###============= t-SNE  t-Distributed Stochastic Neighbor Embedding (t-SNE)==================
#install.packages("Rtsne")
library(Rtsne)
library(graphics)

train_unique = unique(trainDF_threshold)
train_matrix = as.matrix(sapply(train_unique[,-1], as.numeric))
train_matrix = scale(train_matrix)

set.seed(176) # for reproducibility
tsne = Rtsne(train_matrix, dims = 2, perplexity=25, verbose=TRUE, max_iter = 500, pca = TRUE)
# Read the 6276 x 50 data matrix successfully!
#   Using no_dims = 2, perplexity = 25.000000, and theta = 0.500000
# Computing input similarities...
# Normalizing input...
# Building tree...
# - point 0 of 6276
# Done in 17.19 seconds (sparsity = 0.016687)!
#   Learning embedding...
# Iteration 50: error is 93.795932 (50 iterations in 9.38 seconds)
# Iteration 100: error is 88.035343 (50 iterations in 9.88 seconds)
# Iteration 150: error is 87.586115 (50 iterations in 8.95 seconds)
# Iteration 200: error is 87.536920 (50 iterations in 9.14 seconds)
# Iteration 250: error is 87.527490 (50 iterations in 9.05 seconds)
# Iteration 300: error is 3.367713 (50 iterations in 8.33 seconds)
# Iteration 350: error is 2.964163 (50 iterations in 8.10 seconds)
# Iteration 400: error is 2.746925 (50 iterations in 8.30 seconds)
# Iteration 450: error is 2.611191 (50 iterations in 10.46 seconds)
# Iteration 500: error is 2.516216 (50 iterations in 10.11 seconds)
# Fitting performed in 91.71 seconds.
tsneDF = as.data.frame(tsne$Y)
View(tsneDF)
# visualizing in 2D
#library(plotrix)
png(file="E://data//plots//tsne2.png",    width=600, height=350)
colors = rainbow(length(unique(train_unique$Class)))
names(colors) = unique(train_unique$Class)
plot(tsne$Y, t='n', main="t-SNE", xlab="1st Embedding", ylab="2nd Embedding")
text(tsne$Y, labels=train_unique$Class, col=colors[train_unique$Class])
dev.off()

# visualizing in 3D
#install.packages("plot3D")
library(plot3D)
#install.packages("rgl")
library(rgl)

embeddings = as.data.frame(tsne$Y)
embeddings$Class = train_unique$Class
plot3d(x=embeddings$V1, y=embeddings$V2 ,z=embeddings$V3, col = as.numeric(embeddings$Class), pch = ".")
# is that separation between the two larger groups possibly due to the darker letters with lighter background and
# lighter letters with darker background??

###============= PCA =================
library(car)
library(RColorBrewer)

pca = princomp(train[,-1])
summary(pca)
plot(pca)

# first two components
png(file="E://data//plots//pca2.png",    width=600, height=350)
plot(pca$scores[,1:2], t='n', main="First 2 Components")
text(pca$scores[,1:2], labels=train$Class, col=colors()[train$Class])
dev.off()

dev.copy(png,'E://data//plots//pca1.png')
dev.off()

# first three components
plot3d(pca$scores[,1:3], col=colors[train$Class])



