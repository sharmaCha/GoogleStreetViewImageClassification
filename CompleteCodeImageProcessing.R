#install.packages("readr")
library(readr)
#install.packages("Bmp")
library(bmp)
#install.packages("pixmap")
library(pixmap)
#install.packages("dplyr")
library(dplyr)


# location of train and test images
train_files = list.files(path="E:\\data\\train\\train\\.", pattern=".Bmp",all.files=T, full.names=T, no.. = T) 
test_files = list.files(path="E:\\data\\test\\test\\.", pattern=".Bmp",all.files=T, full.names=T, no.. = T)

length(train_files)
length(test_files)
## read in train and test imagest_
#j <- read.bmp("E:\\data\\train\\train\\1.bmp", TRUE)  
#install.packages("animation")
#library("animation")
#is.bmp("E:\\data\\1.Bmp")
#install.packages("jpeg")
#library(jpeg)
#is.jpg("E:\\data\\12.jpg")
#readJPEG(system.file("E:\\data\\12.jpg", package="jpeg"))
#install.packages("rio")
#install.packages("readJPEG")
#install("readJPEG")
#install.packages("jpeg")  ## if necessary
#install.packages("rgdal")
#library(jpeg)
library(rgdal)
#img <- readGDAL(file.path("E:\\data\\1.Bmp"))
#img
#library("rio")
#x1 <- readJPEG("E:\\data\\12.jpg")
#convert("E:\\data\\1.Bmp", output = "E:\\data\\13.bmp")

#train_images = read.bmp(train_files)
train_images <- readGDAL(train_files)
test_images <- readGDAL(test_files)
#train_images = lapply(train_files, read.bmp)
#test_images = lapply(test_files, read.bmp)

# # inspect first element of train_images list
# # the element contains 3 matrices: R, G, and B
img2 = train_images[2][,,1] # matrix
img2 = train_images[[2]] # vector output
length(dim(img2)) 
r = img2[ , , 1]
View(r)
View(img2)
length(dim(img2))
img2_red = img2[, , 1]
View(img2_red)
img2_red1 = img2[1][, , 1]
View(img2_red1)


train_images1 <- readGDAL(train_files)
train_images1[1][,,1]
train_images1[1][,,1]
img2 = train_images1[1][,,1]
length(dim(img2))
# img1_green = img1[[1]][1:20, 1:20, 2] ### not working start
# img1_blue = img1[[1]][1:20, 1:20, 3]
# 
# # take the average of the three matrices and view the greyscale image
# img1_avg = (img1_red + img1_green + img1_blue)/3
# image(img1_avg,col=grey(seq(0,1,length=256)))
# 
# # convert img1_avg matrix to vector
# img1_vec = as.vector(img1_avg)
###########test begin ##############

  ############### test############
# OK, now that we've done it with one image, we need to do it with all the images in the train_images list.
# we will create a matrix of vectors
length(train_files)
train_images = list(rep(0,length(train_files)))
(train_images)
#train_images
#train_images_1 = readGDAL(train_images[1])
#img_test = train_images[1]
train_mat = matrix(ncol = 401, nrow = length(train_files))
nrow(train_mat)
#View(train_mat)
for(i in 1:nrow(train_mat)) {
  ID = unlist(strsplit(train_files[i], split = ".", fixed=T))[2]
  ID2 = as.numeric(unlist(strsplit(ID, split = "/"))[2])
  #train_images[[i]] = readGDAL(train_files[i])
  #img = train_images[[i]]
  train_images[i][,,1] = readGDAL(train_files[i])
  img = train_images[i][,,1]
  if (length(dim(img)) == 3) {
    r = img[ , , 1]
    g = img[ , , 2]
    b = img[ , , 3]
    temp = (r + g + b)/3
    #temp
    temp = as.vector(temp)
    temp = c(ID2, temp)
    train_mat[i] = temp
    #print(i)
  }
  else {
   # r = img[ , , 1]
    #temp = r
    temp = as.vector(img[,])
   # temp = as.vector(img[ , , 1])
    temp = c(ID2, temp)
    train_mat[i] = temp
  }
}
# check dimensions of matrix and to make sure everything loaded properly (no NAs)
dim(train_mat)
sum(is.na(train_mat))
View(train_mat)

# and for the test images also
test_images = list(rep(0,length(test_files)))
test_mat = matrix(ncol = 401, nrow = length(test_files))
for(i in 1:nrow(test_mat)) {
  ID = unlist(strsplit(test_files[i], split = ".", fixed=T))[2]
  ID2 = as.numeric(unlist(strsplit(ID, split = "/"))[2])
  test_images[[i]] = readGDAL(test_files[i])
  img = test_images[[i]]
  if (length(dim(img)) == 3) {
    r = img[ , , 1]
    g = img[ , , 2]
    b = img[ , , 3]
    temp = (r + g + b)/3
    temp = as.vector(temp)
    temp = c(ID2, temp)
    test_mat[i,] = temp
  }
  else {
    temp = as.vector(img[ , ])
    temp = c(ID2, temp)
    test_mat[i,] = temp
  }
}
# check dimensions of matrix and to make sure everything loaded properly (no NAs)
dim(test_mat)
sum(is.na(test_mat))

View(test_mat)
# convert matrices do data frames and delete other matrices, vectors, and lists from memory
trainDF = as.data.frame(train_mat)
trainDF = trainDF %>% dplyr::rename(ID = V1)
testDF = as.data.frame(test_mat)
testDF = testDF %>% dplyr::rename(ID = V1)
rm(r, g, b, temp, i, img, ID, ID2, train_mat, test_mat, train_images, test_images)
trainDF
# Read in the training labels and merge with the trainDF
trainLabels = read_csv("C:/Users/bhart/Documents/JuliaRepo/trainLabels.csv", col_types = cols(ID = col_character()))
trainLabels$ID = as.numeric(trainLabels$ID)
trainDF = merge(x = trainDF, y = trainLabels, by = "ID") # merge by common ID columns
trainDF = trainDF[, c(1,402,2:401)] # rearrange columns in dataframe so that ID is first, Class is second, ...
trainDF$Class = as.factor(trainDF$Class)

# Duplicates - change indices to 3:402 if you just loaded the data
dup1 = which(duplicated(trainDF[,3:402]))
dup2 = which(duplicated(trainDF[,3:402], fromLast = TRUE))
dup_rows = sort(c(dup1, dup2))
trainDF[dup_rows,1:5]
# row 3055 is classified as a "1",  but it has the same pixel values as row 4106, which is classified as an "E"
# it's definitely a 1, so we will remove row 4106
trainDF = trainDF[-c(2297, 3226, 4106, 4598, 4820, 5208, 6270), ]


# display image of specified row of trainDF
row_num = 4165
m = matrix(unlist(trainDF[row_num,-c(1,2)]),nrow = 20,byrow = F)
image(m,col=grey.colors(255), xlab = trainDF$Class[row_num])
hist(m)

# everything checks out, so I will remove the ID column
trainDF = trainDF[,-1]
rm(trainLabels)



# Now let's read in the data a differen way... instead of averaging the R, G, and B matrices, we will just put them next to each other.
# these new dataframes will have 1200 columns of data
train_images = list(rep(0,length(train_files)))
train_mat = matrix(ncol = 1201, nrow = length(train_files))
for(i in 1:nrow(train_mat)) {
  ID = unlist(strsplit(train_files[i], split = ".", fixed=T))[2]
  ID2 = as.numeric(unlist(strsplit(ID, split = "/"))[2])
  train_images[[i]] = read.bmp(train_files[i])
  img = train_images[[i]]
  if (length(dim(img)) == 3) {
    r = img[ , , 1]
    g = img[ , , 2]
    b = img[ , , 3]
    temp = c(as.vector(r), as.vector(g), as.vector(b))
    temp = c(ID2, temp)
    train_mat[i,] = temp
  }
  else {
    temp = c(as.vector(img[ , ]), as.vector(img[ , ]), as.vector(img[ , ]))
    temp = c(ID2, temp)
    train_mat[i,] = temp
  }
}
# check dimensions of matrix and to make sure everything loaded properly (no NAs)
dim(train_mat)
sum(is.na(train_mat))


# and for the test images also
test_images = list(rep(0,length(test_files)))
test_mat = matrix(ncol = 1201, nrow = length(test_files))
for(i in 1:nrow(test_mat)) {
  ID = unlist(strsplit(test_files[i], split = ".", fixed=T))[2]
  ID2 = as.numeric(unlist(strsplit(ID, split = "/"))[2])
  test_images[[i]] = read.bmp(test_files[i])
  img = test_images[[i]]
  if (length(dim(img)) == 3) {
    r = img[ , , 1]
    g = img[ , , 2]
    b = img[ , , 3]
    temp = c(as.vector(r), as.vector(g), as.vector(b))
    temp = c(ID2, temp)
    test_mat[i,] = temp
  }
  else {
    temp = c(as.vector(img[ , ]), as.vector(img[ , ]), as.vector(img[ , ]))
    temp = c(ID2, temp)
    test_mat[i,] = temp
  }
}
# check dimensions of matrix and to make sure everything loaded properly (no NAs)
dim(test_mat)
sum(is.na(test_mat))

# convert matrices do data frames and delete other matrices, vectors, and lists from memory
trainDFwide = as.data.frame(train_mat)
trainDFwide = trainDFwide %>% dplyr::rename(ID = V1)
testDFwide = as.data.frame(test_mat)
testDFwide = testDFwide %>% dplyr::rename(ID = V1)
rm(r, g, b, temp, i, img, ID, ID2, train_mat, test_mat, train_images, test_images, train_files, test_files)

# Read in the training labels and merge with the trainDF
trainLabels = read_csv("C:/Users/bhart/Documents/JuliaRepo/trainLabels.csv", col_types = cols(ID = col_character()))
trainLabels$ID = as.numeric(trainLabels$ID)
trainDFwide = merge(x = trainDFwide, y = trainLabels, by = "ID") # merge by common ID columns
trainDFwide = trainDFwide[, c(1,1202,2:1201)] # rearrange columns in dataframe so that ID is first, Class is second, ...
trainDFwide$Class = as.factor(trainDFwide$Class)

# Duplicates - change indices to 3:402 if you just loaded the data
dup1 = which(duplicated(trainDFwide[,3:1202]))
dup2 = which(duplicated(trainDFwide[,3:1202], fromLast = TRUE))
dup_rows = sort(c(dup1, dup2))
trainDFwide[dup_rows,1:5]
# row 3055 is classified as a "1",  but it has the same pixel values as row 4106, which is classified as an "E"
# it's definitely a 1, so we will remove row 4106
trainDFwide = trainDFwide[-c(2297, 3226, 4106, 4598, 4820, 5208, 6270), ]


# Take a look at trainDF
dim(trainDFwide)
head(trainDFwide[, 1:5])

# everything checks out, so I will remove the ID column
trainDFwide = trainDFwide[,-1]
rm(trainLabels)










###============== Data Organization ===============
# At this point, we have 4 dataframes:
# 1. trainDF with labels (6276 by 401) - extra column contains Class variable
# 2. testDF without labels (6220 by 401) - extra column contains ID variable
# 3. trainDFwide with labels (6276 by 1201) - extra column contains Class variable
# 4. testDFwide without labels (6220 by 1201) - extra column contains ID variable

# From these two dataframes, we will create a few others to be used for model training and testing prior to model 
# submission.  These dataframes can also be processed using different techniques (resizing, thresholding, etc.).
# Split TrainDF into train and test subsets because we don't really know the labels of the test set

# 70% of data in "train" and 30% of data in "test"
# We are splitting the trainDF into two dataframes because then we can calculate test set accuracy.
# We do not have the labels for trainDF, so this allows us to check for overfitting without submitting to Kaggle.
library(class)
library(caTools)
set.seed(123)
split = sample.split(trainDF, SplitRatio = 0.7)
train = subset(trainDF, split == TRUE)
test = subset(trainDF, split == FALSE)
trainWide = subset(trainDFwide, split == TRUE)
testWide = subset(trainDFwide, split == FALSE)

# Make smaller training and testing sets for the gbm because it's such a computationally intensive process
# set.seed(163)
# train_small = sample_n(train, 1000)
# test_small = sample_n(test, 500)
# train_labels_small = train_small[,1]
# test_labels_small = test_small[,1]

# Create two versions of trainDF - one where the images are darker and one where the images are lighter.
# This is an attempt to separate light letters with dark background from dark letters with light backgrounds.
# To accomplish this, we will just split the images into 2 groups by mean pixel value.
# trainDF_temp = trainDF
# trainDF_temp$means = rowMeans(trainDF_temp[,2:length(trainDF_temp)])
# trainDF_light = trainDF_temp[trainDF_temp$means < 255/2,]
# trainDF_dark = trainDF_temp[trainDF_temp$means > 255/2,]
# trainDF_light = trainDF_light[,-402]
# trainDF_dark = trainDF_dark[,-402]

# Using the "imager" package, create dataframe of downsampled training images (10 pixels by 10 pixels)
library(imager)
trainDF_temp = trainDF # make a copy of trainDF just in case i mess it up

downsampled_matrix = matrix(ncol = 100, nrow = nrow(trainDF_temp))
for(i in 1:nrow(trainDF_temp)){
  m = matrix(unlist(trainDF_temp[i, -1]),nrow = 20,byrow = F)
  img = as.cimg(m)
  img_resized = resize(img,round(width(img)/2),round(height(img)/2))
  downsampled_matrix[i, ] = as.vector(img_resized)
}

trainDF_downsampled = as.data.frame(downsampled_matrix)
trainDF_downsampled$Class = trainDF_temp$Class
trainDF_downsampled = trainDF_downsampled[,c(101, 1:100)]
head(trainDF_downsampled[ , 1:5])

# let's take a look at these downsampled images
row_num = 2712
m = matrix(unlist(trainDF_downsampled[row_num, -c(1)]), nrow = 10, byrow = F)
image(m,col=grey.colors(255), xlab = trainDF_downsampled$Class[row_num])
# compared to the originals . . .
m = matrix(unlist(trainDF_temp[row_num, -c(1)]), nrow = 20, byrow = F)
image(m,col=grey.colors(255), xlab = trainDF_temp$Class[row_num])

# OK, it works!  Now let's do it for the test set too (testDF)
testDF_temp = testDF # make a copy of trainDF just in case i mess it up

downsampled_matrix = matrix(ncol = 100, nrow = nrow(testDF_temp))
for(i in 1:nrow(testDF_temp)){
  m = matrix(unlist(testDF_temp[i, -1]),nrow = 20,byrow = F)
  img = as.cimg(m)
  img_resized = resize(img,round(width(img)/2),round(height(img)/2))
  downsampled_matrix[i, ] = as.vector(img_resized)
}

testDF_downsampled = as.data.frame(downsampled_matrix)
testDF_downsampled$ID = testDF_temp$ID
testDF_downsampled = testDF_downsampled[,c(101, 1:100)]
head(testDF_downsampled[ , 1:5])


# Thresholding: round to black or white depending on a threshold value
trainDF_threshold = apply(trainDF[ , -1], 1, threshold)
trainDF_threshold = t(trainDF_threshold)
trainDF_threshold = as.data.frame(trainDF_threshold)
trainDF_threshold$Class = trainDF$Class
trainDF_threshold = trainDF_threshold[ , c(401, 1:400)]

testDF_threshold = apply(testDF[ , -1], 1, threshold)
testDF_threshold = t(testDF_threshold)
testDF_threshold = as.data.frame(testDF_threshold)
testDF_threshold$ID = testDF$ID
testDF_threshold = testDF_threshold[ , c(401, 1:400)]

# OK, no let's downsample the thresholded images: 20 by 20 threshold --> 10 by to threshold
# Training data:
trainDF_threshold_temp = trainDF_threshold # make a copy of trainDF just in case i mess it up

downsampled_matrix = matrix(ncol = 100, nrow = nrow(trainDF_threshold_temp))
for(i in 1:nrow(trainDF_threshold_temp)){
  m = matrix(unlist(trainDF_threshold_temp[i, -1]),nrow = 20,byrow = F)
  img = as.cimg(m)
  img_resized = resize(img,round(width(img)/2),round(height(img)/2))
  downsampled_matrix[i, ] = as.vector(img_resized)
}

trainDF_downsampled_thresh = as.data.frame(downsampled_matrix)
trainDF_downsampled_thresh$Class = trainDF_temp$Class
trainDF_downsampled_thresh = trainDF_downsampled_thresh[,c(101, 1:100)]
head(trainDF_downsampled_thresh[ , 1:5])

#Testing Data:
testDF_threshold_temp = trainDF_threshold # make a copy of trainDF just in case i mess it up

downsampled_matrix = matrix(ncol = 100, nrow = nrow(testDF_threshold_temp))
for(i in 1:nrow(testDF_threshold_temp)){
  m = matrix(unlist(testDF_threshold_temp[i, -1]),nrow = 20,byrow = F)
  img = as.cimg(m)
  img_resized = resize(img,round(width(img)/2),round(height(img)/2))
  downsampled_matrix[i, ] = as.vector(img_resized)
}

testDF_downsampled_thresh = as.data.frame(downsampled_matrix)
testDF_downsampled_thresh$Class = trainDF_temp$Class
testDF_downsampled_thresh = testDF_downsampled_thresh[,c(101, 1:100)]
head(testDF_downsampled_thresh[ , 1:5])


# Comparisons
op = par(mfrow = c(2,2))
row_num = 1221
m = matrix(unlist(trainDF[row_num, -c(1)]), nrow = 20, byrow = F)
image(m,col=grey.colors(255), xlab = trainDF$Class[row_num], main = "Original (20 by 20)")
m = matrix(unlist(trainDF_downsampled[row_num, -c(1)]), nrow = 10, byrow = F)
image(m,col=grey.colors(255), xlab = trainDF_downsampled$Class[row_num], main = "Downsampling (10 by 10)")
m = matrix(unlist(trainDF_threshold[row_num, -c(1)]), nrow = 20, byrow = F)
image(m,col=grey.colors(255), xlab = trainDF_threshold$Class[row_num], main = "Thresholding (20 by 20)")
m = matrix(unlist(trainDF_downsampled_thresh[row_num, -c(1)]), nrow = 10, byrow = F)
image(m,col=grey.colors(255), xlab = trainDF_downsampled_thresh$Class[row_num], main = "Thresholding (10 by 10)")
par(op)


# 70% of data in "train" and 30% of data in "test"
# We are splitting the trainDF into two dataframes because then we can calculate test set accuracy.
# We do not have the labels for trainDF, so this allows us to check for overfitting without submitting to Kaggle.
# split was created earlier for trainDF and trainDFwide
trainDownsamp = subset(trainDF_downsampled, split == TRUE)
testDownsamp = subset(trainDF_downsampled, split == FALSE)
trainThresh = subset(trainDF_threshold, split == TRUE)
testThresh = subset(trainDF_threshold, split == FALSE)
trainDownsampThresh = subset(trainDF_downsampled_thresh, split == TRUE)
testDownsampThresh = subset(trainDF_downsampled_thresh, split == FALSE)


# Make scaled versions of train and test dataframes for machine learning algorithms.
train_scaled = cbind(train$Class, as.data.frame(apply(train[, -c(1)], 2, scale)))
colnames(train_scaled)[which(names(train_scaled) == "train$Class")] = "Class"

test_scaled = cbind(test$Class, as.data.frame(apply(test[, -c(1)], 2, scale)))
colnames(test_scaled)[which(names(test_scaled) == "test$Class")] = "Class"


###=========== Linear Discriminant Analysis with LOOCV ===============
library(MASS)
ldaMod = lda(Class ~ ., data = trainDF, CV = TRUE)

# Assess the accuracy of the prediction
# percent correct for each category of G
classificationTable = table(trainDF$Class, ldaMod$class)
sort(diag(prop.table(classificationTable, 1))) # max was 22.66 % of A's classified correctly
# total percent correct
sum(diag(prop.table(classificationTable))) # only 10% correct on training set
# it's not too surprising that LDA didn't perform well... this is obviously a very nonlinear problem



###======== KNN with k=1 ==============
set.seed(111)
knn.pred = knn(train[,-1], test[,-1], cl = train$Class, k=1, prob = TRUE)
mean(knn.pred != test$Class) # 61% error rate, 39% accuracy on test set
View(as.data.frame.matrix(table(knn.pred, test$Class))) # classification table
# k=3: # 66.5% error rate, 33.5% accuracy on test set

set.seed(111)
knn.pred = knn(trainWide[,-1], testWide[,-1], cl = trainWide$Class, k=1, prob = TRUE)
mean(knn.pred == testWide$Class) 
View(as.data.frame.matrix(table(knn.pred, test$Class)))

set.seed(111)
knn.pred = knn(trainDownsamp[,-1], testDownsamp[,-1], cl = trainDownsamp$Class, k=1, prob = TRUE)
mean(knn.pred == testDownsamp$Class) 
View(as.data.frame.matrix(table(knn.pred, testDownsamp$Class)))

set.seed(111)
knn.pred = knn(trainThresh[,-1], testThresh[,-1], cl = trainThresh$Class, k=1, prob = TRUE)
mean(knn.pred == testThresh$Class) 
View(as.data.frame.matrix(table(knn.pred, testThresh$Class)))

set.seed(111)
knn.pred = knn(trainDownsampThresh[,-1], testDownsampThresh[,-1], cl = trainDownsampThresh$Class, k=1, prob = TRUE)
mean(knn.pred == testDownsampThresh$Class) 
View(as.data.frame.matrix(table(knn.pred, testDownsampThresh$Class)))


# which value of k is best??
set.seed(333)
error = rep(0,35)
#takes about 5 min for k=1:35
for (i in 1:35) {
  knn.pred = knn(train[,-1], test[,-1], train$Class, k = i)
  error[i] = mean(knn.pred != test$Class)
}
plot(1:35, error, type = "l")
plot(1:100, error, type = "l")
error[which.min(error)]

knn.pred = knn(train[, -1], test[, -1], train$Class, k = 1)


###========= random forest ============
library(randomForest)
set.seed(123)

# original data
rf.fit = randomForest(Class ~ ., ntree=500, data=train) # default mtry = sqrt(p), ntree = 500

# Make predictions:
predictRF = predict(rf.fit, newdata=test, type = "response")

View(as.data.frame.matrix(table(predictRF, test$Class)))
mean(predictRF == test$Class)

# 10 by 10
rf.fit = randomForest(Class ~ ., ntree=500, data=trainDownsamp) # default mtry = sqrt(p), ntree = 500

# Make predictions:
predictRF = predict(rf.fit, newdata=testDownsamp, type = "response")

View(as.data.frame.matrix(table(predictRF, testDownsamp$Class)))
mean(predictRF == testDownsamp$Class)

# Threshold
rf.fit = randomForest(Class ~ ., ntree=500, data=trainThresh) # default mtry = sqrt(p), ntree = 500

# Make predictions:
predictRF = predict(rf.fit, newdata=testThresh, type = "response")

View(as.data.frame.matrix(table(predictRF, testThresh$Class)))
mean(predictRF == testThresh$Class)

# 10 by 10 with Threshold
rf.fit = randomForest(Class ~ ., ntree=500, data=trainDownsampThresh) # default mtry = sqrt(p), ntree = 500

# Make predictions:
predictRF = predict(rf.fit, newdata=testDownsampThresh, type = "response")

View(as.data.frame.matrix(table(predictRF, testDownsampThresh$Class)))
mean(predictRF == testDownsampThresh$Class)

# All data R, G, B Concatenated
rf.fit = randomForest(Class ~ ., ntree=500, data=trainWide) # default mtry = sqrt(p), ntree = 500

# Make predictions:
predictRF = predict(rf.fit, newdata=testWide, type = "response")

View(as.data.frame.matrix(table(predictRF, testWide$Class)))
mean(predictRF == testWide$Class)


###=========== GBM ================
library(dplyr)
library(gbm)


gbm.mod = gbm(Class ~ ., data = train_small, distribution = "multinomial", n.trees = 5000, interaction.depth = 4, verbose = TRUE)
?gbm
gbm.perf(gbm.mod, method="OOB")
best.iter = gbm.perf(gbm.mod, method = "OOB")
summary(gbm.mod,n.trees=1)
summary(gbm.mod,n.trees = best.iter)

gbmPred = predict(gbm.mod, newdata = test_small, n.trees = best.iter, type = "response")

pred_mat = matrix(ncol=62, nrow=500)
for (i in 1:500) {
  pred_mat[i,] = gbmPred[i, 1:62, 1]
}

# create data frame of predicted probabilities for each class on test set
predDF = as.data.frame(pred_mat)
colnames(predDF) = names(gbmPred[1,1:62,1])
head(predDF)

# create vector of predicted classes for each row of predDF
predicted_classes = colnames(predDF)[apply(predDF, 1, which.max)]
mean(predicted_classes != test_labels_small) # 90% error with depth=3, ntrees=1000; 94% error with depth=1, ntrees=2000; 87.8% error with depth=4, ntrees=5000


head(gbm.mod$estimator)
dim(gbm.mod$estimator)
predicted_classes_training = colnames(gbm.mod$estimator)[apply(gbm.mod$estimator,1,which.max)]
mean(predicted_classes_training != train_labels_small) #58.3% training error



# GBM with H20 Package
library(h2o)

localH2O = h2o.init(max_mem_size = '6g', # use 4GB of RAM of *GB available
                    nthreads = -1) # use all CPUs (8 on my personal computer)

train_h2o = as.h2o(trainThresh)
test_h2o = as.h2o(testThresh)

gbm1 <- h2o.gbm(
  training_frame = train_h2o,     ##
  validation_frame = test_h2o,   ##
  x=2:401,                     ##
  y=1,                       ## 
  ntrees = 20,                ## decrease the trees, mostly to allow for run time
  ##  (from 50)
  learn_rate = 0.2,           ## increase the learning rate (from 0.1)
  max_depth = 10,             ## increase the depth (from 5)
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType2",  ##
  seed = 2000000)             ##

summary(gbm1)
View(h2o.confusionMatrix(gbm1))
h2o.hit_ratio_table(gbm1,valid = T)[1,2]

gbm2 <- h2o.gbm(
  training_frame = train_h2o,     ##
  validation_frame = test_h2o,   ##
  x=2:401,                     ##
  y=1,                       ## 
  ntrees = 10,                ## add a few trees (from 20, though default is 50)
  learn_rate = 0.01,         ## increase the learning rate even further
  max_depth = 5,             ## 
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType3",  ##
  seed = 2000000)             ##

summary(gbm2)
View(h2o.confusionMatrix(gbm2))
h2o.hit_ratio_table(gbm2,valid = T)[1,2]


## classify test set
h2o_y_test = h2o.predict(gbm1, test_h2o)
preds = as.vector(h2o_y_test$predict)

#testPreds = data.frame(testDF$ID, preds)
#testPreds = testPreds %>% dplyr::rename(ID = testDF.ID, Class = preds)

mean(preds == test$Class) 

View(as.data.frame.matrix(table(preds, test$Class)))

## shut down virutal H2O cluster
h2o.shutdown(prompt = F)

###============= t-SNE ==================
library(Rtsne)

train_unique = unique(trainDF_threshold)
train_matrix = as.matrix(sapply(train_unique[,-1], as.numeric))
train_matrix = scale(train_matrix)

set.seed(176) # for reproducibility
tsne = Rtsne(train_matrix, dims = 3, perplexity=25, verbose=TRUE, max_iter = 1000, pca = TRUE)
tsneDF = as.data.frame(tsne$Y)

# visualizing in 2D
colors = rainbow(length(unique(train_unique$Class)))
names(colors) = unique(train_unique$Class)
plot(tsne$Y, t='n', main="t-SNE", xlab="1st Embedding", ylab="2nd Embedding")
text(tsne$Y, labels=train_unique$Class, col=colors[train_unique$Class])

# visualizing in 3D
library(plot3D)
library(rgl)

embeddings = as.data.frame(tsne$Y)
embeddings$Class = train_unique$Class
plot3d(x=embeddings$V1, y=embeddings$V2 ,z=embeddings$V3, col = as.numeric(embeddings$Class), pch = ".")
# is that separation between the two larger groups possibly due to the darker letters with lighter background and
# lighter letters with darker background??

###============= PCA =================
pca = princomp(train[,-1])
summary(pca)
plot(pca)

# first two components
plot(pca$scores[,1:2], t='n', main="First 2 Components")
text(pca$scores[,1:2], labels=train$Class, col=colors()[train$Class])

# first three components
plot3d(pca$scores[,1:3], col=colors[train$Class])





### ===== Support Vector Machines ===== ###
library("e1071")


# Make smaller training and testing sets for the svm because it's such a computationally intensive process
library(dplyr)
set.seed(163)
train_small = sample_n(train, 1000)


# try different kernel
svm.linear <- tune(svm, Class ~ ., data = train, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
summary(svm.lin)
# threshold data: the lowest error rate for linear kernel is when cost=0.01, 15% accuracy 
# downsample data: the lowest error rate for linear kernel is when cost=0.1, 11.9% accuracy 


svm.radial <- tune(svm, Class ~ ., data = trainWide, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.001,0.01,0.1)))
summary(svm.radial)
# threshold data: the lower error rate for radial kernel is when the cost=5/10/100 and gamma=0.01, 33.4% accuracy
# downsample data: the lower error rate for radial kernel is when the cost=5/10 and gamma=0.01, 27.6% accuracy


svm.poly <- tune(svm, Class ~ ., data = trainWide, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4)))
summary(svm.poly)
# threshold data: the lower error rate for polynomial kernel is when degree=2, and cost=5/10/100, 45.4% accuracy.
# downsample data: the lower error rate for polynomial kernel is when degree=2, and cost=10, 32.9% accuracy.

# polynomial kernel in threshold data 
svm.fit <- svm(Class ~ ., data = trainDF_threshold, kernel = "polynomial",cost=10,degree=2)
svm.pred = predict(svm.fit, testDF_threshold[,-1])





#### ====== Neural Network using H2O package ====== ###
library(h2o)

localH2O = h2o.init(max_mem_size = '6g', # use 4GB of RAM of *GB available
                    nthreads = -1) # use all CPUs (8 on my personal computer)

train_h2o = as.h2o(trainDF)
test_h2o = as.h2o(testDF)

## train model
model =
  h2o.deeplearning(x = 2:length(train_h2o),  # column numbers for predictors
                   y = 1,   # column number for label
                   training_frame = train_h2o, # data in H2O format
                   activation = "RectifierWithDropout", # algorithm
                   input_dropout_ratio = 0.05, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5, 0.5, 0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(300, 300, 300), # one layer of 400 nodes
                   momentum_stable = 0.99,
                   nesterov_accelerated_gradient = T, # use it for speed
                   epochs = 5000) # no. of epochs


View(h2o.confusionMatrix(model)) # % training error rate

## classify test set
h2o_y_test = h2o.predict(model, test_h2o)
preds = as.vector(h2o_y_test$predict)

testPreds = data.frame(testDF$ID, preds)
testPreds = testPreds %>% dplyr::rename(ID = testDF.ID, Class = preds)
write_csv(testPreds, path = "C:/Users/bhart/Dropbox/MSSP/Kaggle Streetview Image Classification/nn_submission_orig.csv")

mean(preds != test$Class) # % testing error rate

View(as.data.frame.matrix(table(preds, test$Class)))




models = c()
for (i in 1:100) {
  rand_activation <- c("TanhWithDropout", "RectifierWithDropout")[sample(1:2,1)]
  rand_numlayers <- sample(2:5,1)
  rand_hidden <- c(sample(100:400,rand_numlayers,T))
  rand_l1 <- runif(1, 0, 1e-3)
  rand_l2 <- runif(1, 0, 1e-3)
  rand_dropout <- c(runif(rand_numlayers, 0, 0.6))
  rand_input_dropout <- runif(1, 0, 0.5)
  dlmodel <- h2o.deeplearning(
    model_id=paste0("dl_random_model_", i),
    training_frame=train_h2o,
    validation_frame=test_h2o, 
    x=2:length(train_h2o), 
    y=1,
    #    epochs=100,                    ## for real parameters: set high enough to get to convergence
    epochs=100,
    stopping_metric="misclassification",
    stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
    stopping_rounds=2,
    score_validation_samples=10000, ## downsample validation set for faster scoring
    score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
    max_w2=10,                      ## can help improve stability for Rectifier
    
    ### Random parameters
    activation=rand_activation, 
    hidden=rand_hidden, 
    l1=rand_l1, 
    l2=rand_l2,
    input_dropout_ratio=rand_input_dropout, 
    hidden_dropout_ratios=rand_dropout
  )                                
  models <- c(models, dlmodel)
}

best_err = 1      ##start with the best reference model from the grid search above, if available
for (i in 1:length(models)) {
  err <- h2o.confusionMatrix(h2o.performance(models[[i]],valid=T))$Error[8]
  if (err < best_err) {
    best_err <- err
    best_model <- models[[i]]
  }
}
h2o.confusionMatrix(best_model,valid=T)
best_params = best_model@allparameters
best_params$hidden
best_params$l1
best_params$l2
best_params$input_dropout_ratio


## shut down virutal H2O cluster
h2o.shutdown(prompt = F)
