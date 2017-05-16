# Process Image Data
# This script creates several dataframes that can be used in the prediction.R script.

library(class)
library(caTools)
#install.packages("imager")
library(imager)
library(ggplot2)
# 
# trainDF = readRDS("E:\\data\\trainDF.RDS")
# testDF = readRDS("E:\\data\\testDF.RDS")


trainDF = readRDS("E:\\data\\trainDF2.RDS")
testDF = readRDS("E:\\data\\testDF.RDS")
###============== Data Organization ===============
# At this point, we have 2 dataframes:
# 1. trainDF with labels (6276 by 401) - extra column contains Class variable
# 2. testDF without labels (6220 by 401) - extra column contains ID variable


# From these two dataframes, we will create a few others to be used for model training and testing prior to model 
# submission.  These dataframes can also be processed using different techniques (resizing, thresholding, etc.).
# Split TrainDF into train and test subsets because we don't really know the labels of the test set

# 70% of data in "train" and 30% of data in "test"
# We are splitting the trainDF into two dataframes because then we can calculate test set accuracy.
# We do not have the labels for trainDF, so this allows us to check for overfitting without submitting to Kaggle.

set.seed(123)
split <- sample.split(trainDF, SplitRatio = 0.7)
train <- subset(trainDF, split == TRUE)
test <- subset(trainDF, split == FALSE)


# Now I will create a matrix of resized images... instead of 20 by 20 pixels, we will resize the images to 10 by 10 pixels.
# Cutting the dimensions in half reduces the total data by 75%, which will help us run models more quickly.

trainDF_temp = trainDF # make a copy of trainDF just in case i mess it up
trainDF_temp
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
bmp("E://plots//image_processing_comparison.bmp")
op = par(mfrow = c(2,2))
row_num = 1342
m = matrix(unlist(trainDF[row_num, -c(1)]), nrow = 20, byrow = F)
image(m,col=grey.colors(255), xlab = trainDF$Class[row_num], main = "Original (20 by 20)")
m = matrix(unlist(trainDF_downsampled[row_num, -c(1)]), nrow = 10, byrow = F)
image(m,col=grey.colors(255), xlab = trainDF_downsampled$Class[row_num], main = "Downsampling (10 by 10)")
m = matrix(unlist(trainDF_threshold[row_num, -c(1)]), nrow = 20, byrow = F)
image(m,col=grey.colors(255), xlab = trainDF_threshold$Class[row_num], main = "Thresholding (20 by 20)")
m = matrix(unlist(trainDF_downsampled_thresh[row_num, -c(1)]), nrow = 10, byrow = F)
image(m,col=grey.colors(255), xlab = trainDF_downsampled_thresh$Class[row_num], main = "Thresholding (10 by 10)")
par(op)
dev.off()


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

