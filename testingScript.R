# Load Data

library(readr)
library(bmp)
library(pixmap)
library(dplyr)
#install.packages("readbitmap")
#library(readbitmap)

# location of train and test images
train_files = list.files(path="E:\\data\\train\\train\\.", pattern=".Bmp",all.files=T, full.names=T, no.. = T) 
test_files = list.files(path="E:\\data\\test\\test\\.", pattern=".Bmp",all.files=T, full.names=T, no.. = T)

# we will create a matrix with a row for each image.
# since images are 20 by 20 pixels, our new matrix will have 400 columns of pixel data.
train_images = list(rep(0,length(train_files)))
#train_images
train_mat = matrix(ncol = 401, nrow = length(train_files))

#write.csv(train_mat)
#write.csv(train_mat, file = "E:\\data\\MyTrainMatInitData.csv")
#length(train_files)
#View(as.data.frame(train_files[[1]]))
#train_mat
#View(train_mat)
stp <- strsplit(train_files[1], split = ".", fixed=T)
# 
# stp
# 
ulist <- unlist(stp)[2]
# ulist

r = as.data.frame(img[ , , 1])
g = as.data.frame(img[ , , 2])
b = as.data.frame(img[ , , 3])
temp = (r + g + b)/3
#temp
# temp = r 
temp = as.vector(temp)
temp = c(ulist, temp)
temp = as.vector(temp)
#write.csv(temp, file = "E:\\data\\MyTrainTempInit.csv")
View(temp)
summary(temp$band1)
for(i in 2:401)
  train_mat[1,i] = temp$band1[i]
View(temp$band1)
View(train_mat)

count <- 0
count1 <- 0
count2 <- 0
count3 <- 0
for(i in 1:nrow(train_mat)) {
  ID = unlist(strsplit(train_files[i], split = ".", fixed=T))[2]
  #ID2 = as.numeric(unlist(strsplit(ID, split = "/"))[2])
  ID2 = as.vector(unlist(strsplit(ID, split = "/"))[2])
  train_images[[i]]= readGDAL(train_files[i])
  img = train_images[[i]]
  if (length(dim(img)) == 3) {
    # r = img[ , , 1]
    # g = img[ , , 2]
    # b = img[ , , 3]
    # temp = (r + g + b)/3
    # temp = as.vector(temp)
    # temp = c(ID2, temp)
    # train_mat[i,] = temp
    count3 <- count3 + 1
  }
  else if (length(dim(img)) == 2) {
    count <- count + 1
    r = as.data.frame(img[ , , 1])
    g = as.data.frame(img[ , , 2])
    b = as.data.frame(img[ , , 3])
    temp = (r + g + b)/3
    # temp = r 
    temp = as.vector(temp)
    temp = c(ID2, temp)
    for(j in 2:401){
      train_mat[i,j] = temp$band1[j]
    }
    #temp = as.vector(img[ , , ])
    #temp = c(ID2, temp)
    #train_mat[i,] = temp
  }
  else if (length(dim(img)) == 1) {
    count1 <- count1 + 1
    
    #temp = as.vector(img[ , , ])
    #temp = c(ID2, temp)
    #train_mat[i,] = temp
  } else if (length(dim(img)) == 0) {
    count2 <- count2 + 1
    #temp = as.vector(img[ , , ])
    #temp = c(ID2, temp)
    #train_mat[i,] = temp
  }
  
}

class(ID2)
count
count1
count2
count3
# check dimensions of matrix and to make sure everything loaded properly (no NAs)
dim(train_mat)
c <- sum(is.na(train_mat))
c

count <- 0
count1 <- 0
count2 <- 0
count3 <- 0
# and for the test images also
test_images = list(rep(0,length(test_files)))
test_mat = matrix(ncol = 401, nrow = length(test_files))
for(i in 1:nrow(test_mat)) {
  ID = unlist(strsplit(test_files[i], split = ".", fixed=T))[2]
  ID2 = as.numeric(unlist(strsplit(ID, split = "/"))[2])
  test_images[[i]] = readGDAL(test_files[i])
  img = test_images[[i]]
  if (length(dim(img)) == 3) {
    # r = as.data.frame(img[ , , 1])
    # g = as.data.frame(img[ , , 2])
    # b = as.data.frame(img[ , , 3])
    # temp = (r + g + b)/3
    # temp = as.vector(temp)
    # temp = c(ID2, temp)
    # test_mat[i,] = temp
    count3 <- count3 + 1
  }
  else if (length(dim(img)) == 2) {
    count <- count + 1
    r = as.data.frame(img[ , , 1])
    g = as.data.frame(img[ , , 2])
    b = as.data.frame(img[ , , 3])
    temp = (r + g + b)/3
    temp = as.vector(temp)
    temp = c(ID2, temp)
    test_mat[i,] = temp
    #temp = as.vector(img[ , , ])
    #temp = c(ID2, temp)
    #train_mat[i,] = temp
  }
  else if (length(dim(img)) == 1) {
    count1 <- count1 + 1
    
    #temp = as.vector(img[ , , ])
    #temp = c(ID2, temp)
    #train_mat[i,] = temp
  } else if (length(dim(img)) == 0) {
    count2 <- count2 + 1
    #temp = as.vector(img[ , , ])
    #temp = c(ID2, temp)
    #train_mat[i,] = temp
  }
  
}
count
count1
count2
# check dimensions of matrix and to make sure everything loaded properly (no NAs)
dim(test_mat)
d <- sum(is.na(test_mat))

# convert matrices do data frames and delete other matrices, vectors, and lists from memory
trainDF = as.data.frame(train_mat)
trainDF = trainDF %>% dplyr::rename(ID = V1)
testDF = as.data.frame(test_mat)
testDF = testDF %>% dplyr::rename(ID = V1)
rm(r, g, b, temp, i, img, ID, ID2, train_mat, test_mat, train_images, test_images)

# Read in the training labels and merge with the trainDF
trainLabels = read_csv("E:\\data\\trainLabels.csv", col_types = cols(ID = col_character()))
trainLabels$ID = as.numeric(trainLabels$ID)
trainDF = merge(x = trainDF, y = trainLabels, by = "ID") # merge by common ID columns
trainDF = trainDF[, c(1,402,2:401)] # rearrange columns in dataframe so that ID is first, Class is second, ...
trainDF$Class = as.factor(trainDF$Class)

# Duplicates - change indices to 3:402 since I just loaded the data
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

# everything checks out, so I will remove the ID column
trainDF = trainDF[,-1]
rm(trainLabels)

# write trainDF and testDF to R Data Sets
saveRDS(testDF, "E:\\data\\testDF2.RDS")
saveRDS(trainDF, "E:\\data\\trainDF2.RDS")


