# Load Data

library(readr)
library(bmp)
library(pixmap)
library(dplyr)
#install.packages("readbitmap")
#library(readbitmap)
library(rgdal)

# location of train and test images
train_files = list.files(path="E:\\data\\train\\train\\.", pattern=".Bmp",all.files=T, full.names=T, no.. = T) 
test_files = list.files(path="E:\\data\\test\\test\\.", pattern=".Bmp",all.files=T, full.names=T, no.. = T)

# we will create a matrix with a row for each image.
# since images are 20 by 20 pixels, our new matrix will have 400 columns of pixel data.
train_images = list(rep(0,length(train_files)))
#train_images
train_mat = matrix(ncol = 401, nrow = length(train_files))

#train_mat1 = matrix(ncol = 401, nrow = length(train_files))


for(i in 1:6283){
  train_mat[i,1] = i
}

#View(train_mat)
# ID = unlist(strsplit(train_files[1], split = ".", fixed=T))[2]
# ID
# # /1
# #ID2 = as.numeric(unlist(strsplit(ID, split = "/"))[2])
# ID2 = as.vector(unlist(strsplit(ID, split = "/"))[2])
# ID2
# # 1

#write.csv(train_mat)
#write.csv(train_mat, file = "E:\\data\\MyTrainMatInitData.csv")
#length(train_files)
#View(as.data.frame(train_files[[1]]))
#train_mat
#View(train_mat)
  stp <- strsplit(train_files[1], split = ".", fixed=T)
# # # 
# # # stp
# # # 
  ulist <- unlist(stp)[2]
train_images[[1]]= readGDAL(train_files[1])
train_images[[1]]
img = train_images[[1]]
  
  View(img)
# # # ulist
# # 
  r = as.data.frame(img[ , , 1])
 View(r)
  g = as.data.frame(img[ , , 2])
  View(g)
 b = as.data.frame(img[ , , 3])
                   temp1 = (r + g + b)/3
                   
                   View(temp1)
# # #temp
# # # temp = r 
  temp2 = as.vector(temp1)
  temp2 = c(ulist, temp1)
 temp2 = as.vector(temp1)
 View(temp2)
# # #write.csv(temp, file = "E:\\data\\MyTrainTempInit.csv")
# # View(temp)
# # summary(temp$band1)
# for(i in 2:401)
# train_mat[1,i] = temp$band1[i]
#train_mat[i,2:401] = temp$band1[1:400]
# View(temp$band1)
# View(train_mat)

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
  #  train_mat[i,2:401] = temp$band1[1:400]
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

View(train_mat)
# check dimensions of matrix and to make sure everything loaded properly (no NAs)
dim(train_mat)
#c <- sum(is.na(train_mat))
#c

View(train_mat)
sum(is.na(train_mat))

count <- 0
count1 <- 0
count2 <- 0
count3 <- 0
# and for the test images also
test_images = list(rep(0,length(test_files)))
test_mat = matrix(ncol = 401, nrow = length(test_files))

for(i in 1:6220){
  test_mat[i,1] = i
}

View(test_mat)

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
    for(j in 2:401){
      test_mat[i,j] = temp$band1[j]
      #  train_mat[i,2:401] = temp$band1[1:400]
    }
   # test_mat[i,] = temp
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
count3


# check dimensions of matrix and to make sure everything loaded properly (no NAs)
dim(test_mat)
#d <- sum(is.na(test_mat))
sum(is.na(test_mat))
# convert matrices do data frames and delete other matrices, vectors, and lists from memory
trainDF = as.data.frame(train_mat)
trainDF = trainDF %>% dplyr::rename(ID = V1)

View(trainDF)
testDF = as.data.frame(test_mat)
testDF = testDF %>% dplyr::rename(ID = V1)

View(testDF)
# ## remove (almost) everything in the working environment.
## You will get no warning, so don't do this unless you are really sure. New thing learnt
rm(r, g, b, temp, i, img, ID, ID2, train_mat, test_mat, train_images, test_images)

# Read in the training labels and merge with the trainDF
trainLabels = read_csv("E:\\data\\trainLabels.csv", col_types = cols(ID = col_character()))
trainLabels$ID = as.numeric(trainLabels$ID)
#class(trainLabels$ID)
trainDF = merge(x = trainDF, y = trainLabels, by = "ID") # merge by common ID columns
View(trainDF)
trainDF = trainDF[, c(1,402,2:401)] # rearrange columns in dataframe so that ID is first, Class is second, ...
View(trainDF)
trainDF$Class = as.factor(trainDF$Class)
class(trainDF$Class)
# Duplicates - change indices to 3:402 since I just loaded the data
dup1 = which(duplicated(trainDF[,3:402]))
View(dup1)
dup2 = which(duplicated(trainDF[,3:402], fromLast = TRUE))
View(dup2)
dup_rows = sort(c(dup1, dup2))
length(dup_rows)
# very powerful 
trainDF[dup_rows,1:5]

# after analysis,I have to find now outliers
write.csv(trainDF, "E:\\data\\trainDF_BeforeOutliers_CSV.csv")
# From the CSV of the RDS I had of Raw Data, the below analysis was done
# row 3055 is classified as a "1",  but it has the same pixel values as row 4106, which is classified as an "E"
# it's definitely a 1, so we will remove row 4106
trainDF = trainDF[-c(2297, 3226, 4106, 4598, 4820, 5208, 6270), ]
nrow(trainDF)
# display image of specified row of trainDF -- testing testing..
#row_num = 4165
row_num = 10
m = matrix(unlist(trainDF[row_num,-c(1,2)]),nrow = 20,byrow = F)
View(m)
image(m,col=grey.colors(255), xlab = trainDF$Class[row_num])

# everything checks out, so I will remove the ID column
trainDF = trainDF[,-1]
rm(trainLabels)

# write trainDF and testDF to R Data Sets
saveRDS(testDF, "E:\\data\\testDF2.RDS")
saveRDS(trainDF, "E:\\data\\trainDF2.RDS")


dataRead <- readRDS("E:\\data\\trainDF2.RDS")
dataReadTest <- readRDS("E:\\data\\testDF2.RDS")
# View(dat)
# summary(dat)
# dataRead[1,]
# summary(dat[1,2:401])
#  cdata <- dataRead[1,2:401]
#  vector.cdata <- c(cdata)
# summary(vector.cdata)
# View(vector.cdata)
csv.train.frame <- as.data.frame(dataRead)
View(csv.train.frame)

 write.csv(csv.train.frame, "E:\\data\\trainDF2_CSV.csv")
 csv.test.frame <- as.data.frame(dataReadTest)
 write.csv(csv.test.frame, "E:\\data\\testDF2_CSV.csv")
 
 View(dataReadTest)
