# Predicting Label from pixel data
# You will need dataframes from the image_processing.R script to run these predictions.


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

library(class)

###======== KNN with k=3 ==============
set.seed(111)
knn.pred <- knn(train[,-1], test[,-1], cl = train$Class, k=3, prob = TRUE)
View(knn.pred)
png(file="E://data//plots//knn3.png",    width=600, height=350)
plot(knn.pred, main="KNN - (k= 3)")
dev.off()
mean(knn.pred != test$Class) # 61% error rate, 39% accuracy on test set
View(as.data.frame.matrix(table(knn.pred, test$Class))) # classification table
# k=3: # 66.5% error rate, 33.5% accuracy on test set
# 67 %
# dependency in previous file ######
set.seed(111)
knn.pred = knn(trainWide[,-1], testWide[,-1], cl = trainWide$Class, k=3, prob = TRUE)
mean(knn.pred == testWide$Class) 
View(as.data.frame.matrix(table(knn.pred, test$Class)))


set.seed(111)
knn.pred = knn(trainDownsamp[,-1], testDownsamp[,-1], cl = trainDownsamp$Class, k=3, prob = TRUE)
mean(knn.pred == testDownsamp$Class) 
View(as.data.frame.matrix(table(knn.pred, testDownsamp$Class)))
#33%
set.seed(111)
knn.pred = knn(trainThresh[,-1], testThresh[,-1], cl = trainThresh$Class, k=3, prob = TRUE)
mean(knn.pred == testThresh$Class) 
View(as.data.frame.matrix(table(knn.pred, testThresh$Class)))
#47%
set.seed(111)
knn.pred = knn(trainDownsampThresh[,-1], testDownsampThresh[,-1], cl = trainDownsampThresh$Class, k=3, prob = TRUE)
mean(knn.pred == testDownsampThresh$Class) 
View(as.data.frame.matrix(table(knn.pred, testDownsampThresh$Class)))
#43%
# which value of k is best??
set.seed(333)
error = rep(0,35)
#takes about 5 - 10 min for k=1:35
for (i in 1:35) {
  knn.pred = knn(train[,-1], test[,-1], train$Class, k = i)
  error[i] = mean(knn.pred != test$Class)
}

plot(1:35, error, type = "l")
# x and y differ ## not working one line below
plot(1:100, error, type = "l")
error[which.min(error)]
# 61 %
knn.pred = knn(train[, -1], test[, -1], train$Class, k = 1)
#knn.pred

###========= random forest ============
library(randomForest)
set.seed(123)

# original data -- takes time
rf.fit = randomForest(Class ~ ., ntree=500, data=train) # default mtry = sqrt(p), ntree = 500

png(file="E://data//plots//randomForest.png",    width=600, height=350)
plot(rf.fit, main="Random Forest")
dev.off()
#rf.fit
# Make predictions:
predictRF = predict(rf.fit, newdata=test, type = "response")

View(as.data.frame.matrix(table(predictRF, test$Class)))
mean(predictRF == test$Class)
#44%
# 10 by 10
rf.fit = randomForest(Class ~ ., ntree=500, data=trainDownsamp) # default mtry = sqrt(p), ntree = 500

# Make predictions:
predictRF = predict(rf.fit, newdata=testDownsamp, type = "response")

View(as.data.frame.matrix(table(predictRF, testDownsamp$Class)))
mean(predictRF == testDownsamp$Class)
#42%
# Threshold
rf.fit = randomForest(Class ~ ., ntree=500, data=trainThresh) # default mtry = sqrt(p), ntree = 500
png(file="E://data//plots//randomForest56.png",    width=600, height=350)
plot(rf.fit, main="Random Forest Threshold")
dev.off()
# Make predictions:
predictRF = predict(rf.fit, newdata=testThresh, type = "response")
write.csv(predictRF, file = "E://data//class//predictionsRF.csv", row.names = TRUE)
#View(as.data.frame.matrix(table(predictRF, testThresh$Class)))
mean(predictRF == testThresh$Class)
#56%
# 10 by 10 with Threshold
rf.fit = randomForest(Class ~ ., ntree=500, data=trainDownsampThresh) # default mtry = sqrt(p), ntree = 500

# Make predictions:
predictRF = predict(rf.fit, newdata=testDownsampThresh, type = "response")

View(as.data.frame.matrix(table(predictRF, testDownsampThresh$Class)))
mean(predictRF == testDownsampThresh$Class)

# All data R, G, B Concatenated
rf.fit = randomForest(Class ~ ., ntree=500, data=trainWide) # default mtry = sqrt(p), ntree = 500
png(file="E://data//plots//randomForestAllData.png",    width=600, height=350)
plot(rf.fit, main="Random Forest All Data")
dev.off()
# Make predictions:
predictRF = predict(rf.fit, newdata=testWide, type = "response")
write.csv(predictRF, file = "E://data//class//predictionsAllRF.csv", row.names = TRUE)

View(as.data.frame.matrix(table(predictRF, testWide$Class)))
mean(predictRF == testWide$Class)


###=========== GBM ================
library(dplyr)
#install.packages("gbm")
library(gbm)
# Make smaller training and testing sets for the gbm because it's such a computationally intensive process
 set.seed(163)
train_small = sample_n(train, 1000)
test_small = sample_n(test, 500)
train_labels_small = train_small[,1]
test_labels_small = test_small[,1]

####### takes 10-15 mins ##########
gbm.mod = gbm(Class ~ ., data = train_small, distribution = "multinomial", n.trees = 5000, interaction.depth = 4, verbose = TRUE)

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
write.csv(predDF, file = "E://data//class//predictionsGBM.csv", row.names = TRUE)

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
conf.h2o.gbm <- h2o.confusionMatrix(gbm1)
conf.h2o.gbm
View(h2o.confusionMatrix(gbm1))
h2o.hit_ratio_table(gbm1,valid = T)[1,2]
# 32%
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
#28%

## classify test set
h2o_y_test = h2o.predict(gbm1, test_h2o)
preds = as.vector(h2o_y_test$predict)

#testPreds = data.frame(testDF$ID, preds)
#testPreds = testPreds %>% dplyr::rename(ID = testDF.ID, Class = preds)
#write_csv(testPreds, path = "C:/Users/bhart/Dropbox/MSSP/Kaggle Streetview Image Classification/nn_submission.csv")

mean(preds == test$Class) 
#32%
View(as.data.frame.matrix(table(preds, test$Class)))

## shut down virutal H2O cluster
h2o.shutdown(prompt = F)



### ===== Support Vector Machines ===== ###
library("e1071")
library(h2o)
localH2O = h2o.init(max_mem_size = '6g', # use 4GB of RAM of *GB available
                    nthreads = -1) # use all CPUs (8 on my personal computer)

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
