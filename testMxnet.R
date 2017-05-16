install.packages("doParallel")
library("doParallel")
install.packages("foreach")
library("foreach");
library("mxnet");
library("imager");
library("data.table");
library("nnet"); #just used for class.ind
require(mlbench)
library("mlbench")


#prep id's targets and data
driver_details<-read.csv("C:\\Users\\chand\\Downloads\\driver_imgs_list.csv\\driver_imgs_list.csv");


# helper function
# normalizes log loss preds so that sum is always 1
# probably not needed here, but anyway
mLogLoss.normalize = function(p, min_eta=1e-15, max_eta = 1.0){
  #min_eta
  for(ix in 1:dim(p)[2]) {
    p[,ix] = ifelse(p[,ix]<=min_eta,min_eta,p[,ix]);
    p[,ix] = ifelse(p[,ix]>=max_eta,max_eta,p[,ix]);
  }
  #normalize
  for(ix in 1:dim(p)[1]) {
    p[ix,] = p[ix,] / sum(p[ix,]);
  }
  return(p);
}

# helper function
#calculates logloss
mlogloss = function(y, p, min_eta=1e-15,max_eta = 1.0){
  class_loss = c(dim(p)[2]);
  loss = 0;
  p = mLogLoss.normalize(p,min_eta, max_eta);
  for(ix in 1:dim(y)[2]) {
    p[,ix] = ifelse(p[,ix]>1,1,p[,ix]);
    class_loss[ix] = sum(y[,ix]*log(p[,ix]));
    loss = loss + class_loss[ix];
  }
  #return loss
  return (list("loss"=-1*loss/dim(p)[1],"class_loss"=class_loss));
}

# mxnet specific logloss metric
mx.metric.mlogloss <- mx.metric.custom("mlogloss", function(label, pred){
  p = t(pred);
  m = mlogloss(class.ind(label),p);
  gc();
  return(m$loss);
})


# settings for image loaders
img_height = 32;
img_width = 32;
use_rgb = FALSE;
# num of colors here, must be 3 if u r using rgb, 1 if use_rgb  = FALSE
num_channels = 1;

# will load image and convert it to grayscale (optionaly)
load.and.prep.img = function(tf){
  im = load.image(tf);
  #convert to gray scale
  #if(!use_rgb){
  #  im = 0.2989 * channel(im,1) + 0.5870 * channel(im,2) + 0.1140 * channel(im,3) 
  #}
  im=grayscale(im)
  #im = resize(im,img_width,img_height);
  return(im);
}


# max files to load
# set this to large number like 10000
# when doing full train
max_train_images_to_load = 100;

# loading images in parallel
# set cpu cores to what you have at disposal
# and watch for RAM, 4 cores should do just fine
# with 8 gb of RAM
cpu.cores = 6;
cl <- makeCluster(cpu.cores); 
registerDoParallel(cl);
train_img_matrix = 
  foreach(cls = 0:9, .packages=c('imager'), .combine=rbind, .multicombine=T) %dopar% {
    train_files = list.files(paste0("imgs/train/c", cls, "/"),"*.*",full.names = T);
    train_files = train_files[1:max_train_images_to_load];
    targets = c();    
    m = data.frame(matrix(0,nrow=length(train_files),ncol=img_width*img_height*num_channels));
    mi = 1;
    for(tf in train_files){
      m[mi,] = as.numeric(load.and.prep.img(tf));
      targets[mi] = cls;
      mi = mi + 1;
    }             
    df = data.frame(m,stringsAsFactors = FALSE);
    df = cbind("target"=targets,df);
    df = cbind("file"=train_files,df);
    return(df);
  }
stopCluster(cl);

#do the same for test images
test_img_matrix = NULL;
test_id = NULL;
#enable it for submission
if(TRUE){
  # test part
  cl <- makeCluster(cpu.cores); 
  registerDoParallel(cl);
  test_files = data.frame("file"=list.files("imgs/test/","*.*",full.names = T),stringsAsFactors = FALSE);
  test_files$rid = 1:nrow(test_files);
  test_files$rid = test_files$rid%%7;
  test_img_matrix = 
    foreach(cls = 0:max(test_files$rid), .packages=c('imager'), .combine=rbind, .multicombine=T) %dopar% {
      t_files = test_files[test_files$rid==cls,"file"];
      m = data.frame(matrix(0,nrow=length(t_files),ncol=img_width*img_height*num_channels));
      mi = 1;
      for(tf in t_files){
        m[mi,] = as.numeric(load.and.prep.img(tf));
        mi = mi + 1;
      }             
      df = data.frame(m,stringsAsFactors = FALSE);
      df = cbind("file"=t_files,df);
      return(df);
    }
  stopCluster(cl);
  test_id = test_img_matrix[,1];
  test_img_matrix = test_img_matrix[,2:ncol(test_img_matrix)];
  
  #transpose and set dim on test part
  test_img_matrix = t(test_img_matrix);
  dim(test_img_matrix) = c(img_width, img_height, num_channels, ncol(test_img_matrix));
}

#prep id's targets and data
driver_details = read.csv("imgs/driver_imgs_list.csv");
imgm = train_img_matrix;
imgm = imgm[order(sample(nrow(imgm))),];
y = imgm$target;
files = basename(as.character(imgm$file))
imgm = imgm[,3:ncol(imgm)];
mx = match(files, driver_details$img)
tgt = as.character(driver_details[mx,"classname"])

# just to check if classes match by filename
table(tgt==paste0("c",y));
# used to split train test so that driver is always in test or train, never in both
subject = as.numeric(as.factor(driver_details[mx,"subject"]))

print(table(subject));

#net configuration below
# input
data = mx.symbol.Variable('data')
conv1 = mx.symbol.Convolution(data=data, kernel=c(3,3), num_filter=20)
tanh1 = mx.symbol.Activation(data=conv1, act_type="tanh")
pool1 = mx.symbol.Pooling(data=tanh1, pool_type="max",
                          kernel=c(2,2), stride=c(2,2))

conv2 = mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=50)
tanh2 = mx.symbol.Activation(data=conv2, act_type="tanh")
pool2 = mx.symbol.Pooling(data=tanh2, pool_type="max",
                          kernel=c(2,2), stride=c(2,2))
flatten = mx.symbol.Flatten(data=pool2)
fc1 = mx.symbol.FullyConnected(data=flatten, num_hidden=500)
tanh3 = mx.symbol.Activation(data=fc1, act_type="tanh")
fc2 = mx.symbol.FullyConnected(data=tanh3, num_hidden=10)
net = mx.symbol.SoftmaxOutput(data=fc2)



# portion of train samples going to fold train
# rest goes for validation
# it's not statified by target, should be corrected
train_test_ratio = 0.8;
test_pred = NULL;
set.seed(1905);
res = c();
# number of runs 80/20 if ratio is left as is (0.8)
nfold = 3;
for(foldId in 1:nfold){
  smpl = sample(max(subject),floor(max(subject)*train_test_ratio))
  g_train = imgm[subject%in%smpl,];
  y_train = y[subject%in%smpl];
  g_test = imgm[!(subject%in%smpl),];
  y_test = y[!(subject%in%smpl)];
  
  # transpose - convert to column major
  g_train = t(g_train);
  g_test = t(g_test);
  
  # set the array dimension
  # this is important, it's width, height, channels, samples
  # required for conv layers
  dim(g_train) = c(img_width, img_height, num_channels, ncol(g_train));
  dim(g_test) = c(img_width, img_height, num_channels, ncol(g_test));
  
  start.time <- Sys.time();
  #mxnet here
  mx.set.seed(0);
  
  #switch here to gpu use: device = mx.gpu();
  device = mx.gpu();
  model <- mx.model.FeedForward.create(
    X                  = g_train,
    y                  = y_train,
    eval.data          = list("data"=g_test,"label"=y_test),
    ctx                = device,
    symbol             = net,
    eval.metric        = mx.metric.mlogloss,
    num.round          = 3,
    learning.rate      = 0.05,
    momentum           = 0.01,
    wd                 = 0.0001,
    initializer=mx.init.uniform(0.1),
    array.batch.size   = 100,
    epoch.end.callback = mx.callback.save.checkpoint("statefarm"),
    batch.end.callback = mx.callback.log.train.metric(100),
    array.layout="columnmajor"
  );
  
  p = t(predict(model,g_test));
  m = mlogloss(class.ind(y_test),p);
  print(m);
  res[foldId] = m$loss;
  
  # here we can predict on full test 
  # and blend it, just a way to ensemble multiple
  # runs
  if(!is.null(test_img_matrix)){
    p_T = t(predict(model,test_img_matrix));
    if(is.null(test_pred)){
      test_pred = p_T;
    } else {
      test_pred = test_pred + p_T;
    }
  }
  
  end.time = Sys.time();
  time.taken <- end.time - start.time
  print(paste("net run time:",time.taken));
  model = NULL;
  gc();
}
print(paste("mean",mean(res),"sd",sd(res)));

if(!is.null(test_pred)){
  test_pred = test_pred /  foldId;
  #make a submission
  df = data.frame(test_pred); colnames(df) = c("c0","c1","c2","c3","c4","c5","c6","c7","c8","c9");
  df = cbind("img"=test_id,df);
  write.csv(df,"submission.csv",quote=F,row.names=F);
}      