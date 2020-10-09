# --------------------------- Initialization configurations ---------------------------
#
# path to the training dataset release 2.0
PATH_OF_TRAINING_DATASET <- "./data/eclipse-metrics-packages-2.0.csv"
# path to the test dataset release 3.0
PATH_OF_TEST_DATASET <- "./data/eclipse-metrics-packages-3.0.csv"

# train the new models everytime, and display the results
USE_TRAINED_MODELS <- FALSE
# if it is configured to train the new models everytime, then we can configure to save the new models everytime
SAVE_TRAINED_MODELS <- FALSE
# if it is configured to save the new trained models everytime, then the version of models = current timestamp
VERSION_OF_TRAINED_MODELS <- gsub(":", "-", gsub(" ", "_", Sys.time()))

# use the already-trained models, if the same predictions and results will be used in the data analysis in the report
# USE_TRAINED_MODELS <- TRUE
# PATH_OF_SINGLE_TREE <- paste("./models/", "tree_single_2020-10-09_15-46-28.RData", sep = "")
# PATH_OF_BAGGING <- paste("./models/", "tree_bagging_2020-10-09_15-46-28.RData", sep = "")
# PATH_OF_RANDOM_FOREST <- paste("./models/", "tree_random_2020-10-09_15-46-28.RData", sep = "")

# --------------------------- Load the dependent functions ---------------------------
source("tree.R")

# --------------------------- Data analysis ---------------------------
# 
# --------------------------- Load the training and test dataset ---------------------------
# read semicolon-separated files
# the training set is from "release 2.0"
train.data.frame <- read.csv(PATH_OF_TRAINING_DATASET, sep = ";") 
# only select FOUT, MLOC, ..., pre and post columns
train.data.frame <- train.data.frame[ ,c('FOUT_avg', 'FOUT_max', 'FOUT_sum', 'MLOC_avg', 'MLOC_max', 'MLOC_sum', 'NBD_avg', 'NBD_max', 'NBD_sum', 'PAR_avg', 'PAR_max', 'PAR_sum', 'VG_avg', 'VG_max', 'VG_sum', 'NOF_avg', 'NOF_max', 'NOF_sum', 'NOM_avg', 'NOM_max', 'NOM_sum', 'NSF_avg', 'NSF_max', 'NSF_sum', 'NSM_avg', 'NSM_max', 'NSM_sum', 'ACD_avg', 'ACD_max', 'ACD_sum', 'NOI_avg', 'NOI_max', 'NOI_sum', 'NOT_avg', 'NOT_max', 'NOT_sum', 'TLOC_avg', 'TLOC_max', 'TLOC_sum', 'NOCU', 'pre', 'post')]
train.data.frame <- data.matrix(train.data.frame)

x.train <- train.data.frame[,1:41]
y.train <- train.data.frame[,42]
y.train[y.train!=0] = 1

# the test set is from "release 3.0"
test.data.frame <- read.csv(PATH_OF_TEST_DATASET, sep = ";")
# only select FOUT, MLOC, ..., pre and post columns
test.data.frame <- test.data.frame[ ,c('FOUT_avg', 'FOUT_max', 'FOUT_sum', 'MLOC_avg', 'MLOC_max', 'MLOC_sum', 'NBD_avg', 'NBD_max', 'NBD_sum', 'PAR_avg', 'PAR_max', 'PAR_sum', 'VG_avg', 'VG_max', 'VG_sum', 'NOF_avg', 'NOF_max', 'NOF_sum', 'NOM_avg', 'NOM_max', 'NOM_sum', 'NSF_avg', 'NSF_max', 'NSF_sum', 'NSM_avg', 'NSM_max', 'NSM_sum', 'ACD_avg', 'ACD_max', 'ACD_sum', 'NOI_avg', 'NOI_max', 'NOI_sum', 'NOT_avg', 'NOT_max', 'NOT_sum', 'TLOC_avg', 'TLOC_max', 'TLOC_sum', 'NOCU', 'pre', 'post')]
test.data.frame <- data.matrix(test.data.frame)

x.test <- test.data.frame[,1:41]
y.test <- test.data.frame[,42]
y.test[y.test!=0] = 1

# --------------------------- The single classification tree ---------------------------

if(USE_TRAINED_MODELS) {
  # load the trained model
  load(file = PATH_OF_SINGLE_TREE)
  
} else {
  # train the single classification tree on the training set
  nmin <- 15
  minleaf <- 5
  nfeat <- 41
  tr.single <- tree_grow(x.train, y.train, nmin, minleaf, nfeat)
  
  # save the trained model
  if(SAVE_TRAINED_MODELS) {
    save(tr.single, file=paste("./models/tree_single_", VERSION_OF_TRAINED_MODELS,".RData", sep = ""))
  }
}

# predict based on the single classification tree on the test set
tr.single.preds <- tree_pred(x.test, tr.single)

# performance result of confusion matrix, precision, recall and accuracy for the single classification tree
result.single <- quality_measure(y.test, tr.single.preds)

# --------------------------- The classification tree by bagging ---------------------------
if(USE_TRAINED_MODELS) {
  # load the trained model
  load(file = PATH_OF_BAGGING)
  
} else {
  # train the classification tree by bagging on the training set
  m <- 100
  tr.bagging <- tree_grow_b(x.train, y.train, nmin, minleaf, nfeat, m)
  
  # save the trained model
  if(SAVE_TRAINED_MODELS) {
    save(tr.bagging, file=paste("./models/tree_bagging_", VERSION_OF_TRAINED_MODELS,".RData", sep = ""))
  }
}

# predict based on the classification tree by bagging on the test set
tr.bagging.preds <- tree_pred_b(x.test, tr.bagging)

# performance result of confusion matrix, precision, recall and accuracy for the classification tree by bagging
result.bagging <- quality_measure(y.test, tr.bagging.preds)

# --------------------------- The classification tree by random forest ---------------------------
if(USE_TRAINED_MODELS) {
  # load the trained model
  load(file = PATH_OF_RANDOM_FOREST)
  
} else {
  # train the classification tree by random forest on the training set
  nfeat <- 6
  tr.random.forest <- tree_grow_b(x.train, y.train, nmin, minleaf, nfeat, m)
  
  # save the trained model
  if(SAVE_TRAINED_MODELS) {
    save(tr.random.forest, file=paste("./models/tree_random_", VERSION_OF_TRAINED_MODELS,".RData", sep = ""))
  }
}

# predict based on the classification tree by random forest on the test set
tr.random.forest.preds <- tree_pred_b(x.test, tr.random.forest)

# performance result of confusion matrix, precision, recall and accuracy for the classification tree by random forest
result.random.forest <- quality_measure(y.test, tr.random.forest.preds)

# --------------------------- Print the performance results of all three tree models ---------------------------

print("quality measure of the single classification tree")
print(result.single)

print("quality measure of the classification tree by bagging")
print(result.bagging)

print("quality measure of the classification tree by random forest")
print(result.random.forest)