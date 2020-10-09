# --------------------------- Load the dependent libraries ---------------------------
library(tidyverse)
library(pwr)

# --------------------------- Initialization configurations ---------------------------
#
PATH_OF_SINGLE_TREE <- paste("./models/", "tree_single.RData", sep = "")
PATH_OF_BAGGING <- paste("./models/", "tree_bagging.RData", sep = "")
PATH_OF_RANDOM_FOREST <- paste("./models/", "tree_random.RData", sep = "")

# --------------------------- Load the dependent functions ---------------------------
#
# --------------------------- Function tree_pred ---------------------------
# function name: 
#   tree_pred
# 
# input arguments:
#   x: vector/matrix (all the columns of the numeric predictors spanned across one or more than one rows)
#   tr: list of vectors (the model of the classification tree)
# 
# returned result:
#   preds: integer of 1 or 0/vector of 1s and 0s (all the predictions)
# 
# function description:
#   predict the class label based on the model of the tree returned by the function tree_grow
tree_pred <- function(x, tr) {
  
  if(is.null(nrow(x))) {
    # if x contains only one row
    label <- tree_pred_by_one_row(x, tr)
    # return the prediction
    label
  } else {
    # if x contains more than one rows
    # predictions of all the rows from x
    preds <- c()
    
    for(x.row.index in 1:nrow(x)) {
      # pick one row from x
      x.row <- x[x.row.index,]
      
      label <- tree_pred_by_one_row(x.row, tr)
      # add the prediction to the predictions
      preds <- c(preds, label)
    }
    # return all the predictions
    preds
  }
}

# --------------------------- Function tree_grow_b ---------------------------
# function name: 
#   tree_grow_b
# 
# input arguments:
#   x: matrix (all the columns of the numeric predictors)
#   y: vector (one column of the actual labels)
#   nmin: integer (the minimum number of observations that a node must contain)
#   minleaf: integer (the minimum number of observations required for a leaf node)
#   nfeat: integer (the number of predictors used to grow the tree)
#   m: the number of bootstrap samples to be drawn
# 
# returned result:
#   trees: list of list of vectors (a list of m models of the classification trees)
# 
# function description:
#   grow the classification tree by the bagging with random forest
#   training set is drawn with replacement from x with split 0.9
tree_grow_b <- function(x, y, nmin, minleaf, nfeat, m) {
  # a list of m different trees
  trees <- list()
  tree.index <- 0
  
  # draw m samples with replacement from x
  for(i in 1:m) {
    # draw a sample (training set) with replacement from x
    indices <- sample(length(y), round(length(y)*0.9), replace = TRUE)
    
    # grow a tree on this sample
    tr <- tree_grow(x[indices,], y[indices], nmin, minleaf, nfeat)
    
    # append this tree to the list of trees
    tree.index <- tree.index + 1
    trees[[tree.index]] <- tr
  }
  
  # return the list of m different trees
  trees
}

# --------------------------- Function tree_pred_b ---------------------------
# function name: 
#   tree_pred_b
# 
# input arguments:
#   x: vector/matrix (all the columns of the numeric predictors spanned across one or more than one rows)
#   trs: list of list of vectors (a list of m models of the classification trees)
# 
# returned result:
#   preds: integer of 1 or 0/vector of 1s and 0s (all the predictions)
# 
# function description:
#   predict the class label based on the voting of all the trees returned by the function tree_grow_b
tree_pred_b <- function(x, trs) {
  
  if(is.null(nrow(x))) {
    # if x contains only one row
    pred <- tree_pred_b_by_one_row(x, trs)
    # return the prediction
    pred
  } else {
    # if x contains more than one rows
    # predictions of all the rows from x
    preds <- c()
    
    for(x.row.index in 1:nrow(x)) {
      # pick one row from x
      x.row <- x[x.row.index,]
      
      pred <- tree_pred_b_by_one_row(x.row, trs)
      # add the prediction to the predictions
      preds <- c(preds, pred)
    }
    # return all the predictions
    preds
  }
}

# --------------------------- Function tree_pred_by_one_row ---------------------------
# function name: 
#   tree_pred_by_one_row
# 
# input arguments:
#   x: vector (all the columns of the numeric predictors for only one row)
#   tr: list of vectors (the model of the classification tree)
# 
# returned result:
#   integer 0 or 1 (one prediction)
# 
# function description:
#   predict the class label based on the model of the tree returned by the function tree_grow
tree_pred_by_one_row <- function(x, tr) {
  # the initial node index is 1 = root node
  current.node.index <- 1
  
  while(is.na(tr[[current.node.index]][3])) {
    
    if(x[tr[[current.node.index]][1]] <= tr[[current.node.index]][2]) {
      # traverse to the left subtree
      current.node.index <- tr[[current.node.index]][4]
      
    } else {
      # traverse to the right subtree
      current.node.index <- tr[[current.node.index]][5]
    }
  } 
  
  # return the classified label
  tr[[current.node.index]][3]
}

# --------------------------- Function tree_pred_b_by_one_row ---------------------------
# function name: 
#   tree_pred_b_by_one_row
# 
# input arguments:
#   x: vector (all the columns of the numeric predictors for only one row)
#   trs: list of list of vectors (a list of m models of the classification trees)
# 
# returned result:
#   integer 0 or 1 (one prediction)
# 
# function description:
#   predict the class label based on the voting of all the trees returned by the function tree_grow_b
tree_pred_b_by_one_row <- function(x, trs) {
  # predictions of all the trees
  preds <- c()
  
  # iterate through all the trees to calculate the predictions
  for(tr in trs) {
    pred <- tree_pred(x, tr)
    preds <- c(preds, pred)
  }
  
  # return the majority vote of all the trees
  vote_of_majority(preds)
}

# --------------------------- Function vote_of_majority ---------------------------
# function name: 
#   vote_of_majority
# 
# input arguments:
#   y: vector (one column of the labels)
# 
# returned result:
#   integer 0 or 1 (the majority of y)
# 
# function description:
#   calcuate the classified label by the vote of the majority of predictions
vote_of_majority <- function(y) {
  
  if(length(y[y == 0]) > length(y[y == 1]))
    0
  else
    1
}

# --------------------------- Function quality_measure ---------------------------
# function name: 
#   quality_measure
# 
# input arguments:
#   y.actual: vector (a vector of actual labals)
#   y.predicted: vector (a vector of predicted labels)
# 
# returned result:
#   NULL
# 
# function description:
#   display the quality measure: confusion matrix and precision/recall/accuracy
quality_measure <- function(y.actual, y.predicted) {
  # confusion matrix
  confusion.matrix <- table(y.actual, y.predicted, dnn = c("Actual", "Predicted"))
  
  # true positive = predicted and observed as defects (label = 1) 
  true.positive <- confusion.matrix[2,2]
  # false positive = predicted as defects, but observed as non-defects
  false.positive <- confusion.matrix[1,2]
  # true negative = predicted and observed as non-defects (label = 0) 
  true.negative <- confusion.matrix[1,1]
  # false negative = predicted as non-defects, but observed as defects
  false.negative <- confusion.matrix[2,1]
  
  # precision = true defects / predicted defects
  precision = true.positive / (true.positive + false.positive)
  # recall = true defects / actual defects
  recall = true.positive / (true.positive + false.negative)
  # accuracy = correct classifications / total
  accuracy = (true.positive + true.negative) / (true.positive + true.negative + false.positive + false.negative)
  
  # store precision, recall and accuracy in accuracy.table
  accuracy.table <- tibble(precision = precision, recall = recall, accuracy = accuracy)
  
  # return the result as a list comprised of accuracy.table and confusion.matrix
  list(accuracy.table, confusion.matrix)
}

# --------------------------- Data analysis ---------------------------
# 
# --------------------------- Load the training and test dataset ---------------------------
# read semicolon-separated files by the function read_csv2
# the training set is from "release 2.0"
train.data.frame <- read_csv2("./data/eclipse-metrics-packages-2.0.csv") %>%
  select(starts_with("FOUT", ignore.case = FALSE), starts_with("MLOC", ignore.case = FALSE), starts_with("NBD", ignore.case = FALSE), starts_with("PAR", ignore.case = FALSE), starts_with("VG", ignore.case = FALSE), starts_with("NOF", ignore.case = FALSE), starts_with("NOM", ignore.case = FALSE), starts_with("NSF", ignore.case = FALSE), starts_with("NSM", ignore.case = FALSE), starts_with("ACD", ignore.case = FALSE), starts_with("NOI", ignore.case = FALSE), starts_with("NOT", ignore.case = FALSE), starts_with("TLOC", ignore.case = FALSE), starts_with("NOCU", ignore.case = FALSE), pre, post)
train.data.frame <- data.matrix(train.data.frame)

x.train <- train.data.frame[,1:41]
y.train <- train.data.frame[,42]
y.train[y.train!=0] = 1

# the test set is from "release 3.0"
test.data.frame <- read_csv2("./data/eclipse-metrics-packages-3.0.csv") %>%
  select(starts_with("FOUT", ignore.case = FALSE), starts_with("MLOC", ignore.case = FALSE), starts_with("NBD", ignore.case = FALSE), starts_with("PAR", ignore.case = FALSE), starts_with("VG", ignore.case = FALSE), starts_with("NOF", ignore.case = FALSE), starts_with("NOM", ignore.case = FALSE), starts_with("NSF", ignore.case = FALSE), starts_with("NSM", ignore.case = FALSE), starts_with("ACD", ignore.case = FALSE), starts_with("NOI", ignore.case = FALSE), starts_with("NOT", ignore.case = FALSE), starts_with("TLOC", ignore.case = FALSE), starts_with("NOCU", ignore.case = FALSE), pre, post)
test.data.frame <- data.matrix(test.data.frame)

x.test <- test.data.frame[,1:41]
y.test <- test.data.frame[,42]
y.test[y.test!=0] = 1

# --------------------------- Load the predictions and results by the single classification tree ---------------------------
# To load trees use:
load(file = PATH_OF_SINGLE_TREE)

# predict based on the single classification tree on the test set
tr.single.preds <- tree_pred(x.test, tr.single)

# performance result of confusion matrix, precision, recall and accuracy for the single classification tree
result.single <- quality_measure(y.test, tr.single.preds)

# --------------------------- Load the predictions and results by the classification tree by bagging ---------------------------
# To load trees use:
load(file = PATH_OF_BAGGING)

# predict based on the classification tree by bagging on the test set
tr.bagging.preds <- tree_pred_b(x.test, tr.bagging)

# performance result of confusion matrix, precision, recall and accuracy for the classification tree by bagging
result.bagging <- quality_measure(y.test, tr.bagging.preds)

# --------------------------- Load the predictions and results by the classification tree by random forest ---------------------------
# To load trees use:
load(file = PATH_OF_RANDOM_FOREST)

# predict based on the classification tree by random forest on the test set
tr.random.forest.preds <- tree_pred_b(x.test, tr.random.forest)

# performance result of confusion matrix, precision, recall and accuracy for the classification tree by random forest
result.random.forest <- quality_measure(y.test, tr.random.forest.preds)

#------------------------------- Test significance -------------------------------------------
#
#------------------------------- Pre-process the to-be tested data -------------------------------------------
#
# pre-process the performance result for later data analysis
perf.single <- result.single[[1]] %>% mutate(model = "single tree")

# pre-process the performance result for later data analysis
perf.bagging <- result.bagging[[1]] %>% mutate(model = "bagging")

# pre-process the performance result for later data analysis
perf.random.forest <- result.random.forest[[1]] %>% mutate(model = "random forest")

#------------------------------- ANOVA -------------------------------------------

performance <- bind_rows(perf.single, perf.bagging, perf.random.forest)
aov(accuracy ~ model, data = performance) # not enough data to compute significance: no information about the variance of the individual models

# Instead use information per observation and compare means (mean of "correct" equals the proportion correct)
predictions <- tibble(predictions = c(tr.single.preds, 
                                      tr.bagging.preds, 
                                      tr.random.forest.preds), 
                      models = c(rep("single tree", 
                                     length(tr.single.preds)), 
                                 rep("bagging", 
                                     length(tr.bagging.preds)), 
                                 rep("random forest", 
                                     length(tr.random.forest.preds))), 
                      ground_truth = c(rep(y.test, 3)), 
                      correct = ifelse(ground_truth == predictions, 1, 0)) %>%
  mutate(models = factor(models,levels=c("single tree", "bagging", "random forest")))

summary(aov(correct ~ models, data = predictions))

#------------------------------- Generalized Linear Model -------------------------------------------
# Glm to estimate effect size

summary(glm(correct ~ models, family = "binomial", data = predictions))
## Bagging performas significantly better than the single tree, random forrest does not.

#------------------------------- Chi-Square Test -------------------------------------------
#
# pairwise chi-square test between the single tree and the tree with bagging
accuracy.single.bagging <- c(result.single[[2]][1] + result.single[[2]][4], result.single[[2]][2] + result.single[[2]][3],
                             result.bagging[[2]][1] + result.bagging[[2]][4], result.bagging[[2]][2] + result.bagging[[2]][3])
accuracy.matrix.single.bagging <- matrix(accuracy.single.bagging, nrow = 2, ncol = 2, byrow = TRUE)
dimnames(accuracy.matrix.single.bagging) <- list(c("Single", "Bagging"), c("Correct", "Wrong"))

chisq.test(accuracy.matrix.single.bagging, simulate.p.value = TRUE)

# pairwise chi-square test between the single tree and the random forest
accuracy.single.random.forest <- c(result.single[[2]][1] + result.single[[2]][4], result.single[[2]][2] + result.single[[2]][3],
                                   result.random.forest[[2]][1] + result.random.forest[[2]][4], result.random.forest[[2]][2] + result.random.forest[[2]][3])
accuracy.matrix.single.random.forest <- matrix(accuracy.single.random.forest, nrow = 2, ncol = 2, byrow = TRUE)
dimnames(accuracy.matrix.single.random.forest) <- list(c("Single", "Random forest"), c("Correct", "Wrong"))

chisq.test(accuracy.matrix.single.random.forest, simulate.p.value = TRUE)

# pairwise chi-square test between the tree with bagging and the random forest
accuracy.bagging.random.forest <- c(result.bagging[[2]][1] + result.bagging[[2]][4], result.bagging[[2]][2] + result.bagging[[2]][3],
                                    result.random.forest[[2]][1] + result.random.forest[[2]][4], result.random.forest[[2]][2] + result.random.forest[[2]][3])
accuracy.matrix.bagging.random.forest <- matrix(accuracy.bagging.random.forest, nrow = 2, ncol = 2, byrow = TRUE)
dimnames(accuracy.matrix.bagging.random.forest) <- list(c("Bagging", "Random forest"), c("Correct", "Wrong"))

chisq.test(accuracy.matrix.bagging.random.forest, simulate.p.value = TRUE)

#------------------------------- Power Test -------------------------------------------
#
#------------------------------- Power Test for Chi-Square Test -------------------------------------------
# Agresti(2007) p.39 - Taken from ?chisq.text documentation
#
# power test for chi-square test between the single tree and the tree with bagging
group.sample <- length(y.test)
p0.1 <- (result.single[[2]][1] + result.single[[2]][4] + result.bagging[[2]][1] + result.bagging[[2]][4]) / (group.sample * 2)
p0.2 <- (result.single[[2]][2] + result.single[[2]][3] + result.bagging[[2]][2] + result.bagging[[2]][3]) / (group.sample * 2)
p1.1.1 <- (result.single[[2]][1] + result.single[[2]][4]) / group.sample
p1.1.2 <- (result.bagging[[2]][1] + result.bagging[[2]][4]) / group.sample
p1.2.1 <- (result.single[[2]][2] + result.single[[2]][3]) / group.sample
p1.2.2 <- (result.bagging[[2]][2] + result.bagging[[2]][3]) / group.sample
effect.size <- sqrt((p0.1 - p1.1.1)^2/p0.1 + (p0.1 - p1.1.2)^2/p0.1 +  (p0.2 - p1.2.1)^2/p0.2 + (p0.2 - p1.2.2)^2/p0.2)
total.samples <- group.sample * 2
degree.freedom <- (2-1) * (2-1)
pwr.chisq.test(w = effect.size, df = degree.freedom, N = total.samples, sig.level=0.05)

# power test for chi-square test between the single tree and the random forest
p0.1 <- (result.single[[2]][1] + result.single[[2]][4] + result.random.forest[[2]][1] + result.random.forest[[2]][4]) / (group.sample * 2)
p0.2 <- (result.single[[2]][2] + result.single[[2]][3] + result.random.forest[[2]][2] + result.random.forest[[2]][3]) / (group.sample * 2)
p1.1.1 <- (result.single[[2]][1] + result.single[[2]][4]) / group.sample
p1.1.2 <- (result.random.forest[[2]][1] + result.random.forest[[2]][4]) / group.sample
p1.2.1 <- (result.single[[2]][2] + result.single[[2]][3]) / group.sample
p1.2.2 <- (result.random.forest[[2]][2] + result.random.forest[[2]][3]) / group.sample
effect.size <- sqrt((p0.1 - p1.1.1)^2/p0.1 + (p0.1 - p1.1.2)^2/p0.1 +  (p0.2 - p1.2.1)^2/p0.2 + (p0.2 - p1.2.2)^2/p0.2)
total.samples <- group.sample * 2
degree.freedom <- (2-1) * (2-1)
pwr.chisq.test(w = effect.size, df = degree.freedom, N = total.samples, sig.level=0.05)

# power test for chi-square test between the tree with bagging and the random forest
p0.1 <- (result.bagging[[2]][1] + result.bagging[[2]][4] + result.random.forest[[2]][1] + result.random.forest[[2]][4]) / (group.sample * 2)
p0.2 <- (result.bagging[[2]][2] + result.bagging[[2]][3] + result.random.forest[[2]][2] + result.random.forest[[2]][3]) / (group.sample * 2)
p1.1.1 <- (result.bagging[[2]][1] + result.bagging[[2]][4]) / group.sample
p1.1.2 <- (result.random.forest[[2]][1] + result.random.forest[[2]][4]) / group.sample
p1.2.1 <- (result.bagging[[2]][2] + result.bagging[[2]][3]) / group.sample
p1.2.2 <- (result.random.forest[[2]][2] + result.random.forest[[2]][3]) / group.sample
effect.size <- sqrt((p0.1 - p1.1.1)^2/p0.1 + (p0.1 - p1.1.2)^2/p0.1 +  (p0.2 - p1.2.1)^2/p0.2 + (p0.2 - p1.2.2)^2/p0.2)
total.samples <- group.sample * 2
degree.freedom <- (2-1) * (2-1)
pwr.chisq.test(w = effect.size, df = degree.freedom, N = total.samples, sig.level=0.05)
