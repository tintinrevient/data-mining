# --------------------------- Student names ---------------------------
# Code by:
#   Fleur (5583837)
#   Anouk (6620590)
#   Shu Zhao (6833519)
# 
# --------------------------- Dependent library ---------------------------
library(tidyverse)

# --------------------------- Basic functions ---------------------------
# 
# --------------------------- Function tree_grow ---------------------------
# function name: 
#   tree_grow
#
# input arguments:
#   x: matrix (all the columns of the numeric predictors)
#   y: vector (one column of the actual labels)
#   nmin: integer (the minimum number of observations that a node must contain)
#   minleaf: integer (the minimum number of observations required for a leaf node)
#   nfeat: integer (the number of predictors used to grow the tree)
#
# returned result:
#   tree: list of vectors (the model of the classification tree)
# 
# function description:
#   grow the classification tree
tree_grow <- function(x, y, nmin, minleaf, nfeat) {
  
  # nodelist <- {{training data}}
  nodelist <- list(x, y)
  current.node.index <- 0
  
  # the model of the tree = a list of nodes
  # each node is a vector = c(one best feature to split the node, the best split point of that best feature, the voted classified label, left children node index, right children node index)
  # for the non-terminal nodes, the vector is like c(the best feature, the best split point, NA, left children, right children)
  # for the terminal nodes, the vector is like c(NA, NA, the voted classified label, NA, NA)
  
  # initialize the model of the trained tree
  tree <- list()
  tree.node.index <- 0
  
  # nfeat.set is sampled out of all the predictors from [1, ncol(x)] to choose the number of nfeat predictors
  nfeat.set <- sort(sample(ncol(x), nfeat))
  
  repeat{
    
    # until nodelist = ∅
    if(length(nodelist) == 0)
      break
    
    # current_node <- select node from nodelist
    current.node.x <- nodelist[[1]]
    current.node.y <- nodelist[[2]]
    current.node.index <- current.node.index + 1
    # nodelist <- nodelist - current node
    nodelist[[2]] <- NULL
    nodelist[[1]] <- NULL
    
    # if impurity(current node) > 0 AND observations(current node) >= nmin, the current node is allowed to be split.
    if(impurity(current.node.y) > 0 && length(current.node.y) >= nmin) {
      
      # find one best feature to split the data
      # feature is represented by its index in the table
      feat.best <- NA
      feat.splitpoints.best <- NA
      feat.impurity.reduction.max <- 0
      
      # iterate through the set of candidate splits in current node.
      # find the best split point of the best split feature with the maximum impurity reduction.
      # S <- set of candidate splits in current node
      # s* <- argmax {impurity reduction(s, current node)}, where s is in S
      for(feat in nfeat.set) {
        # result is a vector
        # result[1] = the maximum impurity reduction
        # result[2] = the best split point
        result <- bestsplit(current.node.x[,feat], current.node.y)
        
        # if there does not exist the best split for the current feature
        # skip this feature, and continue to the next feature
        if(result[1] == 0 && is.na(result[2])) {
          next
        }
        
        # check minleaf constraint
        # if either observations(left children) < minleaf or observations(right children) < minleaf
        # skip this feature, and continue to the next feature
        children.left.size <- length(current.node.y[current.node.x[,feat] <= result[2]])
        children.right.size <- length(current.node.y[current.node.x[,feat] > result[2]])
        if(children.left.size < minleaf || children.right.size < minleaf) {
          next
        }
        
        # update the best feature with its best split point
        # out of all the possible splits that meet the minleaf constraint, pick the best one.
        if(result[1] > feat.impurity.reduction.max) {
          feat.impurity.reduction.max <- result[1]
          feat.splitpoints.best <- result[2]
          feat.best <- feat
        }
      }
      
      if(is.na(feat.best) || is.na(feat.splitpoints.best)) {
        # if there does not exist the best feature to split the node
        # stop splitting, and append the current node as the terminal node to the tree model. 
        tree.node.index <- tree.node.index + 1
        tree[[tree.node.index]] <- c(NA, NA, vote_of_majority(current.node.y), NA, NA) 
        next
        
      } else {
        # split the current node into child nodes
        # child nodes <- apply(s*, current node)
        # children.left are the rows in the table, whose value of the best-split feature is less than and equal to the best-split point.
        children.left <- list(current.node.x[current.node.x[,feat.best] <= feat.splitpoints.best,], current.node.y[current.node.x[,feat.best] <= feat.splitpoints.best])
        # children.right are the rows in the table, whose value of the best-split feature is greater than the best-split point.
        children.right <- list(current.node.x[current.node.x[,feat.best] > feat.splitpoints.best,], current.node.y[current.node.x[,feat.best] > feat.splitpoints.best])
        
        # append the current node as the non-terminal node to the tree model.
        tree.node.index <- tree.node.index + 1
        tree[[tree.node.index]] <- c(feat.best, feat.splitpoints.best, NA, current.node.index + length(nodelist)/2 + 1, current.node.index + length(nodelist)/2 + 2)
        
        # nodelist <- nodelist + child nodes
        nodelist <- c(nodelist, children.left)
        nodelist <- c(nodelist, children.right)
      }
      
    } else {
      
      if (impurity(current.node.y) == 0) {
        # if the current node is pure:
        # append the current node as the terminal node to the tree model by its pure label.
        tree.node.index <- tree.node.index + 1
        tree[[tree.node.index]] <- c(NA, NA, vote_of_majority(current.node.y), NA, NA)
        
      } else {
        # if the current node is not pure, but observations(current node) < nmin
        # append the current node as the terminal node to the tree model by its majority of labels.
        tree.node.index <- tree.node.index + 1
        tree[[tree.node.index]] <- c(NA, NA, vote_of_majority(current.node.y), NA, NA)
      }
    }
  }
  
  # return the model of the trained tree
  tree
}

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

# --------------------------- Function impurity ---------------------------
# function name: 
#   impurity
# 
# input arguments:
#   y: vector (one column of the actual labels)
# 
# returned result:
#   zero.percent * one.percent: double (value of the impurity calculated based on Gini index)
# 
# function description:
#   calculate the impurity based on Gini index for binary classifications
impurity <- function(y) {
  
  zero.percent <- length(y[y == 0]) / length(y)
  one.percent <- 1 - zero.percent
  
  # return the impurity
  zero.percent * one.percent
}

# --------------------------- Function bestsplit ---------------------------
# function name: 
#   bestsplit
# 
# input arguments:
#   x: vector (one column of a numeric predictor)
#   y: vector ( one column of the actual labels)
# 
# returned result:
#   vector comprised of the best split point of a numeric predictor and its maximum impurity reduction
# 
# function description:
#   the best split for a node based on one numeric attribute x and the label y
bestsplit <- function(x, y) {
  
  x.sorted <- sort(unique(x))
  x.sorted.length <- length(x.sorted)
  
  impurity.parent <- impurity(y)
  # initialize the best split point and its reduction of impurity.
  # the reduction of impurity is supposed to be the maximum reduction of impurity.
  impurity.reduction.max <- 0
  x.splitpoints.best <- NA
  
  # if there is only one unique value in x
  # the maximum impurity reduction is 0
  # the best split point is NA
  if(x.sorted.length < 2) {
    return(c(impurity.reduction.max, x.splitpoints.best))
  }
  
  # e.g. x = c(1, 2, 3, 4, 5): x has 5 values, and thus x has (5 - 1) = 4 split points.
  # x.splitpoints = (c(1, 2, 3, 4) + c(2, 3, 4, 5)) / 2
  x.splitpoints <- (x.sorted[1:x.sorted.length-1] + x.sorted[2:x.sorted.length]) / 2
  
  for(x.splitpoint in x.splitpoints) {
    # the left children of a parent: x is less than and equal to the value of the split point.
    proportion.left.children <- length(y[x <= x.splitpoint]) / length(y)
    impurity.left.children <- impurity(y[x <= x.splitpoint])
    
    # the right children of a parent: x is greater than the value of the split point.
    proportion.right.children <- length(y[x > x.splitpoint]) / length(y)
    impurity.right.children <- impurity(y[x > x.splitpoint])
    
    # calculate the reduction of impurity
    impurity.reduction <- impurity.parent - (proportion.left.children*impurity.left.children + proportion.right.children*impurity.right.children)
    
    # update the best split and its reduction of impurity, which is supposed to be maximum.
    if(impurity.reduction > impurity.reduction.max) {
      impurity.reduction.max <- impurity.reduction
      x.splitpoints.best <- x.splitpoint
    }
  }
  
  # return the vector: c(the maximum impurity reduction, the best split point)
  c(impurity.reduction.max, x.splitpoints.best)
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

# --------------------------- The single classification tree ---------------------------
# train the single classification tree on the training set
nmin <- 15
minleaf <- 5
nfeat <- 41
#tr.simple <- tree_grow(x.train, y.train, nmin, minleaf, nfeat)

# --------------BEFORE RE-DOING ANALYSIS, LOAD SAVED TREES--------------

# To load trees use:
load(file = "tree_simple.RData")

# predict based on the single classification tree on the test set
tr.simple.preds <- tree_pred(x.test, tr.simple)

# performance result of confusion matrix, precision, recall and accuracy for the single classification tree
result.simple <- quality_measure(y.test, tr.simple.preds)

# pre-process the performance result for later data analysis
perf.simple <- result.simple[[1]] %>% mutate(model = "single tree")

# --------------------------- The classification tree by bagging ---------------------------
# train the classification tree by bagging on the training set
m <- 100
#tr.bagging <- tree_grow_b(x.train, y.train, nmin, minleaf, nfeat, m)

# --------------BEFORE RE-DOING ANALYSIS, LOAD SAVED TREES--------------

# To load trees use:
load(file = "tree_bagging.RData")


# predict based on the classification tree by bagging on the test set
tr.bagging.preds <- tree_pred_b(x.test, tr.bagging)

# performance result of confusion matrix, precision, recall and accuracy for the classification tree by bagging
result.bagging <- quality_measure(y.test, tr.bagging.preds)

# pre-process the performance result for later data analysis
perf.bagging <- result.bagging[[1]] %>% mutate(model = "bagging")

# --------------------------- The classification tree by random forest ---------------------------
# train the classification tree by random forest on the training set
nfeat <- 6
#tr.random.forest <- tree_grow_b(x.train, y.train, nmin, minleaf, nfeat, m)

# --------------BEFORE RE-DOING ANALYSIS, LOAD SAVED TREES--------------

# To load trees use:
load(file = "tree_random.RData")

# predict based on the classification tree by random forest on the test set
tr.random.forest.preds <- tree_pred_b(x.test, tr.random.forest)

# performance result of confusion matrix, precision, recall and accuracy for the classification tree by random forest
result.random.forest <- quality_measure(y.test, tr.random.forest.preds)

# pre-process the performance result for later data analysis
perf.random.forest <- result.random.forest[[1]] %>% mutate(model = "random forest")

# --------------------------- Print the performance results of all three tree models ---------------------------

print("quality measure of the single classification tree")
print(result.simple)

print("quality measure of the classification tree by bagging")
print(result.bagging)

print("quality measure of the classification tree by random forest")
print(result.random.forest)

#------------------------------- Test significance -------------------------------------------
#
#------------------------------- ANOVA -------------------------------------------

performance <- bind_rows(perf.simple, perf.bagging, perf.random.forest)
aov(accuracy ~ model, data = performance) # not enough data to compute significance: no information about the variance of the individual models

# Instead use information per observation and compare means (mean of "correct" equals the proportion correct)
predictions <- tibble(predictions = c(tr.simple.preds, 
                                      tr.bagging.preds, 
                                      tr.random.forest.preds), 
                      models = c(rep("single tree", 
                                     length(tr.simple.preds)), 
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
# pairwise chi-square test between the simple tree and the tree with bagging
accuracy.simple.bagging <- c(result.simple[[2]][1] + result.simple[[2]][4], result.simple[[2]][2] + result.simple[[2]][3],
                          result.bagging[[2]][1] + result.bagging[[2]][4], result.bagging[[2]][2] + result.bagging[[2]][3])
accuracy.matrix.simple.bagging <- matrix(accuracy.simple.bagging, nrow = 2, ncol = 2, byrow = TRUE)
dimnames(accuracy.matrix.simple.bagging) <- list(c("Simple", "Bagging"), c("Correct", "Wrong"))

chisq.test(accuracy.matrix.simple.bagging, simulate.p.value = TRUE)

# pairwise chi-square test between the simple tree and the random forest
accuracy.simple.random.forest <- c(result.simple[[2]][1] + result.simple[[2]][4], result.simple[[2]][2] + result.simple[[2]][3],
                          result.random.forest[[2]][1] + result.random.forest[[2]][4], result.random.forest[[2]][2] + result.random.forest[[2]][3])
accuracy.matrix.simple.random.forest <- matrix(accuracy.simple.random.forest, nrow = 2, ncol = 2, byrow = TRUE)
dimnames(accuracy.matrix.simple.random.forest) <- list(c("Simple", "Random forest"), c("Correct", "Wrong"))

chisq.test(accuracy.matrix.simple.random.forest, simulate.p.value = TRUE)

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

# power test for chi-square test between the simple tree and the tree with bagging
group.sample <- length(y.test)
p0.1 <- (result.simple[[2]][1] + result.simple[[2]][4] + result.bagging[[2]][1] + result.bagging[[2]][4]) / (group.sample * 2)
p0.2 <- (result.simple[[2]][2] + result.simple[[2]][3] + result.bagging[[2]][2] + result.bagging[[2]][3]) / (group.sample * 2)
p1.1.1 <- (result.simple[[2]][1] + result.simple[[2]][4]) / group.sample
p1.1.2 <- (result.bagging[[2]][1] + result.bagging[[2]][4]) / group.sample
p1.2.1 <- (result.simple[[2]][2] + result.simple[[2]][3]) / group.sample
p1.2.2 <- (result.bagging[[2]][2] + result.bagging[[2]][3]) / group.sample
effect.size <- sqrt((p0.1 - p1.1.1)^2/p0.1 + (p0.1 - p1.1.2)^2/p0.1 +  (p0.2 - p1.2.1)^2/p0.2 + (p0.2 - p1.2.2)^2/p0.2)
total.samples <- group.sample * 2
degree.freedom <- (2-1) * (2-1)
pwr.chisq.test(w = effect.size, df = degree.freedom, N = total.samples, sig.level=0.05)

# power test for chi-square test between the simple tree and the random forest
p0.1 <- (result.simple[[2]][1] + result.simple[[2]][4] + result.random.forest[[2]][1] + result.random.forest[[2]][4]) / (group.sample * 2)
p0.2 <- (result.simple[[2]][2] + result.simple[[2]][3] + result.random.forest[[2]][2] + result.random.forest[[2]][3]) / (group.sample * 2)
p1.1.1 <- (result.simple[[2]][1] + result.simple[[2]][4]) / group.sample
p1.1.2 <- (result.random.forest[[2]][1] + result.random.forest[[2]][4]) / group.sample
p1.2.1 <- (result.simple[[2]][2] + result.simple[[2]][3]) / group.sample
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


#--------------------------Saved all trees so that we always use the same trees if we want to run tests again ----------------------

#save(tr.simple, file="tree_simple.RData")
#save(tr.bagging, file="tree_bagging.RData")
#save(tr.random.forest, file="tree_random.RData")


