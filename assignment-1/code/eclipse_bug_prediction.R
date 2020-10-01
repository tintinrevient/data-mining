library(tidyverse)

# calculate the impurity based on Gini index for binary classifications
# y = one column of the label - multiple rows = vector
impurity_by_gini_index <- function(y) {
  
  zero.percent <- length(y[y == 0]) / length(y)
  one.percent <- 1 - zero.percent
  
  # return the impurity
  zero.percent * one.percent
}

# the best split for a node based on one numeric attribute x and the label y
# x = one column of a numeric predictor - multiple rows = vector
# y = one column of the label - multiple rows = vector
bestsplit <- function(x, y) {
  
  x.sorted <- sort(unique(x))
  x.sorted.length <- length(x.sorted)
  
  # if there is only one unique value in x
  # the maximum impurity reduction is 0
  # the best split point is NA
  if(x.sorted.length < 2) {
    return(c(0, NA))
  }
  
  # e.g. x = c(1, 2, 3, 4, 5): x has 5 values, and thus x has (5 - 1) = 4 split points.
  # x.splitpoints = (c(1, 2, 3, 4) + c(2, 3, 4, 5)) / 2
  x.splitpoints <- (x.sorted[1:x.sorted.length-1] + x.sorted[2:x.sorted.length]) / 2
  
  impurity.parent <- impurity_by_gini_index(y)
  # initialize the best split point and its reduction of impurity.
  # the reduction of impurity is supposed to be the maximum reduction of impurity.
  impurity.reduction.max <- 0
  x.splitpoints.best <- NULL
  
  for(x.splitpoint in x.splitpoints) {
    # the left children of a parent: x is less than and equal to the value of the split point.
    proportion.left.children <- length(y[x <= x.splitpoint]) / length(y)
    impurity.left.children <- impurity_by_gini_index(y[x <= x.splitpoint])
    
    # the right children of a parent: x is greater than the value of the split point.
    proportion.right.children <- length(y[x > x.splitpoint]) / length(y)
    impurity.right.children <- impurity_by_gini_index(y[x > x.splitpoint])
    
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

# calcuate the classified label by the vote of the majority of predictions
# y = one column of the label - multiple rows = vector
vote_of_majority <- function(y) {
  
  if(length(y[y == 0]) > length(y[y == 1]))
    0
  else
    1
}

# grow the classification tree
# x = all the columns of the numeric predictors - multiple rows = matrix
# y = one column of the label - multiple rows = vector
# nmin = the minimum number of observations that a node must contain
# minleaf = the minimum number of observations required for a leaf node
# nfeat = the set of predictors, e.g. nfeat = c(1, 2, 3, 4, 5) or nfeat = c(1, 2, 4)
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
    if(impurity_by_gini_index(current.node.y) > 0 && length(current.node.y) >= nmin) {
      
      # Find one best feature to split the data
      # Feature is represented by its index in the table
      feat.best <- NA
      feat.splitpoints.best <- NA
      feat.impurity.reduction.max <- 0
      
      # iterate through the set of candidate splits in current node.
      # find the best split point of the best split feature with the maximum impurity reduction.
      # S <- set of candidate splits in current node
      # s* <- argmax {impurity reduction(s, current node)}, where s is in S
      for(feat in nfeat) {
        result <- bestsplit(current.node.x[,feat], current.node.y)
        
        # if there does not exist the best split for the current feature
        # continue to the next feature
        if(result[1] == 0 && is.na(result[2])) {
          next
        }
        
        if(result[1] > feat.impurity.reduction.max) {
          feat.impurity.reduction.max <- result[1]
          feat.splitpoints.best <- result[2]
          feat.best <- feat
        }
      }
      
      # if there does not exist the best feature to split the node
      # stop splitting, and append the current node as the terminal node to the tree model. 
      if(is.na(feat.best) || is.na(feat.splitpoints.best)) {
        tree.node.index <- tree.node.index + 1
        tree[[tree.node.index]] <- c(NA, NA, vote_of_majority(current.node.y), NA, NA) 
        next
      }
      
      # child nodes <- apply(s*, current node)
      # children.left are the rows in the table, whose value of the best-split feature is less than and equal to the best-split point.
      children.left <- list(current.node.x[current.node.x[,feat.best] <= feat.splitpoints.best,], current.node.y[current.node.x[,feat.best] <= feat.splitpoints.best])
      # children.right are the rows in the table, whose value of the best-split feature is greater than the best-split point.
      children.right <- list(current.node.x[current.node.x[,feat.best] > feat.splitpoints.best,], current.node.y[current.node.x[,feat.best] > feat.splitpoints.best])
      
      # check minleaf constraint
      if(length(children.left[[2]]) < minleaf || length(children.right[[2]]) < minleaf) {
        # if observations(child nodes) < minleaf
        # stop splitting, and append the current node as the terminal node to the tree model.
        tree.node.index <- tree.node.index + 1
        tree[[tree.node.index]] <- c(NA, NA, vote_of_majority(current.node.y), NA, NA) 
        next
      } else {
        # if observations(child nodes) >= minleaf
        # split the current node into child nodes, and append the current node as the non-terminal node to the tree model.
        tree.node.index <- tree.node.index + 1
        tree[[tree.node.index]] <- c(feat.best, feat.splitpoints.best, NA, current.node.index + length(nodelist)/2 + 1, current.node.index + length(nodelist)/2 + 2)
        
        # nodelist <- nodelist + child nodes
        nodelist <- c(nodelist, children.left)
        nodelist <- c(nodelist, children.right)
      }
      
    } else {
      
      if (impurity_by_gini_index(current.node.y) == 0) {
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

# predict the class label based on the model of the tree returned by tree_grow()
# x = all the columns of the numeric predictors - one row = vector
# tr = the model of the tree
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

# predict the class label based on the model of the tree returned by tree_grow()
# x = all the columns of the numeric predictors - multiple rows = matrix or vector
# tr = the model of the tree
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

# grow the classification tree by the bagging with random forest
# m = the number of bootstrap samples to be drawn
# sample_size = the size of one bootstrap sample (training set) < the size of table
# nfeat_subset_size = the size of the subset of nfeat < the total number of predictors
tree_grow_b <- function(x, y, nmin, minleaf, nfeat, m, sample.size, nfeat.subset.size) {
  # a list of m different trees
  trees <- list()
  tree.index <- 0
  
  # draw m samples with replacement from "table"
  for(i in 1:m) {
    # draw a sample (training set) with replacement from "table"
    indices <- sample(length(y), sample.size, replace = TRUE)
    
    # random forest: randomly select a subset of nfeat
    # 1 <= nfeat_subset_size < nfeat
    nfeat.subset <- sample(nfeat, nfeat.subset.size)
    
    # grow a tree on this sample
    tr <- tree_grow(x, y, nmin, minleaf, nfeat.subset)
    
    # append this tree to the list of trees
    tree.index <- tree.index + 1
    trees[[tree.index]] <- tr
  }
  
  # return the list of m different trees
  trees
}

# predict the class label based on the voting of all the trees returned by tree_grow_b()
# x = all the columns of the numeric predictors - one row = vector
# trees = the list of the models of the trees
tree_pred_b_by_one_row <- function(x, trees) {
  # predictions of all the trees
  preds.trees <- c()
  
  # iterate through all the trees to calculate the predictions
  for(tr in trees) {
    pred <- tree_pred(x, tr)
    preds.trees <- c(preds.trees, pred)
  }
  
  # return the majority vote of all the trees
  vote_of_majority(preds.trees)
}

# predict the class label based on the voting of all the trees returned by tree_grow_b()
# x = all the columns of the numeric predictors - multiple rows = matrix or vector
# trees = the list of the models of the trees
tree_pred_b <- function(x, trees) {
  
  if(is.null(nrow(x))) {
    # if x contains only one row
    label <- tree_pred_b_by_one_row(x, trees)
    # return the prediction
    label
  } else {
    # if x contains more than one rows
    # predictions of all the rows from x
    preds <- c()
    
    for(x.row.index in 1:nrow(x)) {
      # pick one row from x
      x.row <- x[x.row.index,]
      
      label <- tree_pred_b_by_one_row(x.row, trees)
      # add the prediction to the predictions
      preds <- c(preds, label)
    }
    # return all the predictions
    preds
  }
}

# display the statistics
# y.actual = vector
# y.predicted = vector
statistics <- function(y.actual, y.predicted) {
  # confusion matrix
  confusion.matrix <- table(y.actual, y.predicted, dnn = c("Actual", "Predicted"))
  print(confusion.matrix)
  
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
  
  # return the result
  result <- tibble(precision = precision, recall = recall, accuracy = accuracy)
  result
}

# read semicolon-separated files by the read_csv2() function
# the training set "release 2.0"
train.data.frame <- read_csv2("./data/eclipse-metrics-packages-2.0.csv") %>%
  select(starts_with("FOUT", ignore.case = FALSE), starts_with("MLOC", ignore.case = FALSE), starts_with("NBD", ignore.case = FALSE), starts_with("PAR", ignore.case = FALSE), starts_with("VG", ignore.case = FALSE), starts_with("NOF", ignore.case = FALSE), starts_with("NOM", ignore.case = FALSE), starts_with("NSF", ignore.case = FALSE), starts_with("NSM", ignore.case = FALSE), starts_with("ACD", ignore.case = FALSE), starts_with("NOI", ignore.case = FALSE), starts_with("NOT", ignore.case = FALSE), starts_with("TLOC", ignore.case = FALSE), starts_with("NOCU", ignore.case = FALSE), pre, post)
train.data.frame <- data.matrix(train.data.frame)

x.train <- train.data.frame[,1:41]
y.train <- train.data.frame[,42]
y.train[y.train!=0] = 1

# the test set "release 3.0"
test.data.frame <- read_csv2("./data/eclipse-metrics-packages-3.0.csv") %>%
  select(starts_with("FOUT", ignore.case = FALSE), starts_with("MLOC", ignore.case = FALSE), starts_with("NBD", ignore.case = FALSE), starts_with("PAR", ignore.case = FALSE), starts_with("VG", ignore.case = FALSE), starts_with("NOF", ignore.case = FALSE), starts_with("NOM", ignore.case = FALSE), starts_with("NSF", ignore.case = FALSE), starts_with("NSM", ignore.case = FALSE), starts_with("ACD", ignore.case = FALSE), starts_with("NOI", ignore.case = FALSE), starts_with("NOT", ignore.case = FALSE), starts_with("TLOC", ignore.case = FALSE), starts_with("NOCU", ignore.case = FALSE), pre, post)
test.data.frame <- data.matrix(test.data.frame)

x.test <- test.data.frame[,1:41]
y.test <- test.data.frame[,42]
y.test[y.test!=0] = 1

# train the single classification tree on the training set
nmin <- 15
minleaf <- 5
nfeat <- c(1:41)
tr <- tree_grow(x.train, y.train, nmin, minleaf, nfeat)

# predict based on the single classification tree on the test set
tr.preds <- tree_pred(x.test, tr)

# precision, recall and accuracy of the single classification tree
result <- statistics(y.test, tr.preds)
result

# train the classification tree by bagging on the training set
m <- 100
sample.size <- 100
nfeat.subset.size <- 41
tr.bagging <- tree_grow_b(x.train, y.train, nmin, minleaf, nfeat, m, sample.size, nfeat.subset.size)

# predict based on the classification tree by bagging on the test set
tr.bagging.preds <- tree_pred_b(x.test, tr.bagging)

# precision, recall and accuracy of the classification tree by bagging
result <- statistics(y.test, tr.bagging.preds)
result

# train the classification tree by random forest on the training set
nfeat.subset.size <- 6
tr.random.forest <- tree_grow_b(x.train, y.train, nmin, minleaf, nfeat, m, sample.size, nfeat.subset.size)

# predict based on the classification tree by random forest on the test set
tr.random.forest.preds <- tree_pred_b(x.test, tr.random.forest)

# precision, recall and accuracy of the classification tree by random forest
result <- statistics(y.test, tr.random.forest.preds)
result
