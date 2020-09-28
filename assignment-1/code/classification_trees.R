# calculate the impurity based on Gini index for binary classifications
impurity_by_gini_index <- function(y) {
  
  percent_of_zero <- length(y[y == 0]) / length(y)
  percent_of_one <- 1 - percent_of_zero
  
  # return the impurity
  percent_of_zero * percent_of_one
}

# test for the impurity function based on Gini index
# the right answer is: impurity = 0.2314050
# y <- c(1,0,1,1,1,0,0,1,1,0,1)
# impurity <- impurity_by_gini_index(y)
# impurity

# the best split for a node based on one numeric attribute x and the label y
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

# test for the best split based on the data of credit.txt
# the right answer is: bestsplit = 36
# credit.dat <- read.csv("./data/credit.txt")
# result <- bestsplit(credit.dat[,4], credit.dat[,6])
# result[1]
# result[2]

# calcuate the classified label by the vote of the majority of nodes
# table = the dataset containing the predictors and the label = 2D matrix
# index_of_y = the index of the label in the table
# e.g. table = credit.dat = c(age, married, house, income, gender, class)
# age, married, house, income, gender -> predictors
# class -> label
# index_of_y is 6, and the starting index is 1 instead of 0.
vote_of_majority_matrix <- function(table, index_of_y) {
  
  vote_of_zero <- table[table[,index_of_y] == 0, ]
  vote_of_one <- table[table[,index_of_y] == 1, ]
  
  if(length(vote_of_zero[,1]) > length(vote_of_one[,1]))
    0
  else
    1
}

# test for the vote of majority
# the right answer is 1
# vote_of_majority_matrix(credit.dat, 6)

# calcuate the classified label by the vote of the majority of predictions
# vector = 1D vector of all the predictions
vote_of_majority_vector <- function(vector) {
  
  vote_of_zero <- vector[vector == 0]
  vote_of_one <- vector[vector == 1]
  
  if(length(vote_of_zero) > length(vote_of_one))
    0
  else
    1
}

# test for the vote of majority
# the right answer is 1
# preds <- c(1, 1, 1, 0, 1, 0)
# vote_of_majority_vector(preds)

# grow the classification tree

# table = the dataset containing the predictors and the label
# index_of_y = the index of the label in the table
# e.g. table = credit.dat = c(age, married, house, income, gender, class)
# age, married, house, income, gender -> predictors
# class -> label
# index_of_y is 6, and the starting index is 1 instead of 0.

# nmin = the minimum number of observations that a node must contain
# minleaf = the minimum number of observations required for a leaf node
# nfeat = the range of predictors, e.g. nfeat = c(1, 2, 3, 4, 5) or nfeat = c(1, 2, 4)
tree_grow <- function(table, index_of_y, nmin, minleaf, nfeat) {
  
  # nodelist <- {{training data}}
  nodelist <- list(table)
  current_node_index <- 0
  
  # the model of the tree = a list of nodes
  # each node is a vector = c(one best feature to split the node, the best split point of that best feature, the voted classified label, left children node index, right children node index)
  # for the non-terminal nodes, the vector is like c(the best feature, the best split point, NA, left children, right children)
  # for the terminal nodes, the vector is like c(NA, NA, the voted classified label, NA, NA)
  
  # initialize the model of the trained tree
  tree <- list()
  tree_node_index <- 0
  
  repeat{
    
    # until nodelist = ∅
    if(length(nodelist) == 0)
      break
    
    # current_node <- select node from nodelist
    current_node <- nodelist[[1]]
    current_node_index <- current_node_index + 1
    # nodelist <- nodelist - current node
    nodelist[[1]] <- NULL
    
    # if impurity(current node) > 0 AND observations(current node) >= nmin, the current node is allowed to be split.
    if(impurity_by_gini_index(current_node[,index_of_y]) > 0 && length(current_node[,1]) >= nmin) {
      
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
        result <- bestsplit(current_node[,feat], current_node[,index_of_y])
        
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
        tree_node_index <- tree_node_index + 1
        tree[[tree_node_index]] <- c(NA, NA, vote_of_majority_matrix(current_node, index_of_y), NA, NA) 
        next
      }

      # child nodes <- apply(s*, current node)
      # left_children are the rows in the table, whose value of the best-split feature is less than and equal to the best-split point.
      left_children <- current_node[current_node[,feat.best] <= feat.splitpoints.best,]
      # right_children are the rows in the table, whose value of the best-split feature is greater than the best-split point.
      right_children <- current_node[current_node[,feat.best] > feat.splitpoints.best,]
      
      # check minleaf constraint
      if(length(left_children[,1]) < minleaf || length(right_children[,1]) < minleaf) {
        # if observations(child nodes) < minleaf
        # stop splitting, and append the current node as the terminal node to the tree model.
        tree_node_index <- tree_node_index + 1
        tree[[tree_node_index]] <- c(NA, NA, vote_of_majority_matrix(current_node, index_of_y), NA, NA) 
        next
      } else {
        # if observations(child nodes) >= minleaf
        # split the current node into child nodes, and append the current node as the non-terminal node to the tree model.
        tree_node_index <- tree_node_index + 1
        tree[[tree_node_index]] <- c(feat.best, feat.splitpoints.best, NA, current_node_index + length(nodelist) + 1, current_node_index + length(nodelist) + 2)
        
        # nodelist <- nodelist + child nodes
        nodelist[[length(nodelist) + 1]] <- left_children
        nodelist[[length(nodelist) + 1]] <- right_children
      }
      
    } else {
      
      if (impurity_by_gini_index(current_node[,index_of_y]) == 0) {
        # if the current node is pure:
        # append the current node as the terminal node to the tree model by its pure label.
        tree_node_index <- tree_node_index + 1
        tree[[tree_node_index]] <- c(NA, NA, current_node[1,index_of_y], NA, NA)
        
      } else {
        # if the current node is not pure:
        # append the current node as the terminal node to the tree model by its majority of labels.
        tree_node_index <- tree_node_index + 1
        tree[[tree_node_index]] <- c(NA, NA, vote_of_majority_matrix(current_node, index_of_y), NA, NA)
      }
    }
  }
  
  # return the model of the trained tree
  tree
}

# test for the tree_grow function based on "credit.txt"
# credit.dat <- read.csv("./data/credit.txt")
# index_of_y <- 6
# nmin <- 2
# minleaf <- 1
# nfeat <- c(1:5)
# credit.tr <- tree_grow(credit.dat, index_of_y, nmin, minleaf, nfeat)
# credit.tr

# test for the tree_grow function based on "pima.txt"
# pima.dat <- read.csv("./data/pima.txt")
# index_of_y <- 9
# nmin <- 20
# minleaf <- 5
# nfeat <- c(1:8)
# pima.tr <- tree_grow(pima.dat, index_of_y, nmin, minleaf, nfeat)
# pima.tr

# get all the descendants of a tree from a specific node index, including itself.
get_all_descendants <- function(node_index, tr) {
  # include itself first
  descendants <- c(node_index)
  
  # add node to nodelist if it is a non-terminal node
  nodelist <- list()
  if(is.na(tr[[node_index]][3])) {
    nodelist[[1]] <- node_index
  }
  
  # traverse the tree
  while(length(nodelist) > 0) {
    current_node_index <- nodelist[[1]]
    nodelist[[1]] <- NULL
    
    # add the left children and right children to descendants
    descendants <- c(descendants, tr[[current_node_index]][4])
    descendants <- c(descendants, tr[[current_node_index]][5])
    
    # add left children to nodelist if it is a non-terminal node
    if(is.na(tr[[tr[[current_node_index]][4]]][3])) {
      nodelist[[length(nodelist) + 1]] <- tr[[current_node_index]][4]
    }
    
    # add right children to nodelist if it is a non-terminal node
    if(is.na(tr[[tr[[current_node_index]][5]]][3])) {
      nodelist[[length(nodelist) + 1]] <- tr[[current_node_index]][5]
    }
  }
  # return the descendants in descending order of node index
  sort(descendants, decreasing = TRUE)
}

# test the descendants based on "credit.txt"
# the right answer = [7, 6, 5, 4, 2]
# descendants <- get_all_descendants(2, credit.tr)
# descendants

# predict the class label based on the model of the tree returned by tree_grow()
# x: predictors
# tr_model: the model of the tree
# the model of the tree is a traversal of the tree in a breadth-first manner.
tree_pred <- function(x, tr) {
  
  # the initial node index is 1 = root node
  current_node_index <- 1
  
  while(is.na(tr[[current_node_index]][3])) {
    
    if(x[tr[[current_node_index]][1]] <= tr[[current_node_index]][2]) {
      # traverse to the left subtree
      current_node_index <- tr[[current_node_index]][4]
      
    } else {
      # traverse to the right subtree
      current_node_index <- tr[[current_node_index]][5]
    }
  } 
  
  # return the classified label
  tr[[current_node_index]][3]
}

# test for the tree_pred function based on "credit.txt"
# the right answer is: label = 0
# x <- c(46, 0, 1, 32, 0)
# label <- tree_pred(x, credit.tr)
# label

# the right answer is: label = 1
# x <- c(50, 1, 1, 28, 0)
# label <- tree_pred(x, credit.tr)
# label

# test for the tree_pred function based on "pima.txt"
# the right answer is: label = 1
# x <- c(6,148,72,35,0,33.6,0.627,50)
# label <- tree_pred(x, pima.tr)
# label

# the right answer is: label = 0
# x <- c(1,100,66,29,196,32.0,0.444,42)
# label <- tree_pred(x, pima.tr)
# label

# comparison with the decision tree generated from rpart
# the data is based on "pima.txt"
# tree_grow
# library(rpart)
# pima.dtree <- rpart(X1 ~ ., data = pima.dat, method = "class", parms = list(split = "information"))
# summary(pima.dtree)

# plot the model of the decision tree
# library(rpart.plot)
# prp(pima.dtree, type = 2, extra = 104, fallen.leaves = TRUE, main = "Decision Tree")

# tree_pred
# pima.dtree.pred <- predict(pima.dtree, pima.dat, type = "class")
# pima.dtree.perf <- table(pima.dat$X1, pima.dtree.pred, dnn = c("Actual", "Predicted"))
# pima.dtree.perf

# grow the classification tree by the bagging with random forest
# length(nfeat) < the total number of predictors: e.g. nfeat = c(1, 2, 4) out of c(1, 2, 3, 4, 5)
# m = the number of bootstrap samples to be drawn
# sample_size = the size of one bootstrap sample (training set) < the size of table
tree_grow_b <- function(table, index_of_y, nmin, minleaf, nfeat, m, sample_size) {
  # a list of m different trees
  trees <- list()
  tree_index <- 0
  
  # draw m samples with replacement from "table"
  for(i in 1:m) {
    # draw a sample (training set) with replacement from "table"
    indices <- sample(length(table[,1]), sample_size, replace = TRUE)
    
    # random forest: randomly select a subset of nfeat
    num_of_predictors <- floor(runif(1, min=1, max=length(nfeat)))
    sample_of_nfeat <- sample(nfeat, num_of_predictors)
    
    # grow a tree on this sample
    tr <- tree_grow(table[indices,], index_of_y, nmin, minleaf, sample_of_nfeat)
    
    # append this tree to the list of trees
    tree_index <- tree_index + 1
    trees[[tree_index]] <- tr
  }
  
  # return the list of m different trees
  trees
}

# test for the tree_grow_b function based on "pima.txt"
pima.dat <- read.csv("./data/pima.txt")
index_of_y <- 9
nmin <- 20
minleaf <- 5
nfeat <- c(1:8)
m <- 100
sample_size <- 700
pima.trees <- tree_grow_b(pima.dat, index_of_y, nmin, minleaf, nfeat, m, sample_size)

# predict the class label based on the voting of all the trees returned by tree_grow_b()
tree_pred_b <- function(x, trees) {
  # predictions of all the trees
  predictions <- c()
  
  # iterate through all the trees to calculate the predictions
  for(tr in trees) {
    pred <- tree_pred(x, tr)
    predictions <- c(predictions, pred)
  }
  
  # return the majority vote of all the trees
  vote_of_majority_vector(predictions)
}

# test for the tree_pred function based on "pima.txt"
# the right answer is: label = 1
# x <- c(6,148,72,35,0,33.6,0.627,50)
# label <- tree_pred_b(x, pima.trees)
# label

# the right answer is: label = 0
# x <- c(1,100,66,29,196,32.0,0.444,42)
# label <- tree_pred_b(x, pima.trees)
# label

# display the confusion matrix based on the table and the tree model
confusion_matrix <- function(table, nfeat, index_of_y, tr) {
  predictions <- c()
  for(i in 1:length(table[,1])) {
    predictions <- c(predictions, tree_pred(table[i, nfeat], tr))
  }
  
  table(table[,index_of_y], predictions, dnn = c("Actual", "Predicted"))
}

# display the confusion matrix based on the table by the bagging with random forest
confusion_matrix_b <- function(table, nfeat, index_of_y, trees) {
  predictions <- c()
  for(i in 1:length(table[,1])) {
    predictions <- c(predictions, tree_pred_b(table[i, nfeat], trees))
  }
  
  table(table[,index_of_y], predictions, dnn = c("Actual", "Predicted"))
}

# confusion matrix based on "pima.txt"
# nfeat <- c(1:8)
# index_of_y <- 9
# pima.tr.perf <- confusion_matrix(pima.dat, nfeat, index_of_y, pima.tr)
# pima.tr.perf

# confusion matrix based on "pima.txt" by the bagging with random forest
nfeat <- c(1:8)
index_of_y <- 9
pima.trees.perf <- confusion_matrix_b(pima.dat, nfeat, index_of_y, pima.trees)
pima.trees.perf
