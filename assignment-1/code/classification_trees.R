# Calculate the impurity based on Gini index for binary classifications
impurity_by_gini_index <- function(y) {
  
  percent_of_zero <- length(y[y == 0]) / length(y)
  percent_of_one <- 1 - percent_of_zero
  
  # Return the impurity
  percent_of_zero * percent_of_one
}

# Test for the impurity function based on Gini index
# The right answer is: impurity = 0.2314050
# y <- c(1,0,1,1,1,0,0,1,1,0,1)
# impurity <- impurity_by_gini_index(y)
# impurity

# The best split for a node based on one numeric attribute x and the label y
bestsplit <- function(x, y) {
  
  x.sorted <- sort(unique(x))
  x.sorted.length <- length(x.sorted)
  # e.g. x = c(1, 2, 3, 4, 5): x has 5 values, and thus x has (5 - 1) = 4 split points.
  # x.splitpoints = (c(1, 2, 3, 4) + c(2, 3, 4, 5)) / 2
  x.splitpoints <- (x.sorted[1:x.sorted.length-1] + x.sorted[2:x.sorted.length]) / 2
  
  impurity.parent <- impurity_by_gini_index(y)
  # Initialize the best split point and its reduction of impurity.
  # The reduction of impurity is supposed to be the maximum reduction of impurity.
  impurity.reduction.max <- 0
  x.splitpoints.best <- NULL
  
  for(x.splitpoint in x.splitpoints) {
    # The left children of a parent: x is less than and equal to the value of the split point.
    proportion.left.children <- length(y[x <= x.splitpoint]) / length(y)
    impurity.left.children <- impurity_by_gini_index(y[x <= x.splitpoint])
    
    # The right children of a parent: x is greater than the value of the split point.
    proportion.right.children <- length(y[x > x.splitpoint]) / length(y)
    impurity.right.children <- impurity_by_gini_index(y[x > x.splitpoint])
    
    # Calculate the reduction of impurity
    impurity.reduction <- impurity.parent - (proportion.left.children*impurity.left.children + proportion.right.children*impurity.right.children)
    
    # Update the best split and its reduction of impurity, which is supposed to be maximum.
    if(impurity.reduction > impurity.reduction.max) {
      impurity.reduction.max <- impurity.reduction
      x.splitpoints.best <- x.splitpoint
    }
  }
  
  # Return the vector: c(the maximum impurity reduction, the best split point)
  c(impurity.reduction.max, x.splitpoints.best)
}

# Test for the best split based on the data of credit.txt
# The right answer is: bestsplit = 36
# credit.dat <- read.csv("./data/credit.txt")
# result <- bestsplit(credit.dat[,4], credit.dat[,6])
# result[1]
# result[2]

# Calcuate the classified label by the vote of the majority of nodes
# index_of_y is the index of the label in the table
# e.g. table is c(age, married, house, income, gender, class), index_of_y is 6, and the starting index is 1 instead of 0.
vote_of_majority <- function(table, index_of_y) {
  
  vote_of_zero <- table[table[,index_of_y] == 0, ]
  vote_of_one <- table[table[,index_of_y] == 1, ]
  
  if(length(vote_of_zero[,1]) > length(vote_of_one[,1]))
    0
  else
    1
}

# Test for the vote of majority
# The right answer is 1
# vote_of_majority(credit.dat, 6)

# Grow the classification tree
tree_grow <- function(table, nmin, minleaf, nfeat) {
  
  # nodelist <- {{training data}}
  nodelist <- list(table)
  
  # The model of the tree = a list of nodes
  # Each node is a vector = c(one best feature to split the node, the best split point of that best feature, the voted classified label)
  # For the non-terminal nodes, the vector is like c(the best feature, the best split point, NA)
  # For the terminal nodes, the vector is like c(NA, NA, the voted classified label)
  
  # Initialize the model of the trained tree
  tree_nodelist <- list()
  tree_index <- 0
  
  repeat{
    
    # current_node <- select node from nodelist
    current_node <- nodelist[[1]]
    # nodelist <- nodelist - current node
    nodelist[[1]] <- NULL
    
    # nfeat + 1 is the index of the class label in the table
    # e.g. nfeat = 5, and nfeat + 1 = 6 is the index of "class" in "credit.txt"
    
    # If impurity(current node) > 0 AND observations(current node) >= nmin, the current node is allowed to be split.
    if(impurity_by_gini_index(current_node[,nfeat+1]) > 0 && length(current_node[,1]) >= nmin) {
      
      # Find one best feature to split the data
      # Feature is represented by its index in the table
      feat.best <- NA
      feat.splitpoints.best <- NA
      feat.impurity.reduction.max <- 0
      
      # Iterate through the set of candidate splits in current node.
      # Find the best split point of the best split feature with the maximum impurity reduction.
      # S <- set of candidate splits in current node
      # s* <- argmax {impurity reduction(s, current node)}, where s is in S
      for(feat in 1:nfeat) {
        result <- bestsplit(current_node[,feat], current_node[,nfeat+1])
        
        if(result[1] > feat.impurity.reduction.max) {
          feat.impurity.reduction.max <- result[1]
          feat.splitpoints.best <- result[2]
          feat.best <- feat
        }
      }
      
      # child nodes <- apply(s*, current node)
      # left_children are the rows in the table, whose value of the best-split feature is less than and equal to the best-split point.
      left_children <- current_node[current_node[,feat.best] <= feat.splitpoints.best,]
      # right_children are the rows in the table, whose value of the best-split feature is greater than the best-split point.
      right_children <- current_node[current_node[,feat.best] > feat.splitpoints.best,]
      
      # Check minleaf constraint
      if(length(left_children[,1]) < minleaf || length(right_children[,1]) < minleaf) {
        # If observations(child nodes) < minleaf
        # Stop splitting, and append the current node as the terminal node to the tree model.
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(NA, NA, vote_of_majority(current_node, nfeat+1)) 
        next
      } else {
        # If observations(child nodes) >= minleaf
        # Split the current node into child nodes, and append the current node as the non-terminal node to the tree model.
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(feat.best, feat.splitpoints.best, NA)
        
        # nodelist <- nodelist + child nodes
        nodelist[[length(nodelist) + 1]] <- left_children
        nodelist[[length(nodelist) + 1]] <- right_children
      }
      
    } else {
      
      if (impurity_by_gini_index(current_node[,nfeat+1]) == 0) {
        # If the current node is pure
        # Append the current node as the terminal node to the tree model by its pure label.
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(NA, NA, current_node[1,nfeat+1])
        
      } else {
        # If the current node is not pure
        # Append the current node as the terminal node to the tree model by its majority of labels.
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(NA, NA, vote_of_majority(current_node, nfeat+1))
      }
    }
    
    # until nodelist = ∅
    if(length(nodelist) == 0)
      break
  }
  
  # Return the model of the trained tree
  tree_nodelist
}

# Test for the tree_grow function based on "credit.txt"
# credit.dat <- read.csv("./data/credit.txt")
# nmin <- 2
# minleaf <- 1
# nfeat <- 5
# credit.tr <- tree_grow(credit.dat, nmin, minleaf, nfeat)
# credit.tr

# Test for the tree_grow function based on "pima.txt"
pima.dat <- read.csv("./data/pima.txt")
nmin <- 20
minleaf <- 5
nfeat <- 8
pima.tr <- tree_grow(pima.dat, nmin, minleaf, nfeat)

# Predict the class label based on the model of the tree returned by tree_grow()
# x: predictors
# tr_model: the model of the tree
# the model of the tree is a traversal of the tree in a breadth-first manner.
tree_pred <- function(x, tr) {
  
  while(is.na(tr[[1]][3])) {
    
    if(x[tr[[1]][1]] <= tr[[1]][2]) {
      tr[[3]] = NULL
      tr[[1]] = NULL
    } else {
      tr[[2]] = NULL
      tr[[1]] = NULL
    }
  } 
  
  # Return the classified label
  tr[[1]][3]
}

# Test for the tree_pred function based on "credit.txt"
# The right answer is: label = 0
# x <- c(46, 0, 1, 32, 0)
# label <- tree_pred(x, credit.tr)
# label

# The right answer is: label = 1
# x <- c(50, 1, 1, 28, 0)
# label <- tree_pred(x, credit.tr)
# label

# Test for the tree_pred function based on "pima.txt"
# The right answer is: label = 1
# x <- c(6,148,72,35,0,33.6,0.627,50)
# label <- tree_pred(x, pima.tr)
# label

# The right answer is: label = 0
# x <- c(1,100,66,29,196,32.0,0.444,42)
# label <- tree_pred(x, pima.tr)
# label

# Display the confusion matrix based on the table and the tree model
confusion_matrix <- function(table, nfeat, tr) {
  pred <- c()
  for(i in 1:length(table[,1])) {
    pred <- c(pred, tree_pred(table[i,1:nfeat], tr))
  }
  
  table(table[,nfeat+1], pred, dnn = c("Actual", "Predicted"))
}

# Confusion matrix based on "pima.txt"
pima.tr.perf <- confusion_matrix(pima.dat, 8, pima.tr)
pima.tr.perf

# Comparison with the decision tree generated from rpart
# The data is based on "pima.txt"
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


