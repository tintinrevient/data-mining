# Grow the classification tree
tree_grow <- function(data, nmin, minleaf, nfeat) {
  
  nodelist <- list(data)
  
  # Tree data structure
  # list of nodes (feat.best, feat.splitpoints.best, classified label)
  # list of (parent_node, left_node, right_node)
  tree_nodelist <- list()
  tree_index = 0
  
  repeat{
    
    current_node <- nodelist[1]
    nodelist[1] = NULL
    
    # nfeat + 1 --> the class label
    if(impurity_gini_index(current_node[[1]][,nfeat+1]) > 0) {
      feat.best <- NULL
      feat.splitpoints.best <- NULL
      impurity.reduction.max <- 0
      
      # Find the best feature to split the data
      for(feat in 1:nfeat) {
        result <- bestsplit(current_node[[1]][,feat], current_node[[1]][,nfeat+1])
        
        if(result[1] > impurity.reduction.max) {
          impurity.reduction.max = result[1]
          feat.splitpoints.best = result[2]
          feat.best <- feat
        }
      }
      
      # Apply the binary split, which results in the left and right children
      left_children <- current_node[[1]][current_node[[1]][,feat.best] <= feat.splitpoints.best,]
      right_children <- current_node[[1]][current_node[[1]][,feat.best] > feat.splitpoints.best,]
      
      # Check minleaf constrint
      # If either left_children or right_children don't meet this requirement, stop splitting.
      if(length(left_children[[1]]) < minleaf || length(right_children[[1]]) < minleaf) {
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(NULL, NULL, vote_of_majority(current_node, nfeat)) 
        next
      } else {
        # Construct the tree nodelist
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(feat.best, feat.splitpoints.best, -1) 
      }
      
      # check nmin constraint: the number of observations per node
      # Append the left and right children to nodelist
      if(length(left_children[[1]]) >= nmin){
        nodelist[[length(nodelist) + 1]] = left_children
      } else {
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(-1, -1, vote_of_majority(left_children, nfeat)) 
      }
      
      if(length(right_children[[1]]) >= nmin) {
        nodelist[[length(nodelist) + 1]] = right_children
      } else {
        tree_index <- tree_index + 1
        tree_nodelist[[tree_index]] <- c(-1, -1, vote_of_majority(right_children, nfeat)) 
      }
      
    } else {
      tree_index <- tree_index + 1
      tree_nodelist[[tree_index]] <- c(-1, -1, current_node[[1]][1,nfeat+1])
    }
    
    if(length(nodelist) == 0)
      break
  }
  
  # return the best split from the root to the leaves
  tree_nodelist
}

# the test for the tree_grow function
credit.dat <- read.csv("./data/credit.txt")
nmin <- 2
minleaf <- 1
nfeat <- 5
tr <- tree_grow(credit.dat, nmin, minleaf, nfeat)

# Predict based on the classification tree
tree_pred <- function(x, tr) {
  
  while(tr[[1]][3] == -1) {
    
    if(x[tr[[1]][1]] <= tr[[1]][2]) {
      tr[[3]] = NULL
      tr[[1]] = NULL
    } else {
      tr[[2]] = NULL
      tr[[1]] = NULL
    }
  } 
  
  # return the classified label
  tr[[1]][3]
}

# the test for the tree_pred function
x <- c(46, 0, 1, 32, 0)
label <- tree_pred(x, tr)
label

x <- c(50, 1, 1, 28, 0)
label <- tree_pred(x, tr)
label

# the gini index for the binary classification
impurity_gini_index <- function(y) {
  
  total_num <- length(y)
  num_of_zeros <- length(y[y == 0])
  num_of_ones <- length(y[y == 1])
  
  impurity <- (num_of_zeros / total_num) * (num_of_ones / total_num)
}

# the test for the gini index impurity function
# impurity = 0.2314050
# y <- c(1,0,1,1,1,0,0,1,1,0,1)
# impurity <- impurity_gini_index(y)
# impurity

# the best split for a node on the attribute x and the class labels y
bestsplit <- function(x, y) {
  
  x.sorted <- sort(unique(x))
  x.sorted.length <- length(x.sorted)
  x.splitpoints <- (x.sorted[1:x.sorted.length-1] + x.sorted[2:x.sorted.length]) / 2
  
  impurity.parent <- impurity_gini_index(y)
  impurity.reduction.max <- 0
  x.splitpoints.best <- NULL
  
  for(splitpoint in x.splitpoints) {
    proportion.left.children <- length(y[x <= splitpoint]) / length(y)
    impurity.left.children <- impurity_gini_index(y[x <= splitpoint])
    
    proportion.right.children <- length(y[x > splitpoint]) / length(y)
    impurity.right.children <- impurity_gini_index(y[x > splitpoint])
    
    impurity.reduction <- impurity.parent - (proportion.left.children*impurity.left.children + proportion.right.children*impurity.right.children)
    
    if(impurity.reduction > impurity.reduction.max) {
      impurity.reduction.max <- impurity.reduction
      x.splitpoints.best <- splitpoint
    }
  }
  
  # return the vector of (the maximum impurity reduction, the best split point)
  c(impurity.reduction.max, x.splitpoints.best)
}

# the test for the best split based on the data of credit.txt
# bestsplit = 36
# credit.dat <- read.csv("./data/credit.txt")
# result <- bestsplit(credit.dat[,4],credit.dat[,6])
# result[1]
# result[2]

vote_of_majority <- function(data, nfeat) {

  vote_of_zero <- data[data[, nfeat+1] == 0, ]
  vote_of_one <- data[data[, nfeat+1] == 1, ]
  
  if(length(vote_of_zero[, nfeat+1]) > length(vote_of_one[, nfeat+1]))
    0 
  else
   1
}
