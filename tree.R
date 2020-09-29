setwd("G:/My Drive/UU/Masters/Year_3/Data_mining/assignments")
pima.dat <- read.delim("pima.txt", sep = ",")
credit.dat <- read.delim("credit.txt", sep = ",")

# Compute the impurity
impurity <- function(labels) {
  p0t <- length(labels[labels == 0])/length(labels)
  p0t*(1-p0t)
}

# Compute all possible splits on the values of a given variable
compute_splits <- function(values) {
  levels_sorted <- sort(unique(values)) # get unique values of the feature (I call them the levels)
  (levels_sorted[1:length(levels_sorted)-1] + levels_sorted[2:length(levels_sorted)])/2
}

# Get the most common value
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

# Compute the best split for a given variable
bestsplit <- function(values, labels, minleaf = 0) {
  
  splits <- compute_splits(values)
  
  if (length(splits) != 0) {
    imp_pa <- impurity(labels) # parent impurity
    children <- sapply(matrix(values[, drop = F]), function(x) ifelse(x > splits, T, F)) # T is a right child, F is a left child
    if (is.null(dim(children))) {
      children <- t(children)
    }
    nrc <- rowSums(children) # count the number of 1s, i.e. number of right children
    allowed <- nrc >= minleaf & length(values) - nrc >= minleaf # check if right or left child exceed minleaf
    children <- children[allowed,]
    
    if (!is.null(children)) {
      splits <- splits[allowed, drop = F]
      nrc <- nrc[allowed, drop = F]
      
      if(is.null(dim(children))) {
        labels_lc <- labels[children == F]
        labels_rc <- labels[children]
        
        plc <- (length(values)-nrc)/length(labels)
        prc <- nrc/length(labels)
        
        imp_lc <- impurity(labels_lc)
        imp_rc <- impurity(labels_rc)
      } else {
        labels_lc <- apply(children[, , drop = F], 1, function(x) labels[x==F])
        labels_rc <- apply(children[, , drop = F], 1, function(x) labels[x])
        
        plc <- sapply(nrc[, drop = F], function(x) (length(values)-x)/length(labels))
        prc <- sapply(nrc[, drop = F], function(x) x/length(labels))
        
        imp_lc <- sapply(labels_lc[, drop = F], function(x) impurity(x))
        imp_rc <- sapply(labels_rc[, drop = F], function(x) impurity(x))
      }
      
      if (is.null(dim(imp_lc)) | is.null(dim(imp_rc)) | is.null(dim(plc)) | is.null(dim(prc))) {
        imp_lc <- t(imp_lc)
        imp_rc <- t(imp_rc)
        plc <- t(plc)
        prc <- t(prc)
      }
      
      impurity_reductions <- imp_pa - (plc*imp_lc + prc*imp_rc)
      highest_reduction <- max(impurity_reductions)[1]
      index_best_split <- match(highest_reduction, impurity_reductions)[1]
      split <- splits[index_best_split]
      c(split, highest_reduction)
    }
  } else {
    c(NA, NA)
  }
}




# Construct tree
# x: a 2D array with attribute values: each row contains the attribute values of one training example
# y: a 1D array of class labels. Binary 0 or 1. No missing values
# nmin: stop growing tree: minimum number of observations node must contain to split
# minleaf: stop growing tree: minimum number of observations for one node after split
# nfeat: number of features that should be considered each split (draw at random nfeat features and select the best one)
# For normal tree growing nfeat equals the number of features, for random forrest it is less.
# Returns: tree object 

tree_grow <- function(feats, labels, nmin, minleaf, nfeat) {
  which_feats <- sort(sample(1:ncol(feats), nfeat)) # get a random sample of n of the feats
  #  S <- sapply(feats[, which_feats, drop = F], function(feat) bestsplit(feat, labels, minleaf = 1)) # Splits per feat
  nodelist <- list(root = cbind(feats[, which_feats, drop = F], labels)) # Entire training data frame
  
  # Some arrays for storing the necessary info for the tree
  split <- c()
  feat_name <- c()
  majority_left <- c() # Is going to be filled with info on which 
  #class is the most prominents among the left side (lesser than) of the split
  majority_right <- c()
  
  while (length(nodelist)!=0){
    
    current_node <- nodelist[[1]] # pick the node of the list
    nodelist <- nodelist[-1] # remove the top node
    feats <- current_node[,1:ncol(current_node)-1] # set feats according to current node
    labels <- current_node[,ncol(current_node)] # set labels according to current node
    
    # if the impurity is 0, further splitting will not improve the tree
    # nmin is the number of observations that a node must contain at least, for it to be allowed to be split.
    if (impurity(labels)>0 & nrow(current_node) >= nmin){ 
      
      S <- sapply(feats[, , drop = F], function(feat) bestsplit(feat, labels, minleaf = 1))
      s_star <- S[1,which.max(S[2,])]
      split <- c(split, s_star)
      feat_name <- c(feat_name, names(s_star))
      left_child <- current_node[current_node[,names(s_star)] <= s_star[[1]],]
      right_child <- current_node[current_node[,names(s_star)] > s_star[[1]],]
      nl <- nrow(left_child)
      nr <- nrow(right_child)

      if(nrow(current_node) >= nmin){
        majority_left <- c(majority_left, get_mode(left_child[,ncol(left_child)]))
        majority_right <- c(majority_right, get_mode(right_child[,ncol(right_child)]))
        nodelist <- c(nodelist, list(left_child), list(right_child))
      } 
      
    } 
  }
  tree <- data.frame(split, feat_name, majority_left, majority_right)
  tree
}

#----------------------------------------------------------------------------------------------

# Test functions

## tree_grow

### Example input credit
nmin <- 2
minleaf <- 1
nfeat <- ncol(credit.dat)-1
feats <- credit.dat[,1:ncol(credit.dat)-1]
labels <- credit.dat[,ncol(credit.dat)]

tree_grow(feats, labels, nmin, minleaf, nfeat)

### Example input pima
nmin <- 20
minleaf <- 5
nfeat <- ncol(pima.dat)-1
feats <- pima.dat[,1:ncol(pima.dat)-1]
labels <- pima.dat[,ncol(pima.dat)]

tree_grow(feats, labels, nmin, minleaf, nfeat)

## tree_predict