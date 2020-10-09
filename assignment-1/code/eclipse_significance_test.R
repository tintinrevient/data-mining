# --------------------------- Load the dependent libraries ---------------------------
library(tidyverse)
library(pwr)

# --------------------------- Initialization configurations ---------------------------
#
PATH_OF_SINGLE_TREE <- paste("./models/", "tree_single_2020-10-09_14-16-19.RData", sep = "")
PATH_OF_BAGGING <- paste("./models/", "tree_bagging_2020-10-09_14-16-19.RData", sep = "")
PATH_OF_RANDOM_FOREST <- paste("./models/", "tree_random_2020-10-09_14-16-19.RData", sep = "")

# --------------------------- Load the dependent functions ---------------------------
source("tree.R")

# --------------------------- Data analysis ---------------------------
# 
# --------------------------- Load only the test dataset ---------------------------
# read semicolon-separated files
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
perf.single <- tibble(precision = result.single[[1]][1,1], recall = result.single[[1]][1, 2], accuracy = result.single[[1]][1, 3]) %>% 
  mutate(model = "single tree")

# pre-process the performance result for later data analysis
perf.bagging <- tibble(precision = result.bagging[[1]][1,1], recall = result.bagging[[1]][1, 2], accuracy = result.bagging[[1]][1, 3]) %>% 
  mutate(model = "bagging")

# pre-process the performance result for later data analysis
perf.random.forest <- tibble(precision = result.random.forest[[1]][1,1], recall = result.random.forest[[1]][1, 2], accuracy = result.random.forest[[1]][1, 3]) %>% 
  mutate(model = "random forest")

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
