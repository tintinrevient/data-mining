#------------------------------- Chi-Square Test -------------------------------------------
#
# pairwise chi-square test between the simple tree and the tree with bagging
accuracy.simple.bagging <- c(470, 191,
                             518, 143)
accuracy.matrix.simple.bagging <- matrix(accuracy.simple.bagging, nrow = 2, ncol = 2, byrow = TRUE)
dimnames(accuracy.matrix.simple.bagging) <- list(c("Simple", "Bagging"), c("Correct", "Wrong"))

chisq.test(accuracy.matrix.simple.bagging, simulate.p.value = TRUE)

# pairwise chi-square test between the simple tree and the random forest
accuracy.simple.random.forest <- c(470, 191,
                                   477, 184)
accuracy.matrix.simple.random.forest <- matrix(accuracy.simple.random.forest, nrow = 2, ncol = 2, byrow = TRUE)
dimnames(accuracy.matrix.simple.random.forest) <- list(c("Simple", "Random forest"), c("Correct", "Wrong"))

chisq.test(accuracy.matrix.simple.random.forest, simulate.p.value = TRUE)

# pairwise chi-square test between the tree with bagging and the random forest
accuracy.bagging.random.forest <- c(518, 143,
                                    477, 184)
accuracy.matrix.bagging.random.forest <- matrix(accuracy.bagging.random.forest, nrow = 2, ncol = 2, byrow = TRUE)
dimnames(accuracy.matrix.bagging.random.forest) <- list(c("Bagging", "Random forest"), c("Correct", "Wrong"))

chisq.test(accuracy.matrix.bagging.random.forest, simulate.p.value = TRUE)

#------------------------------- ANOVA -------------------------------------------
#
# pairwise ANOVA between the simple tree and the tree with bagging
preds.simple.bagging <- tibble(correct = c(rep(c(1, 0), times = c(470, 191)), rep(c(1, 0), times = c(518, 143))), 
                      models = c(rep("simple tree", 661), rep("bagging", 661))) %>%
  mutate(models = factor(models,levels=c("simple tree", "bagging")))

summary(aov(correct ~ models, data = preds.simple.bagging))

# pairwise ANOVA between the simple tree and the random forest
preds.simple.random.forest <- tibble(correct = c(rep(c(1, 0), times = c(470, 191)), rep(c(1, 0), times = c(477, 184))), 
                               models = c(rep("simple tree", 661), rep("random forest", 661))) %>%
  mutate(models = factor(models,levels=c("simple tree", "random forest")))

summary(aov(correct ~ models, data = preds.simple.random.forest))

# pairwise ANOVA between the tree with bagging and the random forest
preds.bagging.random.forest <- tibble(correct = c(rep(c(1, 0), times = c(518, 143)), rep(c(1, 0), times = c(477, 184))), 
                                     models = c(rep("bagging", 661), rep("random forest", 661))) %>%
  mutate(models = factor(models,levels=c("bagging", "random forest")))

summary(aov(correct ~ models, data = preds.bagging.random.forest))

#------------------------------- Power Test -------------------------------------------
#
#------------------------------- Power Test for Chi-Square Test -------------------------------------------
# power test for chi-square test between the simple tree and the tree with bagging
p0.1 <- (470 + 518) / (661 * 2)
p0.2 <- (191 + 143) / (661 * 2)
p1.1.1 <- 470 / 661
p1.1.2 <- 518 / 661
p1.2.1 <- 191 / 661
p1.2.2 <- 143 / 661
effect.size <- sqrt((p0.1 - p1.1.1)^2/p0.1 + (p0.1 - p1.1.2)^2/p0.1 +  (p0.2 - p1.2.1)^2/p0.2 + (p0.2 - p1.2.2)^2/p0.2)
total.samples <- 661 * 2
degree.freedom <- (2-1) * (2-1)
pwr.chisq.test(w = effect.size, df = degree.freedom, N = total.samples, sig.level=0.05)

# power test for chi-square test between the simple tree and the random forest
p0.1 <- (470 + 477) / (661 * 2)
p0.2 <- (191 + 184) / (661 * 2)
p1.1.1 <- 470 / 661
p1.1.2 <- 477 / 661
p1.2.1 <- 191 / 661
p1.2.2 <- 184 / 661
effect.size <- sqrt((p0.1 - p1.1.1)^2/p0.1 + (p0.1 - p1.1.2)^2/p0.1 +  (p0.2 - p1.2.1)^2/p0.2 + (p0.2 - p1.2.2)^2/p0.2)
total.samples <- 661 * 2
degree.freedom <- (2-1) * (2-1)
pwr.chisq.test(w = effect.size, df = degree.freedom, N = total.samples, sig.level=0.05)

# power test for chi-square test between the tree with bagging and the random forest
p0.1 <- (518 + 477) / (661 * 2)
p0.2 <- (143 + 184) / (661 * 2)
p1.1.1 <- 518 / 661
p1.1.2 <- 477 / 661
p1.2.1 <- 143 / 661
p1.2.2 <- 184 / 661
effect.size <- sqrt((p0.1 - p1.1.1)^2/p0.1 + (p0.1 - p1.1.2)^2/p0.1 +  (p0.2 - p1.2.1)^2/p0.2 + (p0.2 - p1.2.2)^2/p0.2)
total.samples <- 661 * 2
degree.freedom <- (2-1) * (2-1)
pwr.chisq.test(w = effect.size, df = degree.freedom, N = total.samples, sig.level=0.05)

#------------------------------- Power Test for ANOVA -------------------------------------------
# power test for one-way ANOVA between the simple tree and the tree with bagging
group.num <- 2
group.sample <- 661
p = 1/2
mean.g1 <- mean(c(rep(c(1, 0), times = c(470, 191))))
mean.g2 <- mean(c(rep(c(1, 0), times = c(518, 143))))
mean.grand <- (mean.g1 + mean.g2) / 2
# within-group variance
variance.within.group <- (var(c(rep(c(1, 0), times = c(470, 191)))) + var(c(rep(c(1, 0), times = c(518, 143)))))/2
# between-group variance
variance.between.group <- var(c(mean.g1, mean.g2))
# F-statistic
f.statistic <- variance.between.group / variance.within.group
effect.size <- sqrt((p*(mean.g1 - mean.grand)^2 + p*(mean.g2 - mean.grand)^2)/variance.within.group)
pwr.anova.test(k = group.num, n = group.sample, f = effect.size, sig.level = 0.05)

# power test for one-way ANOVA between the simple tree and the random forest
group.num <- 2
group.sample <- 661
p = 1/2
mean.g1 <- mean(c(rep(c(1, 0), times = c(470, 191))))
mean.g2 <- mean(c(rep(c(1, 0), times = c(477, 184))))
mean.grand <- (mean.g1 + mean.g2) / 2
# within-group variance
variance.within.group <- (var(c(rep(c(1, 0), times = c(470, 191)))) + var(c(rep(c(1, 0), times = c(477, 184)))))/2
# between-group variance
variance.between.group <- var(c(mean.g1, mean.g2))
# F-statistic
f.statistic <- variance.between.group / variance.within.group
effect.size <- sqrt((p*(mean.g1 - mean.grand)^2 + p*(mean.g2 - mean.grand)^2)/variance.within.group)
pwr.anova.test(k = group.num, n = group.sample, f = effect.size, sig.level = 0.05)

# power test for one-way ANOVA between the tree with bagging and the random forest
group.num <- 2
group.sample <- 661
p = 1/2
mean.g1 <- mean(c(rep(c(1, 0), times = c(477, 184))))
mean.g2 <- mean(c(rep(c(1, 0), times = c(518, 143))))
mean.grand <- (mean.g1 + mean.g2) / 2
# within-group variance
variance.within.group <- (var(c(rep(c(1, 0), times = c(477, 184)))) + var(c(rep(c(1, 0), times = c(518, 143)))))/2
# between-group variance
variance.between.group <- var(c(mean.g1, mean.g2))
# F-statistic
f.statistic <- variance.between.group / variance.within.group
effect.size <- sqrt((p*(mean.g1 - mean.grand)^2 + p*(mean.g2 - mean.grand)^2)/variance.within.group)
pwr.anova.test(k = group.num, n = group.sample, f = effect.size, sig.level = 0.05)
