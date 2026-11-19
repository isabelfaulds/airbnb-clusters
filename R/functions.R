# functions.R
# reusable helpers for the san diego airbnb project (math 167r, group 7)
# every function here works on any variable, so we write the logic once
# and apply it with sapply / lapply instead of copy-pasting code

# ---- descriptive statistics ----

# summary stats for one numeric vector
# returns a named vector so sapply can stack the results into a table
describe_numeric <- function(x) {
  stopifnot(is.numeric(x))
  miss <- sum(is.na(x))
  x <- x[!is.na(x)]
  n <- length(x)
  m <- sum(x) / n
  s <- sqrt(sum((x - m)^2) / (n - 1))
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  # sample skewness: 0 means symmetric, positive means a long right tail
  skew <- (sum((x - m)^3) / n) / (sum((x - m)^2) / n)^1.5
  c(n = n, missing = miss, mean = m, sd = s, min = min(x), q1 = q[1],
    median = q[2], q3 = q[3], max = max(x), iqr = q[3] - q[1], skew = skew)
}

# apply describe_numeric to several columns at once
# one row per variable, rounded so it fits in the report
describe_all <- function(df, vars, digits = 2) {
  out <- t(sapply(df[vars], describe_numeric))
  round(as.data.frame(out), digits)
}

# counts and percentages for a categorical variable (keeps NA as its own level)
freq_table <- function(x) {
  counts <- table(x, useNA = "ifany")
  data.frame(level = names(counts),
             n = as.vector(counts),
             percent = round(100 * as.vector(counts) / length(x), 1))
}

# ---- plots ----

# histogram next to a normal qq plot for one variable
# the qq plot is how we decide between parametric and nonparametric tests
plot_hist_qq <- function(x, label) {
  x <- x[!is.na(x)]
  hist(x, breaks = 15, col = "grey80", border = "white",
       main = label, xlab = label)
  qqnorm(x, main = paste("Normal Q-Q:", label), pch = 19, cex = 0.6)
  qqline(x, col = "firebrick", lwd = 2)
}

# ---- hypothesis tests ----

# compare one numeric variable between two groups
# step 1: shapiro-wilk on each group to check normality
# step 2: welch t-test if both groups look normal, otherwise wilcoxon rank-sum
compare_two_groups <- function(x, g, alpha = 0.05) {
  keep <- !is.na(x) & !is.na(g)
  x <- x[keep]
  g <- droplevels(factor(g[keep]))
  stopifnot(nlevels(g) == 2)
  normal_p <- tapply(x, g, function(v) shapiro.test(v)$p.value)
  both_normal <- all(normal_p > alpha)
  test <- if (both_normal) t.test(x ~ g) else wilcox.test(x ~ g, exact = FALSE)
  list(test = if (both_normal) "Welch two-sample t-test" else "Wilcoxon rank-sum test",
       shapiro_p = normal_p,
       n = tapply(x, g, length),
       median = tapply(x, g, median),
       mean = tapply(x, g, mean),
       statistic = unname(test$statistic),
       p_value = test$p.value,
       decision = if (test$p.value < alpha) "reject H0" else "fail to reject H0")
}

# test whether two categorical variables are related
# chi-square needs every expected count >= 5, otherwise we switch to fisher's exact test
test_association <- function(x, y, alpha = 0.05) {
  tab <- table(x, y)
  expected <- suppressWarnings(chisq.test(tab, correct = FALSE)$expected)
  use_chisq <- all(expected >= 5)
  test <- if (use_chisq) chisq.test(tab, correct = FALSE) else fisher.test(tab)
  list(table = tab,
       expected = round(expected, 1),
       test = if (use_chisq) "Pearson chi-square test" else "Fisher's exact test",
       statistic = if (use_chisq) unname(test$statistic) else NA,
       df = if (use_chisq) unname(test$parameter) else NA,
       p_value = test$p.value,
       decision = if (test$p.value < alpha) "reject H0" else "fail to reject H0")
}

# ---- clustering ----

# run k-means for several values of k and score each one
# within-cluster sum of squares feeds the elbow plot, silhouette measures separation
kmeans_scan <- function(X, ks = 2:6, nstart = 100, seed = 167) {
  d <- dist(X)
  fits <- lapply(ks, function(k) {
    set.seed(seed + k)
    kmeans(X, centers = k, nstart = nstart, iter.max = 100)
  })
  scores <- data.frame(
    k = ks,
    within_ss = sapply(fits, function(f) f$tot.withinss),
    silhouette = sapply(fits, function(f) mean(cluster::silhouette(f$cluster, d)[, "sil_width"])))
  names(fits) <- ks
  list(scores = scores, fits = fits)
}

# median of each variable inside each cluster, plus the cluster size
cluster_profile <- function(df, cl, vars) {
  prof <- aggregate(df[vars], by = list(cluster = cl), FUN = median, na.rm = TRUE)
  prof$n <- as.vector(table(cl))
  prof
}

# relabel clusters so cluster 1 is the cheapest, 2 the next, and so on
# k-means and pd-clustering number clusters randomly, this makes them comparable
order_clusters <- function(cl, price) {
  rank_by_price <- rank(tapply(price, cl, median))
  as.integer(rank_by_price[as.character(cl)])
}

# adjusted rand index between two clusterings, written from the formula
# 1 = identical partitions, 0 = no better than random agreement
adjusted_rand <- function(a, b) {
  tab <- table(a, b)
  pairs <- function(v) sum(choose(v, 2))
  index <- pairs(tab)
  row_p <- pairs(rowSums(tab))
  col_p <- pairs(colSums(tab))
  expected <- row_p * col_p / choose(sum(tab), 2)
  max_index <- (row_p + col_p) / 2
  (index - expected) / (max_index - expected)
}
