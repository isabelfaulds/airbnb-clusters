# Run from the repository root: Rscript test_price_occupancy.R
# Exploratory question: are higher nightly rates associated with lower occupancy?
# H0: population Spearman rho >= 0; H1: population Spearman rho < 0.
# R computes the one-sided p-value at the null boundary rho = 0.
d <- read.csv("data/san_diego_listings.csv", na.strings = c("", "NA"))
keep <- is.finite(d$ttm_avg_rate) & is.finite(d$ttm_occupancy) &
  d$ttm_avg_rate > 0 & d$ttm_occupancy >= 0 & d$ttm_occupancy <= 1 &
  !is.na(d$ttm_reserved_days) & d$ttm_reserved_days > 0
x <- d$ttm_avg_rate[keep]
y <- d$ttm_occupancy[keep]
# Ties are present, so use the asymptotic Spearman p-value.
test <- cor.test(x, y, method = "spearman", alternative = "less", exact = FALSE)
# Also report a two-sided result because the direction was explored previously.
two_sided <- cor.test(x, y, method = "spearman",
                      alternative = "two.sided", exact = FALSE)
dir.create("outputs", showWarnings = FALSE)
results <- data.frame(n = length(x), excluded = nrow(d) - length(x),
  spearman_rho = unname(test$estimate),
  p_one_sided_negative = test$p.value, p_two_sided = two_sided$p.value)
write.csv(results, "outputs/price_occupancy_test.csv", row.names = FALSE)
report <- c(
  "Exploratory Spearman correlation: nightly price and occupancy",
  "H0: population Spearman rho >= 0. H1: population Spearman rho < 0.",
  sprintf("Included listings: %d; excluded: %d.", length(x), nrow(d) - length(x)),
  sprintf("Spearman rho: %.4f", test$estimate),
  sprintf("One-sided p-value (negative association): %.6g", test$p.value),
  sprintf("Two-sided p-value: %.6g", two_sided$p.value),
  "Method: asymptotic Spearman test (ties present); alpha = 0.05.",
  if (test$p.value < .05) "Reject H0 at alpha = 0.05: evidence of a negative association."
  else "Do not reject H0 at alpha = 0.05.",
  "Exploratory: the direction was chosen after earlier inspection of these data.",
  "Assumes independent listings; shared hosts may violate independence.",
  "Population generalization depends on the sampling design of these 100 listings.",
  "Association does not establish that changing price causes occupancy to change.")
writeLines(report, "outputs/price_occupancy_test.txt")
png("outputs/price_occupancy_test.png", width = 1400, height = 1000, res = 160)
par(mar = c(5, 5, 5, 2))
plot(x, 100 * y, log = "x", pch = 19, col = "#007F86AA",
  xlab = "Average nightly rate (USD; log axis)", ylab = "Occupancy (%)",
  main = "Price and occupancy: exploratory Spearman test")
mtext(sprintf("n = %d | rho = %.3f | one-sided p = %.4g",
              length(x), test$estimate, test$p.value), side = 3, line = .5)
dev.off()
cat(paste(report, collapse = "\n"), "\n")
