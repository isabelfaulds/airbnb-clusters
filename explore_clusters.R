# Run from the repository root: Rscript explore_clusters.R
# Exploratory analysis only: k-means partitions data even without natural groups.
library(cluster)
d <- read.csv("data/san_diego_listings.csv", na.strings = c("", "NA"))
dir.create("outputs", showWarnings = FALSE)
vars <- c("ttm_avg_rate", "ttm_occupancy", "ttm_avg_length_of_stay",
          "ttm_avg_min_nights", "cleaning_fee", "guests", "bedrooms",
          "rating_overall", "num_reviews", "instant_book")
write.csv(data.frame(variable = vars,
  missing = sapply(d[vars], function(x) sum(is.na(x)))),
  "outputs/missingness.csv", row.names = FALSE)
# There is no explicit active-status column; positive bookings is a proxy only.
keep <- complete.cases(d[c("ttm_avg_rate", "ttm_occupancy",
                          "ttm_avg_length_of_stay", "ttm_avg_min_nights")]) &
  d$ttm_reserved_days > 0 & d$ttm_avg_rate > 0 &
  d$ttm_avg_length_of_stay > 0 & d$ttm_avg_min_nights > 0
d <- d[keep, ]
features <- data.frame(log_price = log(d$ttm_avg_rate),
  occupancy = d$ttm_occupancy,
  log_stay = log(d$ttm_avg_length_of_stay),
  log_minimum = log(d$ttm_avg_min_nights))
# Standardizing gives each transformed variable equal initial weight.
fit_candidates <- function(x, label) {
  x <- scale(x)
  distances <- dist(x)
  fits <- lapply(2:6, function(k) {
    set.seed(167 + k)
    kmeans(x, centers = k, nstart = 100, iter.max = 100)
  })
  scores <- data.frame(model = label, k = 2:6,
    within_ss = sapply(fits, function(f) f$tot.withinss),
    silhouette = sapply(fits, function(f)
      mean(silhouette(f$cluster, distances)[, "sil_width"])))
  list(scores = scores, best = fits[[which.max(scores$silhouette)]])
}
a <- fit_candidates(features[1:3], "Price + occupancy + stay")
b <- fit_candidates(features, "Plus minimum nights")
scores <- rbind(a$scores, b$scores)
write.csv(scores, "outputs/cluster_scores.csv", row.names = FALSE)
d$cluster <- a$best$cluster
profile_vars <- c("ttm_avg_rate", "ttm_occupancy", "ttm_avg_length_of_stay",
                  "ttm_avg_min_nights")
profiles <- aggregate(d[profile_vars], list(cluster = d$cluster), median)
profiles$n <- as.vector(table(d$cluster))
write.csv(profiles, "outputs/cluster_medians.csv", row.names = FALSE)
write.csv(as.data.frame(table(d$cluster, d$room_type)),
          "outputs/cluster_room_types.csv", row.names = FALSE)
palette <- c("#007F86", "#E17635", "#7355A1", "#C84663", "#557B35", "#85603B")
png("outputs/exploration.png", width = 1800, height = 1400, res = 160)
par(mfrow = c(2, 2), mar = c(4.5, 4.7, 3, 1))
room_levels <- c("entire_home", "private_room", "shared_room")
plot(d$ttm_avg_rate, 100 * d$ttm_occupancy, log = "x", pch = 19,
 col = palette[match(d$room_type, room_levels)],
 xlab = "Average nightly rate (USD; log axis)", ylab = "Occupancy (%)",
 main = "Price and occupancy by official room type")
legend("topright", c("Entire home", "Private room", "Shared room"),
       col = palette[1:3], pch = 19, bty = "n", cex = .8)
plot(d$ttm_avg_rate, 100 * d$ttm_occupancy, log = "x", pch = 19,
 col = palette[d$cluster], xlab = "Average nightly rate (USD; log axis)",
 ylab = "Occupancy (%)", main = "Exploratory k-means groups (3 variables)")
legend("topright", paste("Cluster", sort(unique(d$cluster))),
       col = palette[sort(unique(d$cluster))], pch = 19, bty = "n", cex = .8)
plot(d$ttm_avg_length_of_stay, d$ttm_avg_rate, log = "xy", pch = 19,
 col = palette[d$cluster], xlab = "Average stay (nights; log axis)",
 ylab = "Average nightly rate (USD; log axis)", main = "Stay length adds another dimension")
# Leave the fourth panel blank in the shareable image.
plot.new()
dev.off()
cat("Listings retained:", nrow(d), "\n")
print(scores, row.names = FALSE)
print(profiles, row.names = FALSE)
print(table(cluster = d$cluster, room_type = d$room_type))
cat("Spearman price/occupancy correlation:",
    cor(d$ttm_avg_rate, d$ttm_occupancy, method = "spearman"), "\n")
