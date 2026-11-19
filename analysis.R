# analysis.R
# full analysis for the san diego airbnb project (math 167r, group 7)
# run from the repo root:  source("analysis.R")
# it prints the key results, saves tables to outputs/ and plots to figures/
# report.Rmd sources this file, so every number in the report comes from here

library(cluster)        # silhouette()
library(ggplot2)        # publication quality plots
library(FPDclustering)  # PDC() = probabilistic distance clustering (install.packages("FPDclustering"))
source("R/functions.R")

dir.create("outputs", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# ---- 1. load and clean ----

# empty strings in the csv are missing values, not real categories
d <- read.csv("data/san_diego_listings.csv", na.strings = c("", "NA"))

# true/false columns come in as text, turn them into factors with readable labels
d$superhost <- factor(d$superhost, levels = c("False", "True"),
                      labels = c("Not superhost", "Superhost"))
d$guest_favorite <- factor(d$guest_favorite, levels = c("False", "True"),
                           labels = c("Not guest favorite", "Guest favorite"))
d$room_type <- factor(d$room_type, levels = c("entire_home", "private_room", "shared_room"),
                      labels = c("Entire home", "Private room", "Shared room"))

# keep listings that were actually booked in the last 12 months
# (there is no "active" column, so positive reserved days is our proxy)
d <- d[d$ttm_reserved_days > 0 & d$ttm_avg_rate > 0, ]
n_listings <- nrow(d)

# occupancy as a percent is easier to read than a 0-1 proportion
d$occupancy_pct <- 100 * d$ttm_occupancy

# ---- 2. descriptive statistics ----

# continuous variables we describe (labels are what shows up in the report)
num_vars <- c(ttm_avg_rate = "Nightly rate (USD)",
              occupancy_pct = "Occupancy (%)",
              ttm_avg_length_of_stay = "Avg stay (nights)",
              ttm_avg_min_nights = "Avg minimum nights",
              ttm_revenue = "Revenue, last 12 months (USD)",
              rating_overall = "Overall rating (1-5)",
              num_reviews = "Number of reviews",
              bedrooms = "Bedrooms",
              guests = "Max guests",
              cleaning_fee = "Cleaning fee (USD)")

desc_table <- describe_all(d, names(num_vars))
rownames(desc_table) <- num_vars
write.csv(desc_table, "outputs/descriptive_stats.csv")

# categorical variables: room type, superhost, guest favorite
# (instant_book is 67% missing and professional_management is 95% "False",
#  so we leave those two out, they don't carry useful information here)
cat_vars <- c(room_type = "Room type", superhost = "Superhost",
              guest_favorite = "Guest favorite")
freq_tables <- lapply(d[names(cat_vars)], freq_table)

# spearman correlations between the main variables (rank based, so skew doesn't matter)
cor_vars <- c("ttm_avg_rate", "occupancy_pct", "ttm_avg_length_of_stay",
              "ttm_avg_min_nights", "rating_overall", "num_reviews")
cor_matrix <- round(cor(d[cor_vars], method = "spearman", use = "pairwise.complete.obs"), 2)
write.csv(cor_matrix, "outputs/spearman_correlations.csv")

# ---- 3. plots of the variables ----

# figure 1: histogram + qq plot for the four variables we cluster on later
png("figures/fig1_hist_qq.png", width = 2000, height = 1100, res = 200)
par(mfrow = c(2, 4), mar = c(4, 4, 2.5, 1))
for (v in c("ttm_avg_rate", "occupancy_pct", "ttm_avg_length_of_stay", "rating_overall")) {
  plot_hist_qq(d[[v]], num_vars[[v]])
}
dev.off()

# figure 2: bar charts of the categorical variables
png("figures/fig2_categorical.png", width = 2000, height = 650, res = 200)
par(mfrow = c(1, 3), mar = c(4, 4, 2.5, 1))
for (v in names(cat_vars)) {
  counts <- table(d[[v]])
  barplot(counts, col = c("#2c7fb8", "#f28e2b", "#999999")[seq_along(counts)],
          main = cat_vars[[v]], ylab = "Listings", cex.names = 0.8)
}
dev.off()

# ---- 4. hypothesis tests ----

# test 1: do superhosts have a different occupancy rate than other hosts?
# H0: occupancy has the same distribution for superhosts and non-superhosts
# H1: the distributions differ (two-sided), alpha = 0.05
occ_test <- compare_two_groups(d$occupancy_pct, d$superhost)

# figure 3: occupancy by superhost status
fig3 <- ggplot(d, aes(superhost, occupancy_pct, fill = superhost)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.12, size = 1.4, alpha = 0.6) +
  scale_fill_manual(values = c("#999999", "#2c7fb8"), guide = "none") +
  labs(x = NULL, y = "Occupancy, last 12 months (%)") +
  theme_minimal(base_size = 12)
ggsave("figures/fig3_occupancy_superhost.png", fig3, width = 5, height = 3.5, dpi = 200, bg = "white")

# test 2: is superhost status related to having the guest favorite badge?
# H0: superhost status and guest favorite status are independent
# H1: they are associated, alpha = 0.05
badge_test <- test_association(d$superhost, d$guest_favorite)

# ---- 5. cluster analysis ----

# features: price and stay length are very right-skewed, so we log them first,
# then standardize all three so each one counts equally in the distances
features <- data.frame(log_rate = log(d$ttm_avg_rate),
                       occupancy = d$ttm_occupancy,
                       log_stay = log(d$ttm_avg_length_of_stay))
X <- scale(features)

# k-means for k = 2..6, keep the k with the highest average silhouette
scan <- kmeans_scan(X, ks = 2:6)
best_k <- scan$scores$k[which.max(scan$scores$silhouette)]
km <- scan$fits[[as.character(best_k)]]
write.csv(round(scan$scores, 3), "outputs/kmeans_scores.csv", row.names = FALSE)

# figure 4: elbow plot and silhouette plot side by side
png("figures/fig4_choose_k.png", width = 2000, height = 750, res = 200)
par(mfrow = c(1, 2), mar = c(4, 4.5, 2.5, 1))
plot(scan$scores$k, scan$scores$within_ss, type = "b", pch = 19,
     xlab = "Number of clusters (k)", ylab = "Within-cluster sum of squares",
     main = "Elbow method")
plot(scan$scores$k, scan$scores$silhouette, type = "b", pch = 19,
     xlab = "Number of clusters (k)", ylab = "Average silhouette width",
     main = "Silhouette method")
abline(v = best_k, lty = 2, col = "firebrick")
dev.off()

# number clusters from cheapest to most expensive so they read the same every run
d$cluster <- factor(order_clusters(km$cluster, d$ttm_avg_rate))

# pd-clustering (tortora's package) on the same standardized data and same k
# pdc gives each listing a probability of belonging to each cluster
set.seed(167)
pdc <- PDC(X, k = best_k)
d$pd_cluster <- factor(order_clusters(pdc$label, d$ttm_avg_rate))
d$pd_max_prob <- apply(pdc$probability, 1, max)

# how similar are the two clusterings, and how well separated is each one?
agreement_ari <- adjusted_rand(d$cluster, d$pd_cluster)
sil_kmeans <- max(scan$scores$silhouette)
sil_pdc <- mean(silhouette(as.integer(d$pd_cluster), dist(X))[, "sil_width"])
uncertain_share <- mean(d$pd_max_prob < 0.5)   # listings with no clear "home" cluster

# what does a typical listing in each cluster look like? (medians)
profile_vars <- c("ttm_avg_rate", "occupancy_pct", "ttm_avg_length_of_stay",
                  "ttm_avg_min_nights", "num_reviews", "bedrooms")
profiles <- cluster_profile(d, d$cluster, profile_vars)
superhost_share <- tapply(d$superhost == "Superhost", d$cluster, mean)
profiles$pct_superhost <- round(100 * superhost_share, 0)
profiles$pct_entire_home <- round(100 * tapply(d$room_type == "Entire home", d$cluster, mean), 0)
write.csv(profiles, "outputs/cluster_profiles.csv", row.names = FALSE)

# do the clusters line up with the official room types? (cross-tab)
cluster_by_room <- table(Cluster = d$cluster, `Room type` = d$room_type)

# is cluster membership related to being a superhost? (links clusters to a categorical variable)
# H0: cluster and superhost status are independent, alpha = 0.05
cluster_superhost_test <- test_association(d$cluster, d$superhost)

# figure 5: clusters in price vs occupancy, and price vs stay length
cluster_cols <- c("1" = "#1b9e77", "2" = "#d95f02", "3" = "#7570b3",
                  "4" = "#e7298a", "5" = "#66a61e", "6" = "#e6ab02")
fig5a <- ggplot(d, aes(ttm_avg_rate, occupancy_pct, colour = cluster)) +
  geom_point(size = 2, alpha = 0.8) + scale_x_log10() +
  scale_colour_manual(values = cluster_cols) +
  labs(x = "Nightly rate (USD, log scale)", y = "Occupancy (%)", colour = "Cluster") +
  theme_minimal(base_size = 11)
fig5b <- ggplot(d, aes(ttm_avg_length_of_stay, ttm_avg_rate, colour = cluster)) +
  geom_point(size = 2, alpha = 0.8) + scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = cluster_cols) +
  labs(x = "Average stay (nights, log scale)", y = "Nightly rate (USD, log scale)",
       colour = "Cluster") +
  theme_minimal(base_size = 11)
ggsave("figures/fig5a_clusters_price_occupancy.png", fig5a, width = 4.6, height = 3.4, dpi = 200, bg = "white")
ggsave("figures/fig5b_clusters_stay_price.png", fig5b, width = 4.6, height = 3.4, dpi = 200, bg = "white")

# figure 6: where the clusters are on the map (longitude / latitude)
fig6 <- ggplot(d, aes(longitude, latitude, colour = cluster)) +
  geom_point(size = 2.2, alpha = 0.8) +
  scale_colour_manual(values = cluster_cols) +
  coord_quickmap() +
  labs(x = "Longitude", y = "Latitude", colour = "Cluster") +
  theme_minimal(base_size = 11)
ggsave("figures/fig6_cluster_map.png", fig6, width = 5, height = 4, dpi = 200, bg = "white")

# rough coastal check: share of each cluster west of longitude -117.22
# (that line runs roughly through the coastal neighborhoods: la jolla, pacific beach, mission beach, point loma)
coastal_share <- round(100 * tapply(d$longitude < -117.22, d$cluster, mean), 0)

# ---- 6. print the key results ----

cat("listings analyzed:", n_listings, "\n\n")
print(desc_table)
cat("\nsuperhost vs occupancy:", occ_test$test, " p =", signif(occ_test$p_value, 3),
    "->", occ_test$decision, "\n")
cat("superhost vs guest favorite:", badge_test$test, " p =", signif(badge_test$p_value, 3),
    "->", badge_test$decision, "\n\n")
print(round(scan$scores, 3))
cat("\nchosen k:", best_k, "\n")
print(profiles)
print(cluster_by_room)
cat("\nk-means vs pd-clustering adjusted rand index:", round(agreement_ari, 2), "\n")
cat("silhouette k-means:", round(sil_kmeans, 2), " pd-clustering:", round(sil_pdc, 2), "\n")
cat("cluster vs superhost:", cluster_superhost_test$test, " p =",
    signif(cluster_superhost_test$p_value, 3), "\n")
cat("percent of each cluster on the coast (west of -117.22):", coastal_share, "\n")
cat("listings with no clear pd-cluster (max probability < 0.5):", round(100 * uncertain_share), "%\n")
