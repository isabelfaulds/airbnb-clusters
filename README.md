# What Kinds of Airbnbs Are There? Clustering San Diego Listings

MATH 167R (Statistical Programming with R), Fall 2026, Group 7: Esha Hooda, Isabel Faulds, Brian Pham

We group San Diego Airbnb listings by how they are priced and booked (nightly rate, occupancy, length of stay) and check whether those natural groups match Airbnb's official room types. We also test two hypotheses about superhosts.

**Main findings**
- k-means finds 3 listing types (silhouette 0.41): **budget monthly rentals**, **popular short stays**, and **expensive homes that are rarely booked**. PD-clustering finds the same three (adjusted Rand index 0.63).
- The clusters cut across room types: almost everything is an "entire home," but entire homes split into a busy group and a mostly empty, expensive group near the coast.
- Superhosts have higher occupancy (median 46% vs 17%, Wilcoxon p < 0.001) and are much more likely to be Guest Favorites (chi-square p < 0.001).

## Files

| File | What it does |
|---|---|
| `data/san_diego_listings.csv` | AirROI San Diego listings (100-listing sample). The other CSVs are not used in this analysis. |
| `R/functions.R` | Reusable functions: descriptive stats, frequency tables, histogram + Q-Q plots, two-group test, association test, k-means scan, cluster profiles, adjusted Rand index |
| `analysis.R` | The full analysis, top to bottom. Saves tables to `outputs/` and figures to `figures/` |
| `report.Rmd` | The written report. Knits to Word (`report.docx`) and pulls every number from `analysis.R` |
| `CONTRIBUTIONS.md` | One-page description of what each member did |

## How to run

1. Open the project in RStudio (File → Open Project, or set the working directory to this folder).
2. Install the packages once:
   ```r
   install.packages(c("cluster", "ggplot2", "FPDclustering", "rmarkdown", "knitr", "flextable"))
   ```
3. Run the analysis: `source("analysis.R")`
4. Knit the report: open `report.Rmd` and click **Knit**, or run `rmarkdown::render("report.Rmd")`.

`outputs/`, `figures/` and `report.docx` are generated, so they are not committed. Run the steps above to recreate them.

## Methods in one paragraph

Price and stay length are log-transformed because they are strongly right-skewed. All three clustering variables are standardized so each counts equally. We run k-means for k = 2 to 6 (100 random starts each) and choose k with the elbow method and the average silhouette width. We compare the result with probabilistic distance clustering (`FPDclustering::PDC`). The hypothesis tests check normality with Shapiro–Wilk and Q-Q plots first, then use a Welch t-test or a Wilcoxon rank-sum test, and a chi-square test (or Fisher's exact test when expected counts are under 5). All tests use α = 0.05.
