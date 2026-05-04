# ============================================================
# Unemployment Rate - Multiple Regression Analysis
# ============================================================

# --- 1. Install & Load Required Libraries ---
library(tidyverse)
library(cluster)
library(factoextra)
library(scales)



# --- 2. Import Data ---
df <- read_csv("UNEMPLOYED.csv")   # <-- change to your actual filename
# ---- 3. Prepare Data for Clustering ----
  
  # Aggregate by Country (average across all years)
  df_agg <- df %>%
  group_by(`Country Name`) %>%
  summarise(
    Unemployment_Rate = mean(`Unemployment Rate`, na.rm = TRUE),
    GDP               = mean(`GDP (in USD)`, na.rm = TRUE),
    Agriculture       = mean(`Employment Sector: Agriculture`, na.rm = TRUE),
    Industry          = mean(`Employment Sector: Industry`, na.rm = TRUE),
    Services          = mean(`Employment Sector: Services`, na.rm = TRUE)
  ) %>%
  drop_na()  # Remove rows with missing values

# Save country names for labeling later
country_labels <- df_agg$`Country Name`

# Scale the numeric features (important for K-Means)
df_scaled <- df_agg %>%
  select(-`Country Name`) %>%
  scale()

rownames(df_scaled) <- country_labels

# ---- 4. Determine Optimal Number of Clusters ----

# Method 1: Elbow Method
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  labs(
    title    = "Elbow Method — Optimal Number of Clusters",
    subtitle = "Look for the 'elbow' where inertia stops dropping sharply",
    x        = "Number of Clusters (k)",
    y        = "Total Within-Cluster Sum of Squares"
  )

# Method 2: Silhouette Method
fviz_nbclust(df_scaled, kmeans, method = "silhouette") +
  labs(
    title    = "Silhouette Method — Optimal Number of Clusters",
    subtitle = "Higher silhouette width = better-defined clusters",
    x        = "Number of Clusters (k)",
    y        = "Average Silhouette Width"
  )

# ---- 5. Run K-Means Clustering ----
set.seed(123)  # For reproducibility
k <- 3         # Adjust based on elbow/silhouette results

kmeans_result <- kmeans(df_scaled, centers = k, nstart = 25)

# Add cluster labels back to aggregated data
df_agg$Cluster <- as.factor(kmeans_result$cluster)

cat("\n--- Cluster Sizes ---\n")
print(table(df_agg$Cluster))

# ---- 6. Cluster Summary ----
cat("\n--- Cluster Means (Original Scale) ---\n")
cluster_summary <- df_agg %>%
  group_by(Cluster) %>%
  summarise(
    Count            = n(),
    Avg_Unemployment = mean(Unemployment_Rate),
    Avg_GDP          = mean(GDP),
    Avg_Agriculture  = mean(Agriculture),
    Avg_Industry     = mean(Industry),
    Avg_Services     = mean(Services)
  )
print(cluster_summary)

# ---- 7. Visualizations ----

# 7a. Cluster Plot (PCA-reduced to 2D)
fviz_cluster(kmeans_result,
             data        = df_scaled,
             geom        = "point",
             ellipse     = TRUE,
             ellipse.type = "convex",
             repel       = TRUE,
             ggtheme     = theme_minimal()) +
  labs(
    title    = "K-Means Clustering of Countries",
    subtitle = "Based on Unemployment Rate, GDP, and Sectoral Employment (1990–2022)",
    caption  = "Dimensionality reduced via PCA for visualization"
  )

# 7b. Boxplot — Unemployment Rate by Cluster
ggplot(df_agg, aes(x = Cluster, y = Unemployment_Rate, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Unemployment Rate by Cluster",
    x     = "Cluster",
    y     = "Average Unemployment Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 7c. Boxplot — GDP by Cluster
ggplot(df_agg, aes(x = Cluster, y = GDP, fill = Cluster)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_y_continuous(labels = label_dollar(scale = 1e-9, suffix = "B")) +
  labs(
    title = "GDP by Cluster",
    x     = "Cluster",
    y     = "Average GDP (Billions USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 7d. Sectoral Employment by Cluster (stacked bar)
cluster_long <- cluster_summary %>%
  select(Cluster, Avg_Agriculture, Avg_Industry, Avg_Services) %>%
  pivot_longer(cols = -Cluster, names_to = "Sector", values_to = "Percentage") %>%
  mutate(Sector = str_remove(Sector, "Avg_"))

ggplot(cluster_long, aes(x = Cluster, y = Percentage, fill = Sector)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Sectoral Employment Distribution by Cluster",
    x     = "Cluster",
    y     = "Proportion of Employment",
    fill  = "Sector"
  ) +
  theme_minimal()

# ---- 8. List Countries per Cluster ----
cat("\n--- Countries per Cluster ---\n")
for (i in 1:k) {
  cat(paste0("\nCluster ", i, ":\n"))
  countries <- df_agg %>%
    filter(Cluster == i) %>%
    pull(`Country Name`)
  cat(paste(countries, collapse = ", "), "\n")
}

