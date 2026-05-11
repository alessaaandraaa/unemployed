library(tidyverse)
library(cluster)
library(factoextra)
library(scales)

# Load and process data
df <- read_csv("UNEMPLOYED.csv") 
df_agg <- df %>%
  group_by(`Country Name`) %>%
  summarise(
    Unemployment_Rate = mean(`Unemployment Rate`, na.rm = TRUE),
    GDP               = mean(`GDP (in USD)`, na.rm = TRUE),
    Agriculture       = mean(`Employment Sector: Agriculture`, na.rm = TRUE),
    Industry          = mean(`Employment Sector: Industry`, na.rm = TRUE),
    Services          = mean(`Employment Sector: Services`, na.rm = TRUE)
  ) %>%
  drop_na()  

country_labels <- df_agg$`Country Name`
df_numeric <- df_agg[, -1]        
df_scaled  <- scale(df_numeric)
rownames(df_scaled) <- country_labels

# DIAGNOSTIC PLOTS (Runs before set.seed in original script)
p_elbow <- fviz_nbclust(df_scaled, kmeans, method = "wss") +
  labs(title = "Elbow method", x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares")
ggsave("elbow_method.png", p_elbow, width = 8, height = 6)

p_sil <- fviz_nbclust(df_scaled, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method — Optimal Number of Clusters", subtitle = "Higher silhouette width = better-defined clusters", x = "Number of Clusters (k)", y = "Average Silhouette Width")
ggsave("silhouette_method.png", p_sil, width = 8, height = 6)

# CLUSTERING (Matching original script exactly)
set.seed(17)  
k <- 3        
kmeans_result <- kmeans(df_scaled, centers = k)
df_agg$Cluster <- as.factor(kmeans_result$cluster)

# Assign levels exactly like unemployment_2.R
levels(df_agg$Cluster) <- c(
  "1" = "Low GDP, Service-Dominant",
  "2" = "Low GDP, Agriculture-Dominant",
  "3" = "High GDP, Service-Dominant"
)

# 3. Cluster Plot
p_clusters <- fviz_cluster(kmeans_result, data = df_scaled, geom = "point", ellipse = TRUE, ellipse.type = "convex", repel = TRUE, ggtheme = theme_minimal()) +
  labs(title = "K-Means Clustering of Countries", subtitle = "Based on Unemployment Rate, GDP, and Sectoral Employment (1990–2022)")
ggsave("cluster_plot.png", p_clusters, width = 10, height = 8)

# 4. Unemployment Boxplot
p_unemp <- ggplot(df_agg, aes(x = Cluster, y = Unemployment_Rate, fill = Cluster)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Unemployment Rate by Cluster", x = "Cluster", y = "Average Unemployment Rate (%)") +
  theme_minimal() + theme(legend.position = "none")
ggsave("unemployment_by_cluster.png", p_unemp, width = 8, height = 6)

# 5. GDP Boxplot
p_gdp <- ggplot(df_agg, aes(x = Cluster, y = GDP, fill = Cluster)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_y_log10(labels = label_dollar(scale = 1e-9, suffix = "B")) +  
  labs(title = "GDP by Cluster", x = "Cluster", y = "Average GDP (Billions USD)") +
  theme_minimal() + theme(legend.position = "none")
ggsave("gdp_by_cluster.png", p_gdp, width = 8, height = 6)

# 6. Sector Distribution
center_vals <- attr(df_scaled, "scaled:center")
scale_vals <- attr(df_scaled, "scaled:scale")
centers_orig <- sweep(kmeans_result$centers, 2, scale_vals, "*") 
centers_orig <- sweep(centers_orig, 2, center_vals, "+") 
centers_orig <- as.data.frame(centers_orig)

# Match centers labels to the assigned factor levels
centers_orig$Cluster <- factor(
  c("Low GDP, Service-Dominant",
    "Low GDP, Agriculture-Dominant",
    "High GDP, Service-Dominant")
)
colnames(centers_orig) <- c("Avg_Unemployment", "Avg_GDP", "Avg_Agriculture", "Avg_Industry", "Avg_Services", "Cluster")

cluster_long <- centers_orig %>%
  select(Cluster, Avg_Agriculture, Avg_Industry, Avg_Services) %>%
  pivot_longer(cols = -Cluster, names_to = "Sector", values_to = "Percentage") %>%
  mutate(Sector = str_remove(Sector, "Avg_"))

p_sector <- ggplot(cluster_long, aes(x = Cluster, y = Percentage, fill = Sector)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Sectoral Employment Distribution by Cluster", x = "Cluster", y = "Proportion of Employment", fill = "Sector") +
  theme_minimal()
ggsave("sector_by_cluster.png", p_sector, width = 8, height = 6)

# Output stats for Chapter 4
cat("\n--- CLUSTER CENTROIDS (ORIGINAL SCALE) ---\n")
print(centers_orig)
cat("\n--- SUMMARY STATISTICS BY CLUSTER ---\n")
df_agg %>% group_by(Cluster) %>% summarise(
  Count = n(),
  Avg_Unemployment = mean(Unemployment_Rate),
  Median_Unemployment = median(Unemployment_Rate),
  Avg_GDP_B = mean(GDP)/1e9,
  Avg_Agri = mean(Agriculture),
  Avg_Ind = mean(Industry),
  Avg_Serv = mean(Services)
) %>% print()
