library(tidyverse)
library(cluster)
library(factoextra)
library(scales)
library(plotly)

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

# SCALING

country_labels <- df_agg$`Country Name`
df_numeric <- df_agg[, -1]        
df_scaled  <- scale(df_numeric)

rownames(df_scaled) <- country_labels

# ELBOW + SILHOUETTE

fviz_nbclust(df_scaled, kmeans, method = "wss") +
  labs(
    title = "Elbow method",
    x        = "Number of Clusters (k)",
    y        = "Total Within-Cluster Sum of Squares"
  )

fviz_nbclust(df_scaled, kmeans, method = "silhouette") +
  labs(
    title    = "Silhouette Method — Optimal Number of Clusters",
    subtitle = "Higher silhouette width = better-defined clusters",
    x        = "Number of Clusters (k)",
    y        = "Average Silhouette Width"
  )

set.seed(17)  
k <- 3        

# CLUSTERING 

kmeans_result <- kmeans(df_scaled, centers = k)
df_agg$Cluster <- as.factor(kmeans_result$cluster)

levels(df_agg$Cluster) <- c(
  "1" = "Low GDP, Service-Dominant",
  "2" = "Low GDP, Agriculture-Dominant",
  "3" = "High GDP, Service-Dominant"
)


center_vals <- attr(df_scaled, "scaled:center")
scale_vals <- attr(df_scaled, "scaled:scale")

centers_orig <- sweep(kmeans_result$centers, 2,
                      scale_vals, "*") 
centers_orig <- sweep(centers_orig, 2,
                      center_vals, "+") 
centers_orig <- as.data.frame(centers_orig)
centers_orig$Cluster <- factor(
  c("Low GDP, Service-Dominant",
    "Low GDP, Agriculture-Dominant",
    "High GDP, Service-Dominant")
)
colnames(centers_orig) <- c("Avg_Unemployment", "Avg_GDP", 
                            "Avg_Agriculture", "Avg_Industry", 
                            "Avg_Services", "Cluster")

print(centers_orig)

# PLOTTING

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
  )


p_unemp <- ggplot(df_agg, aes(x = Cluster, y = Unemployment_Rate, fill = Cluster,
                              text = paste0("Country: ", `Country Name`,
                                            "<br>Unemployment: ", round(Unemployment_Rate, 1), "%"))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Unemployment Rate by Cluster",
    x     = "Cluster",
    y     = "Average Unemployment Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(p_unemp, tooltip = "text")


p <- ggplot(df_agg, aes(x = Cluster, y = GDP, fill = Cluster,
                        text = paste0("Country: ", `Country Name`,
                                      "<br>GDP: $", round(GDP / 1e9, 1), "B"))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_y_log10(labels = label_dollar(scale = 1e-9, suffix = "B")) +  
  labs(
    title = "GDP by Cluster",
    x     = "Cluster",
    y     = "Average GDP (Billions USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(p, tooltip = "text")

cluster_long <- centers_orig %>%
  select(Cluster, Avg_Agriculture, Avg_Industry, Avg_Services) %>%
  pivot_longer(cols = -Cluster, names_to = "Sector", values_to = "Percentage") %>%
  mutate(Sector = str_remove(Sector, "Avg_"))

p_sector <- ggplot(cluster_long, aes(x = Cluster, y = Percentage, fill = Sector,
                                     text = paste0("Cluster: ", Cluster,
                                                   "<br>Sector: ", Sector,
                                                   "<br>Proportion: ", round(Percentage, 1), "%"))) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Sectoral Employment Distribution by Cluster",
    x     = "Cluster",
    y     = "Proportion of Employment",
    fill  = "Sector"
  ) +
  theme_minimal()

cluster_names <- levels(df_agg$Cluster)

for (cl in cluster_names) {
  cat(paste0("\n", cl, ":\n"))
  countries <- df_agg %>%
    filter(Cluster == cl) %>%
    pull(`Country Name`)
  cat(paste(countries, collapse = ", "), "\n")
}

