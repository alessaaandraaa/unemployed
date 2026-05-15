library(tidyverse)
library(cluster)
library(factoextra)
library(scales)
library(plotly)
library(gt)

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
    title = "Elbow Method",
    x     = "Number of Clusters (k)",
    y     = "Total Within-Cluster Sum of Squares"
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
scale_vals  <- attr(df_scaled, "scaled:scale")

centers_orig <- sweep(kmeans_result$centers, 2, scale_vals, "*")
centers_orig <- sweep(centers_orig, 2, center_vals, "+")
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

# ── TABLE 1: CLUSTER SUMMARY ──────────────────────────────────────────────────

centers_orig %>%
  select(Cluster, Avg_Unemployment, Avg_GDP,
         Avg_Agriculture, Avg_Industry, Avg_Services) %>%
  gt() %>%
  tab_header(
    title    = md("**K-Means Cluster Summary**"),
    subtitle = "Average values per cluster (1990–2022)"
  ) %>%
  cols_label(
    Cluster          = "Cluster",
    Avg_Unemployment = "Avg. Unemployment (%)",
    Avg_GDP          = "Avg. GDP (USD)",
    Avg_Agriculture  = "Agriculture (%)",
    Avg_Industry     = "Industry (%)",
    Avg_Services     = "Services (%)"
  ) %>%
  fmt_number(columns = c(Avg_Unemployment, Avg_Agriculture,
                         Avg_Industry, Avg_Services),
             decimals = 1) %>%
  fmt_currency(columns = Avg_GDP, currency = "USD",
               suffixing = TRUE, decimals = 1) %>%
  tab_style(
    style = cell_fill(color = "#f0f4ff"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  opt_row_striping() %>%
  tab_options(table.width = pct(100))

# CLUSTER PLOT

fviz_cluster(kmeans_result,
             data         = df_scaled,
             geom         = "point",
             ellipse      = TRUE,
             ellipse.type = "convex",
             repel        = TRUE,
             ggtheme      = theme_minimal()) +
  labs(
    title    = "K-Means Clustering of Countries",
    subtitle = "Based on Unemployment Rate, GDP, and Sectoral Employment (1990–2022)"
  )

# ── GRAPH 1: UNEMPLOYMENT RATE BY CLUSTER ────────────────────────────────────

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

# ── TABLE 2: UNEMPLOYMENT RATE RESULTS ───────────────────────────────────────

df_agg %>%
  group_by(Cluster) %>%
  summarise(
    N         = n(),
    Min       = min(Unemployment_Rate, na.rm = TRUE),
    Q1        = quantile(Unemployment_Rate, 0.25, na.rm = TRUE),
    Median    = median(Unemployment_Rate, na.rm = TRUE),
    Mean      = mean(Unemployment_Rate, na.rm = TRUE),
    Q3        = quantile(Unemployment_Rate, 0.75, na.rm = TRUE),
    Max       = max(Unemployment_Rate, na.rm = TRUE)
  ) %>%
  gt() %>%
  tab_header(
    title    = md("**Unemployment Rate by Cluster — Summary Statistics**"),
    subtitle = "All values in %"
  ) %>%
  cols_label(
    Cluster = "Cluster", N = "Countries",
    Min = "Min", Q1 = "Q1", Median = "Median",
    Mean = "Mean", Q3 = "Q3", Max = "Max"
  ) %>%
  fmt_number(columns = c(Min, Q1, Median, Mean, Q3, Max), decimals = 1) %>%
  tab_style(
    style = list(cell_fill(color = "#fff7e6"),
                 cell_text(weight = "bold")),
    locations = cells_body(
      columns = Mean
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  opt_row_striping() %>%
  tab_options(table.width = pct(100))

# ── GRAPH 2: GDP BY CLUSTER ───────────────────────────────────────────────────

p_gdp <- ggplot(df_agg, aes(x = Cluster, y = GDP, fill = Cluster,
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

ggplotly(p_gdp, tooltip = "text")

# ── TABLE 3: GDP RESULTS ──────────────────────────────────────────────────────

df_agg %>%
  group_by(Cluster) %>%
  summarise(
    N      = n(),
    Min    = min(GDP, na.rm = TRUE),
    Median = median(GDP, na.rm = TRUE),
    Mean   = mean(GDP, na.rm = TRUE),
    Max    = max(GDP, na.rm = TRUE)
  ) %>%
  gt() %>%
  tab_header(
    title    = md("**GDP by Cluster — Summary Statistics**"),
    subtitle = "Values in billions USD"
  ) %>%
  cols_label(
    Cluster = "Cluster", N = "Countries",
    Min = "Min GDP", Median = "Median GDP",
    Mean = "Mean GDP", Max = "Max GDP"
  ) %>%
  fmt_currency(columns = c(Min, Median, Mean, Max),
               currency = "USD", suffixing = TRUE, decimals = 1) %>%
  tab_style(
    style = list(cell_fill(color = "#fff7e6"),
                 cell_text(weight = "bold")),
    locations = cells_body(columns = Mean)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  opt_row_striping() %>%
  tab_options(table.width = pct(100))

# ── GRAPH 3: SECTORAL EMPLOYMENT BY CLUSTER ───────────────────────────────────

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

ggplotly(p_sector, tooltip = "text")

# ── TABLE 4: SECTORAL EMPLOYMENT RESULTS ─────────────────────────────────────

cluster_long %>%
  pivot_wider(names_from = Sector, values_from = Percentage) %>%
  mutate(Total = Agriculture + Industry + Services) %>%
  gt() %>%
  tab_header(
    title    = md("**Sectoral Employment Distribution by Cluster**"),
    subtitle = "Cluster center values (average % employed per sector)"
  ) %>%
  cols_label(
    Cluster     = "Cluster",
    Agriculture = "Agriculture (%)",
    Industry    = "Industry (%)",
    Services    = "Services (%)",
    Total       = "Total (%)"
  ) %>%
  fmt_number(columns = c(Agriculture, Industry, Services, Total), decimals = 1) %>%
  tab_style(
    style = list(cell_fill(color = "#e8f5e9"),
                 cell_text(weight = "bold")),
    locations = cells_body(columns = Services)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  data_color(
    columns   = c(Agriculture, Industry, Services),
    direction = "column",
    palette   = "Blues"
  ) %>%
  opt_row_striping() %>%
  tab_options(table.width = pct(100))

# ── COUNTRY MEMBERSHIP ────────────────────────────────────────────────────────

cluster_names <- levels(df_agg$Cluster)

for (cl in cluster_names) {
  cat(paste0("\n", cl, ":\n"))
  countries <- df_agg %>%
    filter(Cluster == cl) %>%
    pull(`Country Name`)
  cat(paste(countries, collapse = ", "), "\n")
}