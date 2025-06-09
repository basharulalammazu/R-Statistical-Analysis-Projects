library(readr)
library(dplyr)
library(ggplot2)

# Load data
data <- read.csv("R-Statistical-Analysis-Projects/dataset/penguins_size.csv")

# Overview
str(data)
summary(data)
sum(is.na(data))

# ===== UNIVARIATE ANALYSIS =====

# 1. Histogram (Numerical)
ggplot(data, aes(x = flipper_length_mm)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Flipper Length", x = "Flipper Length (mm)", y = "Frequency") +
  theme_minimal()

# 2. Density Plot
ggplot(data, aes(x = flipper_length_mm)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.6) +
  labs(title = "Density Plot of Flipper Length", x = "Flipper Length (mm)", y = "Density") +
  theme_minimal()

# 3. Box Plot
ggplot(data, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  labs(title = "Body Mass by Species", x = "Species", y = "Body Mass (g)") +
  theme_minimal()

# 4. Bar Graph (Categorical Variable)
ggplot(data, aes(x = island)) +
  geom_bar(fill = "purple") +
  labs(title = "Count of Penguins by Island", x = "Island", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ===== MULTIVARIATE ANALYSIS =====

# SCATTER PLOT (Numerical vs Numerical) 
ggplot(data, aes(x = culmen_length_mm, y = culmen_depth_mm, color = species)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "Culmen Depth vs Culmen Length", x = "Culmen Length (mm)", y = "Culmen Depth (mm)") +
  theme_minimal()

# VIOLIN PLOT (Simulating Numerical vs Numerical by Binning)
# Bin culmen_length_mm into ranges
data$length_bin <- cut(data$culmen_length_mm, breaks = 4)

ggplot(data, aes(x = length_bin, y = culmen_depth_mm, fill = length_bin)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(title = "Culmen Depth Across Culmen Length Ranges", x = "Culmen Length Bin", y = "Culmen Depth (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ========================
# RADAR CHART (Mean by Species)
# ========================
radar_data <- data %>%
  group_by(species) %>%
  summarise(across(c(culmen_length_mm, culmen_depth_mm, flipper_length_mm, body_mass_g), mean)) %>%
  as.data.frame()

# Prepare for radar (fmsb requires 2 extra rows: max & min)
radar_data_prep <- rbind(
  apply(radar_data[,-1], 2, max),
  apply(radar_data[,-1], 2, min),
  radar_data[,-1]
)
rownames(radar_data_prep) <- c("Max", "Min", radar_data$species)

# Plot radar
colors_border <- c("red", "blue", "green")
radarchart(radar_data_prep,
           axistype = 1,
           pcol = colors_border,
           plwd = 2,
           plty = 1,
           cglcol = "grey", cglty = 1,
           axislabcol = "black",
           vlcex = 0.8)
legend("topright", legend = radar_data$species, col = colors_border, lty = 1, lwd = 2)

# ========================
# LINE GRAPH (culmen_length_mm across Index)
# ========================
data$row_index <- 1:nrow(data)

ggplot(data, aes(x = row_index, y = culmen_length_mm, color = species)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(title = "Culmen Length Across Individuals", x = "Index", y = "Culmen Length (mm)") +
  theme_minimal()
