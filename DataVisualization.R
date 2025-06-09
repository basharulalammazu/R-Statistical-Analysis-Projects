# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(fmsb)

# Create assets folder if not exists
if (!dir.exists("assets")) dir.create("assets")

# Load data
data <- read.csv("D:/University/Semester 8/Data Science/Final/LAB/Performance2/penguins_size.csv")

# Overview
str(data)
summary(data)
sum(is.na(data))

# ===== UNIVARIATE ANALYSIS =====

# 1. Histogram
hist_plot <- ggplot(data, aes(x = flipper_length_mm)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Flipper Length", x = "Flipper Length (mm)", y = "Frequency") +
  theme_minimal()
ggsave("assets/histogram_flipper_length.png", plot = hist_plot, width = 6, height = 4)

# 2. Density Plot
density_plot <- ggplot(data, aes(x = flipper_length_mm)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.6) +
  labs(title = "Density Plot of Flipper Length", x = "Flipper Length (mm)", y = "Density") +
  theme_minimal()
ggsave("assets/density_flipper_length.png", plot = density_plot, width = 6, height = 4)

# 3. Box Plot
box_plot <- ggplot(data, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  labs(title = "Body Mass by Species", x = "Species", y = "Body Mass (g)") +
  theme_minimal()
ggsave("assets/boxplot_body_mass_by_species.png", plot = box_plot, width = 6, height = 4)

# 4. Bar Graph
bar_plot <- ggplot(data, aes(x = island)) +
  geom_bar(fill = "purple") +
  labs(title = "Count of Penguins by Island", x = "Island", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("assets/bar_island_count.png", plot = bar_plot, width = 6, height = 4)

# ===== MULTIVARIATE ANALYSIS =====

# 5. Scatter Plot
scatter_plot <- ggplot(data, aes(x = culmen_length_mm, y = culmen_depth_mm, color = species)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "Culmen Depth vs Culmen Length", x = "Culmen Length (mm)", y = "Culmen Depth (mm)") +
  theme_minimal()
ggsave("assets/scatter_culmen_length_vs_depth.png", plot = scatter_plot, width = 6, height = 4)

# 6. Violin Plot
data$length_bin <- cut(data$culmen_length_mm, breaks = 4)
violin_plot <- ggplot(data, aes(x = length_bin, y = culmen_depth_mm, fill = length_bin)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(title = "Culmen Depth Across Culmen Length Ranges", x = "Culmen Length Bin", y = "Culmen Depth (mm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("assets/violin_culmen_depth_by_length_bin.png", plot = violin_plot, width = 6, height = 4)

# 7. Radar Chart
radar_data <- data %>%
  group_by(species) %>%
  summarise(across(c(culmen_length_mm, culmen_depth_mm, flipper_length_mm, body_mass_g), mean)) %>%
  as.data.frame()

radar_data_prep <- rbind(
  apply(radar_data[,-1], 2, max),
  apply(radar_data[,-1], 2, min),
  radar_data[,-1]
)
rownames(radar_data_prep) <- c("Max", "Min", radar_data$species)

# Save radar chart
png("assets/radar_species_means.png", width = 600, height = 600)
radarchart(radar_data_prep,
           axistype = 1,
           pcol = c("red", "blue", "green"),
           plwd = 2,
           plty = 1,
           cglcol = "grey", cglty = 1,
           axislabcol = "black",
           vlcex = 0.8)
legend("topright", legend = radar_data$species, col = c("red", "blue", "green"), lty = 1, lwd = 2)
dev.off()

# 8. Line Graph
data$row_index <- 1:nrow(data)
line_plot <- ggplot(data, aes(x = row_index, y = culmen_length_mm, color = species)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(title = "Culmen Length Across Individuals", x = "Index", y = "Culmen Length (mm)") +
  theme_minimal()
ggsave("assets/line_culmen_length_by_index.png", plot = line_plot, width = 6, height = 4)
