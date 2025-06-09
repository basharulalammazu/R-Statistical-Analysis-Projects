# 📊 R Statistical Analysis Projects

Welcome to **R-Statistical-Analysis-Projects** — a collection of R scripts showcasing powerful **statistical analysis** and **data visualization** using real-world datasets.

---

## 📁 Project Overview

### 1️⃣ Student Performance Correlation Analysis  
🔍 Analyze how study habits, attendance, motivation, and more influence student exam scores using a variety of statistical methods.

- 📂 File: `1_StudentCorrelationAnalysis.R`  
- 📑 Dataset: [Student Performance Factors (Kaggle)](https://www.kaggle.com/datasets/lainguyn123/student-performance-factors)  
- 🧪 Methods Used:
  - Pearson, Spearman, Kendall Correlations
  - ANOVA
  - Chi-Squared Test
  - Mutual Information

---

### 2️⃣ Penguin Data Visualization  
🖼️ Explore the physical features of penguins across different islands through compelling visualizations.

- 📂 File: `2_PenguinsDataVisualization.R`  
- 📑 Dataset: [Palmer Penguins (Kaggle)](https://www.kaggle.com/datasets/parulpandey/palmer-archipelago-antarctica-penguin-data)  
- 📊 Visualizations Included:
  - Histogram & Density Plot
  - Box Plot & Bar Chart
  - Scatter & Violin Plot
  - Radar Chart
  - Line Graph


## 🗃️ Directory Structure

```
R-Statistical-Analysis-Projects/
│
├── 1_StudentCorrelationAnalysis.R         # Statistical correlation tests
├── 2_PenguinsDataVisualization.R          # Visual analytics on penguins
│
├── dataset/
│   ├── StudentPerformanceFactors.csv      # Student dataset
│   └── penguins_size.csv                  # Penguin dataset
│
├── assets/
│   ├── histogram_flipper_length.png
│   ├── density_flipper_length.png
│   ├── boxplot_body_mass_by_species.png
│   ├── bar_island_count.png
│   ├── scatter_culmen_length_vs_depth.png
│   ├── violin_culmen_depth_by_length_bin.png
│   ├── radar_species_means.png
│   └── line_culmen_length_by_index.png
│
└── README.md
```

---

## 🧰 Setup & Requirements

Make sure you have R installed along with these packages:

```r
install.packages(c("ggplot2", "dplyr", "readr", "infotheo", "fmsb"))
````

---

## 🚀 How to Run the Projects

### ▶ Student Correlation Analysis

1. Open `1_StudentCorrelationAnalysis.R`.
2. Ensure `StudentPerformanceFactors.csv` is placed in the `data/` folder.
3. Run the script to view correlation test results.

### ▶ Penguin Visual Analysis

1. Open `2_PenguinsDataVisualization.R`.
2. Ensure `penguins_size.csv` is placed in the `data/` folder.
3. Run the script to generate and save visuals in the `assets/` folder.

---

## 🖼️ Visualization Samples

Visual outputs saved in the `assets/` folder include:

* 🔹 Histogram & Density Plot
* 📦 Boxplot & Bar Chart
* 🎯 Scatter & Violin Plot
* 🧭 Radar Chart
* 📈 Line Graph

> Great for assignments, data stories, or visual dashboards!

---

## 👨‍💻 Author

**Basharul - Alam - Mazu** <br>
💼 BSc in CSE | American International University - Bangladesh (AIUB) <br>
🌐 [Portfolio](https://basharulalammazu.github.io) | 🐙 [GitHub](https://github.com/basharulalammazu) <br>
📧 [basharulalammazu@gmail.com](mailto:basharulalammazu@gmail.com) 

---

## 📜 License

This project is licensed under the [MIT License](LICENSE).

