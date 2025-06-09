# Load the dataset
dataset <- read.csv("R-Statistical-Analysis-Projects/dataset/StudentPerformanceFactors.csv")

# Basic info
dim(dataset)
str(dataset)
head(dataset)
sum(is.na(dataset))


# 1. Pearson Correlation [x = Numarical, Y = Numerical]
cor.test(dataset$Hours_Studied, dataset$Exam_Score, method = "pearson")


# 2. Spearman Correlation [x = Numarical, Y = Numerical]
cor.test(dataset$Attendance, dataset$Exam_Score, method = "spearman")


# 3. Kendall Correlation [x = Numarical/categorical, Y = Categorical/Numerical]
cor.test(dataset$Previous_Scores, dataset$Exam_Score, method = "kendall")


# 4. ANOVA [x = Numarical/Categorical, Y = categorical/Numerical]
anova_result <- aov(Exam_Score ~ Access_to_Resources, data = dataset)
summary(anova_result)


# 5. Chi-Squared [x = Categorical, Y = Categorical]
table_chi <- table(dataset$Gender, dataset$Motivation_Level)
chisq.test(table_chi)


# 6. Mutual Information
# install.packages("infotheo") # Install library

library(infotheo)

# Discretize the continuous predictor and target
x_disc <- discretize(dataset$Hours_Studied)
y_disc <- discretize(dataset$Exam_Score)

# Mutual Information [x = Categorical, Y = Categorical]
mutinformation(x_disc, y_disc)