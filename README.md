# Environmental-Adaptations-and-Temperature-Effects-on-C.-elegans-Strain-Distribution
First, download strain data from: 
https://caendr.org/data/data-release/c-elegans/latest
The csv file name is: 20231213_c_elegans_strain_data 
This dataset Includes strain, isotype, location information, and more.

Follow the R script code attached and these are the figures you can expect to get once done with this analysis!

This code was created on macOS 15.3

# Install necessary packages if not already installed
install.packages("tidyverse")
install.packages("BiocManager")
BiocManager::install("DESeq2")
install.packages("readr")
install.packages("pheatmap")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("factoextra")

                 
# Load packages
library(tidyverse)
library(DESeq2)
library(pheatmap)
library(ggplot2)
library(BiocManager)
library(readr)
library(readr)
library(factoextra)

#File Directory
getwd()
setwd("/Users/lesliesouthern/Desktop/caenor")
# Change this to your actual folder path

df <- read_csv("strain_data.csv")

# Read the CSV file
df <- read.csv("strain_data.csv", stringsAsFactors = FALSE)

# Read the CSV with automatic column type detection
df <- read_csv("strain_data.csv", show_col_types = FALSE)

# Read the CSV, skipping the first row (which is acting as column names)
df <- read_csv("strain_data.csv", col_names = TRUE, skip = 1, show_col_types = FALSE)

# Check the first few rows and structure
head(df)
str(df)
colnames(df)

sapply(df, function(x) length(unique(x)))  # Count unique values per column

ggplot(df, aes(x = as.numeric(ambient_temp))) +
  geom_histogram(bins = 30, fill = "pink", alpha = 0.6) +
  labs(title = "Distribution of Ambient Temperatures", x = "Temperature (°C)", y = "Frequency")

ggplot(df, aes(x = landscape, fill = landscape)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Strain Distribution by Landscape", x = "Landscape Type", y = "Count")

ggplot(df, aes(x = substrate, y = as.numeric(ambient_temp))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Temperature Variation by Substrate Type", x = "Substrate Type", y = "Temperature (°C)")

anova_result <- aov(as.numeric(ambient_temp) ~ landscape, data = df)
summary(anova_result)

chisq.test(table(df$substrate, df$landscape))
+ Since the p-value is very small, it means there is a significant association between substrate type and landscape.
+ In other words, C. elegans strains are not randomly distributed across landscapes based on their substrate preferences.

# Select relevant numerical columns
df_numeric <- df[, c("latitude", "longitude", "ambient_temp", "ambient_humidity", "substrate_temp")]

# Convert to numeric (ignoring warnings)
df_numeric <- data.frame(lapply(df_numeric, function(x) as.numeric(as.character(x))))

# Remove rows with missing values
df_numeric <- na.omit(df_numeric)

# Perform PCA
pca_result <- prcomp(df_numeric, scale. = TRUE)

# View summary of PCA
summary(pca_result)
library(ggplot2)

PC1 (53.99%) explains most of the variation in your data.
PC2 (30.26%) explains additional variation.
Together, PC1 + PC2 = 84.25%, meaning the first two PCs capture most of the meaningful differences.

# Extract PCA loadings (importance of each trait)
pca_loadings <- pca_result$rotation

# View loadings for PC1 and PC2
pca_loadings[, 1:2]

PC1 (53.99% Variance)
Latitude & Longitude drive variation → likely reflecting geographic structure in strains.
Higher temperatures increase PC1 → suggesting temperature adaptation.
PC2 (30.26% Variance)
Substrate Temp & Humidity are major drivers → indicating local microenvironment adaptations.

# Load required libraries
library(ggplot2)
library(cluster)
library(dplyr)
library(tidyverse)

str(df) # See the structure of your original dataframe
class(env_data)  # Should return "data.frame"
head(env_data)
colSums(is.na(df))
df$ambient_temp[is.na(df$ambient_temp)] <- mean(df$ambient_temp, na.rm = TRUE)
env_data <- df %>%
  select(latitude, longitude, ambient_temp, ambient_humidity, substrate_temp) %>%
  drop_na()  # Removes rows where any of these are still NA
set.seed(123)
km_res <- kmeans(scale(env_data), centers = 3, nstart = 25)
library(ggplot2)
env_data$cluster <- as.factor(km_res$cluster)

ggplot(env_data, aes(x = latitude, y = longitude, color = cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Clustering of *C. elegans* Strains by Environmental Adaptation")







