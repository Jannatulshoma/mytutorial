# Install and load the readxl package
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")

# Load the necessary libraries
library(readxl)
library(ggplot2)

# Read the Excel file
file_path <- "C:/Users/janna/Documents/ROI-G5.xlsx"
data_G5 <- read_excel(file_path)


# calculate the frequencies
frequency_table_G5 <- table(data_G5$Area)

# Calculate relative frequencies
relative_freq_G5 <- frequency_table / length(data_G5$Area)

# Convert the relative frequencies to a data frame for easier plotting
relative_freq_df_G5 <- data.frame(Area = as.numeric(names(relative_freq)), 
                               Relative_Frequency = as.numeric(relative_freq))

# Plot the relative frequency distribution
ggplot(relative_freq_df_G5, aes(x = Area, y = Relative_Frequency)) +
  geom_bar(stat = "identity", fill = "black", color = "black") +
  labs(title = "Relative Frequency Distribution of Nuclear Area_G5",
       x = "Nuclear Area", y = "Relative Frequency") +
  theme_minimal()


library(readxl)
library(dplyr)

# Read the Excel file
file_path <- "C:/Users/janna/Documents/ROI-G5.xlsx"
data_G5 <- read_excel(file_path)

# Filter rows where Area is greater than 100
filtered_data_G5 <- data_G5[data_G5$Area > 100, ]


# Order the filtered data by Area in descending order
ordered_data_G5 <- filtered_data_G5 %>% 
  arrange(desc(Area))

#remove if any NA it has
ordered_data_G5 <- na.omit(ordered_data_G5)

# Load necessary libraries
library(readxl)
library(ggplot2)


# Step 2: Calculate the Spearman correlation coefficient
correlation <- cor(ordered_data_G5$Protein1, ordered_data_G5$Protein2, method = "spearman")

# Step 3: Create a scatter plot with the Spearman correlation coefficient displayed
ggplot(ordered_data_G5, aes(x = Protein1, y = Protein2)) +
  geom_point() +
  labs(title = "Spearman Correlation Analysis_G5",
       x = "Protein 1", y = "Protein 2") +
  geom_text(x = Inf, y = Inf, label = paste("Spearman correlation =", round(correlation, 2)),
            hjust = 1, vjust = 1, color = "red", size = 4)


# Spearman Correlation in Normal Cell
library(readxl)
file_path <- "C:/Users/janna/Documents/ROI-J7.xlsx"
Normal_J7 <- read_excel(file_path)

library(ggplot2)
# Step 2: Calculate the Spearman correlation coefficient
correlation <- cor(Normal_J7$Protein1, Normal_J7$Protein2, method = "spearman")

# Step 3: Create a scatter plot with the Spearman correlation coefficient displayed
ggplot(Normal_J7, aes(x = Protein1, y = Protein2)) +
  geom_point() +
  labs(title = "Spearman Correlation Analysis in Normal cells",
       x = "Protein 1", y = "Protein 2") +
  geom_text(x = Inf, y = Inf, label = paste("Spearman correlation =", round(correlation, 2)),
            hjust = 1, vjust = 1, color = "red", size = 4)