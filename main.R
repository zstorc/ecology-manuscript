### R script for data preparation and multivariate models

## Load required packages
library(dplyr)
library(tidyr)
library(vegan)
library(ecodist)

## Import dataframes
species_comp <- read.csv('data/species_comp.csv')
climate_df <- read.csv('data/AWE012.csv')
Seq_YR_Diff <- read.csv('data/seq_yr_diff.csv', header = FALSE)


### Community Data Preparation

## Data Selection
species_comp <- species_comp %>%
  # Filter rows for the first 3 years of Sequence "Age"
  filter(Age <= 3) %>%
  # Filter rows where treatment is "N" and Sequence is not "VII"
  filter(TMT == "N" & Sequence != "VII") %>%
  # Remove treatment column
  select(-TMT) %>%
  # Remove columns with unidentified species
  select(-starts_with("UNK")) %>%
  # Sort by Age, Year, Sequence, Plot, and Subplot
  arrange(Age, Year, Sequence, Plot, Subplot)

# Calculate the frequency of occurrence for each species
species_comp123 <- species_comp[species_comp$Age %in% 1:3, ]
freq <- colSums(species_comp123 > 0) / colSums(!is.na(species_comp123))

# Select only species that occur in at least 0.75% of subplots
species_comp <- species_comp[, freq >= 0.0075]

## Convert to Relative Cover

# Define cover column names by index
cover_cols <- names(species_comp)[6:ncol(species_comp)]

# Sum species cover columns row-wise to get total cover per subplot
species_comp$total_cover <- rowSums(species_comp[, cover_cols])

# Covert species cover values to relative cover
species_comp <- species_comp %>%
  rowwise() %>%
  mutate(across(all_of(cover_cols), ~ . / total_cover)) %>%
  select(-total_cover) %>%
  arrange(Age, Year, Sequence, Plot, Subplot) %>%
  ungroup()

## Calculate Plot Level Averages

# Calculate average species cover by plot and sort values
species_by_plot <- species_comp %>%
  group_by(Year, Sequence, Plot, Age) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  arrange(Age, Year, Sequence, Plot) %>%
  ungroup()


### Climate Data Preparation

# Select rows with years relevant to study (2010-2022)
climate_df <- climate_df %>%
  filter(RECYEAR >= 2010, RECYEAR <= 2022)

# Convert month numbers to their name abbreviation
climate_df$RECMONTH <- month.abb[climate_df$RECMONTH]

# Create a list containing all month abbreviations in order for reindexing
month_order <- month.abb

## Precipitation 

# Set the data type of daily precipitation column to numeric
climate_df$DPPT <- as.numeric(as.character(climate_df$DPPT))

# Calculate total precipitation by month, pivoting months to columns
monthly_ppt <- climate_df %>%
  group_by(RECYEAR, RECMONTH) %>%
  summarise(DPPT = sum(DPPT, na.rm = TRUE)) %>%
  pivot_wider(names_from = RECMONTH, values_from = DPPT) %>%
  ungroup()

# Reorder columns based on month order
monthly_ppt <- monthly_ppt[, month_order]

# Create a dataframe with precipitation summaries
ppt_summary <- data.frame(Year = 2010:2022, 
                          # Total yearly precipitation
                          YR_PPT = rowSums(monthly_ppt),
                          # Total June/July precipitation
                          JJ_PPT = rowSums(monthly_ppt[, c("Jun", "Jul")])
)

# Create a table of daily precipitation vectors by year
Daily_YR_PPT <- climate_df %>%
  group_by(RECYEAR, DAYOFYEAR) %>%
  summarise(DPPT = sum(DPPT, na.rm = TRUE)) %>%
  pivot_wider(names_from = DAYOFYEAR, values_from = DPPT) %>%
  rename(Year = RECYEAR) %>%
  ungroup()

# Create a table of weekly precipitation vectors by year
Weekly_YR_PPT <- climate_df %>%
  mutate(WEEKOFYEAR = ceiling(DAYOFYEAR / 7)) %>%
  group_by(RECYEAR, WEEKOFYEAR) %>%
  summarise(DPPT = sum(DPPT, na.rm = TRUE)) %>%
  pivot_wider(names_from = WEEKOFYEAR, values_from = DPPT) %>%
  rename(Year = RECYEAR) %>%
  ungroup()

# Create a table of monthly precipitation vectors by year
Monthly_YR_PPT <- climate_df %>%
  group_by(RECYEAR, RECMONTH) %>%
  summarise(DPPT = sum(DPPT, na.rm = TRUE)) %>%
  pivot_wider(names_from = RECMONTH, values_from = DPPT) %>%
  rename(Year = RECYEAR) %>%
  ungroup()

# Create a table of daily June/July precipitation vectors by year
Daily_JJ_PPT <- climate_df %>%
  filter(RECMONTH %in% c("Jun", "Jul")) %>%
  group_by(RECYEAR, DAYOFYEAR) %>%
  summarise(DPPT = sum(DPPT, na.rm = TRUE)) %>%
  pivot_wider(names_from = DAYOFYEAR, values_from = DPPT) %>%
  rename(Year = RECYEAR) %>%
  select(-"152", -"213") %>%
  ungroup()

# Create a table of weekly June/July precipitation vectors by year
Weekly_JJ_PPT <- climate_df %>%
  filter(RECMONTH %in% c("Jun", "Jul")) %>%
  mutate(WEEKOFYEAR = ceiling(DAYOFYEAR / 7)) %>%
  group_by(RECYEAR, WEEKOFYEAR) %>%
  summarise(DPPT = sum(DPPT, na.rm = TRUE)) %>%
  pivot_wider(names_from = WEEKOFYEAR, values_from = DPPT) %>%
  rename(Year = RECYEAR) %>%
  ungroup()

# Create a table of monthly June/July precipitation vectors by year
Monthly_JJ_PPT <- climate_df %>%
  filter(RECMONTH %in% c("Jun", "Jul")) %>%
  group_by(RECYEAR, RECMONTH) %>%
  summarise(DPPT = sum(DPPT, na.rm = TRUE)) %>%
  pivot_wider(names_from = RECMONTH, values_from = DPPT) %>%
  rename(Year = RECYEAR) %>%
  ungroup()

## Average Temperature

# Set the data type of the average temperature column to numeric
climate_df$TAVE <- as.numeric(as.character(climate_df$TAVE))

# Calculate average temperature by month, pivoting months to columns
monthly_avg_temp <- climate_df %>%
  group_by(RECYEAR, RECMONTH) %>%
  summarise(TAVE = mean(TAVE, na.rm = TRUE)) %>%
  pivot_wider(names_from = RECMONTH, values_from = TAVE) %>%
  ungroup()

# Reorder columns based on the month order
monthly_avg_temp <- monthly_avg_temp[, month_order]

# Create a dataframe with average temperature summaries
avg_temp_summary <- data.frame(Year = 2010:2022,
                               # Average yearly temperature
                               YR_AvgTemp = rowMeans(monthly_avg_temp),
                               # Average June/July temperature
                               JJ_AvgTemp = rowMeans(monthly_avg_temp[, c("Jun", "Jul")])
)

# Create a table of daily average temperature vectors by year
Daily_YR_AvgTemp <- climate_df %>%
  group_by(RECYEAR, DAYOFYEAR) %>%
  summarise(TAVE = sum(TAVE, na.rm = TRUE)) %>%
  pivot_wider(names_from = DAYOFYEAR, values_from = TAVE) %>%
  rename(Year = RECYEAR) %>%
  ungroup() 

# Create a table of weekly average temperature vectors by year
Weekly_YR_AvgTemp <- climate_df %>%
  mutate(WEEKOFYEAR = ceiling(DAYOFYEAR / 7)) %>%
  group_by(RECYEAR, WEEKOFYEAR) %>%
  summarise(TAVE = mean(TAVE, na.rm = TRUE)) %>%
  pivot_wider(names_from = WEEKOFYEAR, values_from = TAVE) %>%
  rename(Year = RECYEAR) %>%
  ungroup()

# Create a table of monthly average temperature vectors by year
Monthly_YR_AvgTemp <- climate_df %>%
  group_by(RECYEAR, RECMONTH) %>%
  summarise(TAVE = mean(TAVE, na.rm = TRUE)) %>%
  pivot_wider(names_from = RECMONTH, values_from = TAVE) %>%
  rename(Year = RECYEAR) %>%
  ungroup()

# Create a table of daily June/July average temperature vectors by year
Daily_JJ_AvgTemp <- climate_df %>%
  filter(RECMONTH %in% c("Jun", "Jul")) %>%
  group_by(RECYEAR, DAYOFYEAR) %>%
  summarise(TAVE = sum(TAVE, na.rm = TRUE)) %>%
  pivot_wider(names_from = DAYOFYEAR, values_from = TAVE) %>%
  rename(Year = RECYEAR) %>%
  select(-"152", -"213") %>%
  ungroup() 

# Create a table of weekly June/July average temperature vectors by year
Weekly_JJ_AvgTemp <- climate_df %>%
  filter(RECMONTH %in% c("Jun", "Jul")) %>%
  mutate(WEEKOFYEAR = ceiling(DAYOFYEAR / 7)) %>%
  group_by(RECYEAR, WEEKOFYEAR) %>%
  summarise(TAVE = mean(TAVE, na.rm = TRUE)) %>%
  pivot_wider(names_from = WEEKOFYEAR, values_from = TAVE) %>%
  rename(Year = RECYEAR) %>%
  ungroup()

# Create a table of monthly June/July average temperature vectors by year
Monthly_JJ_AvgTemp <- climate_df %>%
  filter(RECMONTH %in% c("Jun", "Jul")) %>%
  group_by(RECYEAR, RECMONTH) %>%
  summarise(TAVE = mean(TAVE, na.rm = TRUE)) %>%
  pivot_wider(names_from = RECMONTH, values_from = TAVE) %>%
  rename(Year = RECYEAR) %>%
  ungroup()

## Climate Summary by Plot

# Merge climate summary data frames together by year
climate_summary <- merge(ppt_summary, avg_temp_summary, by = "Year")

# Assign plot level factor columns to new object
plot_factor_cols <- species_by_plot[, c(1:4)]

# Merge climate summary dataframe with plot level factor columns
climate_by_plot <- merge(plot_factor_cols, climate_summary, by.x = "Year", by.y = "Year") %>%
  arrange(Age, Year, Sequence, Plot)

#Yearly precipitation vectors
Weekly_YR_PPT <- merge(plot_factor_cols, Weekly_YR_PPT, by.x = "Year", by.y = "Year") %>%
  arrange(Age, Year, Sequence, Plot)
Monthly_YR_PPT <- merge(plot_factor_cols, Monthly_YR_PPT, by.x = "Year", by.y = "Year") %>%
  arrange(Age, Year, Sequence, Plot)

#June/July precipitation vectors
Daily_JJ_PPT <- merge(plot_factor_cols, Daily_JJ_PPT, by.x = "Year", by.y = "Year") %>%
  arrange(Age, Year, Sequence, Plot)
Weekly_JJ_PPT <- merge(plot_factor_cols, Weekly_JJ_PPT, by.x = "Year", by.y = "Year") %>%
  arrange(Age, Year, Sequence, Plot)

#Yearly average temp vectors
Weekly_YR_AvgTemp <- merge(plot_factor_cols, Weekly_YR_AvgTemp, by.x = "Year", by.y = "Year") %>%
  arrange(Age, Year, Sequence, Plot)
Monthly_YR_AvgTemp <- merge(plot_factor_cols, Monthly_YR_AvgTemp, by.x = "Year", by.y = "Year") %>%
  arrange(Age, Year, Sequence, Plot)

#June/July average temp vectors
Daily_JJ_AvgTemp <- merge(plot_factor_cols, Daily_JJ_AvgTemp, by.x = "Year", by.y = "Year") %>%
  arrange(Age, Year, Sequence, Plot)
Weekly_JJ_AvgTemp <- merge(plot_factor_cols, Weekly_JJ_AvgTemp, by.x = "Year", by.y = "Year") %>%
  arrange(Age, Year, Sequence, Plot)


### Subset Dataframes for Dissimilarity Matrices

## Subset community dataframe by sequence age for the first 3 years
for (age in 1:3) {
  # Create community data subsets: "community_age1", "community_age2" and "community_age3"
  assign(paste0("community_age", age), species_by_plot %>% 
    filter(Age == age) %>%
    select(-Year, -Sequence, -Plot, -Age) %>% # Remove descriptor columns
    select_if(colSums(., na.rm = TRUE) > 0)) # Remove columns with 0 species
}
  
## Subset climate dataframe by sequence planting year
climate_age1 <- climate_by_plot %>%
  filter(Age == 1) %>%
  select(-Year, -Sequence, -Plot, -Age) %>% # Remove descriptor columns
  vegan::decostand('range') # express climate variables in [0,1] range
  
## Subset Weekly_YR_PPT dataframe by sequence planting year
Weekly_YR_PPT <- Weekly_YR_PPT %>%
  filter(Age == 1) %>%
  select(-Year, -Sequence, -Plot, -Age) # Remove descriptor columns

## Subset Monthly_YR_PPT dataframe by sequence planting year
Monthly_YR_PPT <- Monthly_YR_PPT %>%
  filter(Age == 1) %>%
  select(-Year, -Sequence, -Plot, -Age) # Remove descriptor columns
  
## Subset Daily_JJ_PPT dataframe by sequence planting year
Daily_JJ_PPT <- Daily_JJ_PPT %>%
  filter(Age == 1) %>%
  select(-Year, -Sequence, -Plot, -Age) # Remove descriptor columns
  
## Subset Weekly_JJ_PPT dataframe by sequence planting year
Weekly_JJ_PPT <- Weekly_JJ_PPT %>%
  filter(Age == 1) %>%
  select(-Year, -Sequence, -Plot, -Age) # Remove descriptor columns
  
## Subset Weekly_YR_AvgTemp dataframe by sequence planting year
Weekly_YR_AvgTemp <- Weekly_YR_AvgTemp %>%
  filter(Age == 1) %>%
  select(-Year, -Sequence, -Plot, -Age) # Remove descriptor columns

## Subset Monthly_YR_AvgTemp dataframe by sequence planting year
Monthly_YR_AvgTemp <- Monthly_YR_AvgTemp %>%
  filter(Age == 1) %>%
  select(-Year, -Sequence, -Plot, -Age) # Remove descriptor columns
 
## Subset Daily_JJ_AvgTemp dataframe by sequence planting year
Daily_JJ_AvgTemp <- Daily_JJ_AvgTemp %>%
  filter(Age == 1) %>%
  select(-Year, -Sequence, -Plot, -Age) # Remove descriptor columns
 
## Subset Weekly_JJ_AvgTemp dataframe by sequence planting year
Weekly_JJ_AvgTemp <- Weekly_JJ_AvgTemp %>%
  filter(Age == 1) %>%
  select(-Year, -Sequence, -Plot, -Age) # Remove descriptor columns

 
### Create Dissimilarity Matrices

# Community Age 1
community_matrix1 <- vegdist(community_age1, method='bray', binary=T)
# Community Age 2
community_matrix2 <- vegdist(community_age2, method='bray', binary=T)
# Community Age 3
community_matrix3 <- vegdist(community_age3, method='bray', binary=T)

# Sequence year difference
Seq_YR_Diff_Matrix <- as.dist(as.matrix(Seq_YR_Diff))

# Function to scale climate distance matrices
scale <- function(matrix) {
  min_val <- min(matrix)
  max_val <- max(matrix) 
  scaled_matrix <- (matrix - min_val) / (max_val - min_val)
  return(scaled_matrix)
}

# Precipitation
Weekly_YR_PPT_Matrix <- scale(dist(Weekly_YR_PPT))
Monthly_YR_PPT_Matrix <- scale(dist(Monthly_YR_PPT))
Daily_JJ_PPT_Matrix <- scale(dist(Daily_JJ_PPT))
Weekly_JJ_PPT_Matrix <- scale(dist(Weekly_JJ_PPT))

# Temperature
Weekly_YR_AvgTemp_Matrix <- scale(dist(Weekly_YR_AvgTemp))
Monthly_YR_AvgTemp_Matrix <- scale(dist(Monthly_YR_AvgTemp))
Daily_JJ_AvgTemp_Matrix <- scale(dist(Daily_JJ_AvgTemp))
Weekly_JJ_AvgTemp_Matrix <- scale(dist(Weekly_JJ_AvgTemp))


### Multiple Regression on Distance Matrices

## Community Age 1 Dissimilarity Models

# Set seed for permutation tests
set.seed(123)

# Precipitation individually
ecodist::MRM(community_matrix1 ~ Weekly_YR_PPT_Matrix)
ecodist::MRM(community_matrix1 ~ Monthly_YR_PPT_Matrix)
ecodist::MRM(community_matrix1 ~ Daily_JJ_PPT_Matrix)
ecodist::MRM(community_matrix1 ~ Weekly_JJ_PPT_Matrix)

# Temperature individually
ecodist::MRM(community_matrix1 ~ Weekly_YR_AvgTemp_Matrix)
ecodist::MRM(community_matrix1 ~ Monthly_YR_AvgTemp_Matrix)
ecodist::MRM(community_matrix1 ~ Daily_JJ_AvgTemp_Matrix)
ecodist::MRM(community_matrix1 ~ Weekly_JJ_AvgTemp_Matrix)

# Precipitation while accounting for difference in planting year
ecodist::MRM(community_matrix1 ~ Weekly_YR_PPT_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix1 ~ Monthly_YR_PPT_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix1 ~ Daily_JJ_PPT_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix1 ~ Weekly_JJ_PPT_Matrix + Seq_YR_Diff_Matrix)

# Temperature while accounting for difference in planting year
ecodist::MRM(community_matrix1 ~ Weekly_YR_AvgTemp_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix1 ~ Monthly_YR_AvgTemp_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix1 ~ Daily_JJ_AvgTemp_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix1 ~ Weekly_JJ_AvgTemp_Matrix + Seq_YR_Diff_Matrix)

## Community Age 2 Dissimilarity Models

# Set seed for permutation tests
set.seed(123)

# Precipitation individually
ecodist::MRM(community_matrix2 ~ Weekly_YR_PPT_Matrix)
ecodist::MRM(community_matrix2 ~ Monthly_YR_PPT_Matrix)
ecodist::MRM(community_matrix2 ~ Daily_JJ_PPT_Matrix)
ecodist::MRM(community_matrix2 ~ Weekly_JJ_PPT_Matrix)

# Temperature individually
ecodist::MRM(community_matrix2 ~ Weekly_YR_AvgTemp_Matrix)
ecodist::MRM(community_matrix2 ~ Monthly_YR_AvgTemp_Matrix)
ecodist::MRM(community_matrix2 ~ Daily_JJ_AvgTemp_Matrix)
ecodist::MRM(community_matrix2 ~ Weekly_JJ_AvgTemp_Matrix)

# Precipitation while accounting for difference in planting year
ecodist::MRM(community_matrix2 ~ Weekly_YR_PPT_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix2 ~ Monthly_YR_PPT_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix2 ~ Daily_JJ_PPT_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix2 ~ Weekly_JJ_PPT_Matrix + Seq_YR_Diff_Matrix)

# Temperature while accounting for difference in planting year
ecodist::MRM(community_matrix2 ~ Weekly_YR_AvgTemp_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix2 ~ Monthly_YR_AvgTemp_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix2 ~ Daily_JJ_AvgTemp_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix2 ~ Weekly_JJ_AvgTemp_Matrix + Seq_YR_Diff_Matrix)

## Community Age 3 Dissimilarity Models

# Set seed for permutation tests
set.seed(123)

# Precipitation individually
ecodist::MRM(community_matrix3 ~ Weekly_YR_PPT_Matrix)
ecodist::MRM(community_matrix3 ~ Monthly_YR_PPT_Matrix)
ecodist::MRM(community_matrix3 ~ Daily_JJ_PPT_Matrix)
ecodist::MRM(community_matrix3 ~ Weekly_JJ_PPT_Matrix)

# Temperature individually
ecodist::MRM(community_matrix3 ~ Weekly_YR_AvgTemp_Matrix)
ecodist::MRM(community_matrix3 ~ Monthly_YR_AvgTemp_Matrix)
ecodist::MRM(community_matrix3 ~ Daily_JJ_AvgTemp_Matrix)
ecodist::MRM(community_matrix3 ~ Weekly_JJ_AvgTemp_Matrix)

# Precipitation while accounting for difference in planting year
ecodist::MRM(community_matrix3 ~ Weekly_YR_PPT_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix3 ~ Monthly_YR_PPT_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix3 ~ Daily_JJ_PPT_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix3 ~ Weekly_JJ_PPT_Matrix + Seq_YR_Diff_Matrix)

# Temperature while accounting for difference in planting year
ecodist::MRM(community_matrix3 ~ Weekly_YR_AvgTemp_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix3 ~ Monthly_YR_AvgTemp_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix3 ~ Daily_JJ_AvgTemp_Matrix + Seq_YR_Diff_Matrix)
ecodist::MRM(community_matrix3 ~ Weekly_JJ_AvgTemp_Matrix + Seq_YR_Diff_Matrix)

## Combined model for community age 3

# Create a list of precipitation dissimilarity matrices
precipitation_matrices <- list(
  Weekly_YR_PPT_Matrix = Weekly_YR_PPT_Matrix,
  Monthly_YR_PPT_Matrix = Monthly_YR_PPT_Matrix,
  Daily_JJ_PPT_Matrix = Daily_JJ_PPT_Matrix,
  Weekly_JJ_PPT_Matrix = Weekly_JJ_PPT_Matrix
)
# Create a list of temperature dissimilarity matrices
temperature_matrices <- list(
  Weekly_YR_AvgTemp_Matrix = Weekly_YR_AvgTemp_Matrix,
  Monthly_YR_AvgTemp_Matrix = Monthly_YR_AvgTemp_Matrix,
  Daily_JJ_AvgTemp_Matrix = Daily_JJ_AvgTemp_Matrix,
  Weekly_JJ_AvgTemp_Matrix = Weekly_JJ_AvgTemp_Matrix
)

# Loop over every possible combination of precipitation and temperature matrices
for (precip_matrix_name in names(precipitation_matrices)) {
  for (temp_matrix_name in names(temperature_matrices)) {
    # Construct the model formula
    formula_text <- sprintf("community_matrix3 ~ precipitation_matrices[['%s']] + temperature_matrices[['%s']]", precip_matrix_name, temp_matrix_name)
    
    # Construct the full command to run MRM, including data assignment
    command_text <- sprintf("ecodist::MRM(%s)", formula_text)
    
    # Evaluate the command and print the summary of results directly
    model_result <- eval(parse(text = command_text))
    
    # Print the model results
    print(model_result)
    
    # Print headers between different model results
    cat("============================================================\n")
    cat(sprintf("Model results for %s + %s\n", precip_matrix_name, temp_matrix_name))
    cat("============================================================\n")
  }
}

# Multiple regression - best combined model accounting for difference in planting year
ecodist::MRM(community_matrix3 ~ Weekly_JJ_PPT_Matrix + Daily_JJ_AvgTemp_Matrix)

# Multiple regression - best combined model accounting for difference in planting year
ecodist::MRM(community_matrix3 ~ Weekly_JJ_PPT_Matrix + Daily_JJ_AvgTemp_Matrix + Seq_YR_Diff_Matrix)


### Dissimilarity Visualization

# Open a JPEG device and name plot file to be saved
jpeg('dissimilarity_plot.jpeg', width = 12*300, height = 18*300, res = 300)

# Set up plot to show 6 panels (3 rows and 2 columns)
par(mfrow = c(3, 2), mar = c(6, 6, 6, 2))

## Community Age 1

# Plot Dissimilarity of Community Age 1 by Planting Year Monthly_YR_PPT
plot(Monthly_YR_PPT_Matrix, community_matrix1, xlab = "Dissimilarity in Monthly_YR_PPT (Planting Year)", ylab = "Dissimilarity in Community Age 1", xlim = c(0,1), ylim = c(0,1), cex.lab = 1.6, cex.axis = 1.4)
fit_ppt1 <- lm(community_matrix1 ~ Monthly_YR_PPT_Matrix)
abline(fit_ppt1, col = "black")
mtext("A", side = 3, adj = -0.1, line = 1, cex = 2)

# Plot Dissimilarity of Community Age 1 by Planting Year Weekly_YR_AvgTemp
plot(Weekly_YR_AvgTemp_Matrix, community_matrix1, xlab = "Dissimilarity in Weekly_YR_AvgTemp (Planting Year)", ylab = "Dissimilarity in Community Age 1", xlim = c(0,1), ylim = c(0,1), cex.lab = 1.6, cex.axis = 1.4)
mtext("B", side = 3, adj = -0.1, line = 1, cex = 2)

## Community Age 2

# Plot Dissimilarity of Community Age 2 by Planting Year Daily_JJ_PPT
plot(Daily_JJ_PPT_Matrix, community_matrix2, xlab = "Dissimilarity in Daily_JJ_PPT (Planting Year)", ylab = "Dissimilarity in Community Age 2", xlim = c(0,1), ylim = c(0,1), cex.lab = 1.6, cex.axis = 1.4)
fit_ppt2 <- lm(community_matrix2 ~ Daily_JJ_PPT_Matrix)
abline(fit_ppt2, col = "black")
mtext("C", side = 3, adj = -0.1, line = 1, cex = 2)

# Plot Dissimilarity of Community Age 2 by Planting Year Weekly_YR_AvgTemp
plot(Weekly_YR_AvgTemp_Matrix, community_matrix2, xlab = "Dissimilarity in Weekly_YR_AvgTemp (Planting Year)", ylab = "Dissimilarity in Community Age 2", xlim = c(0,1), ylim = c(0,1), cex.lab = 1.6, cex.axis = 1.4)
mtext("D", side = 3, adj = -0.1, line = 1, cex = 2)

## Community Age 3

# Plot Dissimilarity of Community Age 3 by Planting Year Weekly_JJ_PPT
plot(Weekly_JJ_PPT_Matrix, community_matrix3, xlab = "Dissimilarity in Weekly_JJ_PPT (Planting Year)", ylab = "Dissimilarity in Community Age 3", xlim = c(0,1), ylim = c(0,1), cex.lab = 1.6, cex.axis = 1.4)
fit_ppt3 <- lm(community_matrix3 ~ Weekly_JJ_PPT_Matrix)
abline(fit_ppt3, col = "black")
mtext("E", side = 3, adj = -0.1, line = 1, cex = 2)

# Plot Dissimilarity of Community Age 3 by Planting Year Daily_JJ_AvgTemp
plot(Daily_JJ_AvgTemp_Matrix, community_matrix3, xlab = "Dissimilarity in Daily_JJ_AvgTemp (Planting Year)", ylab = "Dissimilarity in Community Age 3", xlim = c(0,1), ylim = c(0,1), cex.lab = 1.6, cex.axis = 1.4)
fit_temp3 <- lm(community_matrix3 ~ Daily_JJ_AvgTemp_Matrix)
abline(fit_temp3, col = "black")
mtext("F", side = 3, adj = -0.1, line = 1, cex = 2)

# Close the device to save the file
dev.off()
