# Load the required libraries
library(readxl)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(writexl)
library(MASS)

# Specify the file path
file_path <- "C:/Users/nicco/Downloads/CER_DEV2.xls"

# Read the Excel file
data <- read_excel(file_path)

# Specify the numerical predictors without quotes
numerical_predictors <- c("Sq", "Ssk", "Sku", "Sp", "Sv", "Sz", "Sa", "Smr...16", "Smc", "Sxp", "Sal...19", "Str...20", "Std...21", "Vmp...22", "Vvp", "Vmp...24", "Vmc", "Vvc", "Vvv", "Spd", "Spc", "S10", "S5p", "S5v", "Sda...33", "Sha", "Sdv", "Shv", "SkF...37", "Spk...38", "Svk", "Smr...40", "Smr...41", "St", "Str...43", "Std...44", "Sal...45", "Sds", "Ssc", "Sfd", "Smr...49", "Sdc", "SkF...51", "Spk...52", "Smq", "Sbi", "Sci", "Svi", "Sme", "Sda...58", "Spa", "Alt", "Are", "Pro...62", "Pro...63", "Den", "FLTvLivellamentometododellazonaminimaFiltrodiGauss0.08mm", "FLTqLivellamentometododellazonaminimaFiltrodiGauss0.08mm", "Smean", "Spar", "SWtFiltrodiGauss0.08mm", "Stdi", "Numerodimotivi", "AltezzaMedio", "AreaMedio", "Profonditamax", "Profonditamed", "Densitamed")

# Split the data into training and test based on the values in column "CAT"
training_data <- data[data$CAT == 1, ]
test_data <- data[data$CAT == 2, ]

# Subset the training data with the numerical predictors
selected_training_data <- training_data[, c("FACTOR", numerical_predictors)]

# Specify the file paths for the training and test files
training_file <- "C:\\Users\\nicco\\Downloads\\CERDDEV_training.xlsx"
test_file <- "C:\\Users\\nicco\\Downloads\\CERDEV_test.xlsx"

# Write the training data to a new Excel file
write.xlsx(training_data, file = training_file, rownames = FALSE)

# Write the test data to a new Excel file
write.xlsx(test_data, file = test_file, rownames = FALSE)

# Subset the training data with the numerical predictors
selected_training_data <- training_data[, c("FACTOR", numerical_predictors)]

# Remove rows with NULL values
selected_training_data_clean <- selected_training_data[complete.cases(selected_training_data), ]

# Remove rows with NULL values
selected_training_data_clean <- selected_training_data[complete.cases(selected_training_data), ]

# Print the numerical predictors
print(numerical_predictors)

# Subset the training data with the numerical predictors
selected_training_data_clean<- selected_training_data_clean[, c("FACTOR", numerical_predictors)]

# Remove rows with missing values
selected_training_data_clean <- na.omit(selected_training_data)

# Calculate the variances of each column
variances <- apply(selected_training_data_clean[, numerical_predictors], 2, var, na.rm = TRUE)

# Print the variances
print(variances)

# Filter numerical predictors based on non-zero variance
non_zero_var_predictors <- numerical_predictors[variances > 0]
variances <- apply(selected_training_data_clean[, numerical_predictors], 2, var)
zero_variance_vars <- numerical_predictors[variances == 0]
numerical_predictors <- setdiff(numerical_predictors, zero_variance_vars)
print(zero_variance_vars)
selected_training_data_clean <- selected_training_data_clean[, !(colnames(selected_training_data_clean) %in% zero_variance_vars)]
numerical_predictors <- setdiff(numerical_predictors, zero_variance_vars)
non_zero_var_predictors <- setdiff(non_zero_var_predictors, zero_variance_vars)

# Remove rows with missing values
non_zero_var_predictors <- na.omit(non_zero_var_predictors)
numerical_predictors <- na.omit(numerical_predictors)

# Calculate the correlation matrix
cor_matrix <- cor(selected_training_data_clean[, non_zero_var_predictors])

# Define the file path and name for the CSV file
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/1_cor_matrix.csv"

# Save the cross table as a CSV file
write.csv(cor_matrix, file = file_path, row.names = TRUE)

# Identify highly correlated variables
cor_threshold <- 0.8  # Adjust the threshold as needed
highly_correlated_vars <- character(0)  # Empty vector to store the highly correlated variables
variables_to_remove <- character(0)  # Empty vector to store the variables to remove

# Iterate through the correlation matrix to find highly correlated variables
for (i in 1:(ncol(cor_matrix) - 1)) {
  for (j in (i + 1):ncol(cor_matrix)) {
    if (abs(cor_matrix[i, j]) >= cor_threshold) {
      # Store the names of highly correlated variables in the vector
      highly_correlated_vars <- c(highly_correlated_vars, colnames(cor_matrix)[i], colnames(cor_matrix)[j])
      variables_to_remove <- c(variables_to_remove, colnames(cor_matrix)[j])
    }
  }
}

# Remove duplicates
highly_correlated_vars <- unique(highly_correlated_vars)
variables_to_remove <- unique(variables_to_remove)

# View the highly correlated variables
print(highly_correlated_vars)

# Define the file path and name for the CSV file
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/2_variables_to_remove.csv"

# Save the cross table as a CSV file
write.csv(variables_to_remove, file = file_path, row.names = TRUE)

# List variables that have been retained based on correlation strength
retained_variables <- setdiff(colnames(selected_training_data_clean), variables_to_remove)
print(retained_variables)

# Define the file path and name for the CSV file
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/3_retained_variables.csv"

# Save the cross table as a CSV file
write.csv(retained_variables, file = file_path, row.names = TRUE)

# Remove the highly correlated variables
selected_training_data_clean <- selected_training_data_clean[, !colnames(selected_training_data_clean) %in% variables_to_remove]

# Define the file path and name for the CSV file
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/4_selected_training_data_clean.csv"

# Save the cross table as a CSV file
write.csv(selected_training_data_clean, file = file_path, row.names = TRUE)

#--------------------------------------------------CANONICAL DISCRIMINANT ANALYSIS

# Perform Canonical Discriminant Analysis
cda_model <- lda(FACTOR ~ ., data = selected_training_data_clean)

# Extract the discriminant scores for each observation
scores <- predict(cda_model)$x

# Create a data frame with scores and class labels
df <- data.frame(scores, Class = as.factor(selected_training_data_clean$FACTOR))

# Check for missing values in scores
if (any(is.na(df$LDA1)) || any(is.na(df$LDA2))) {
  stop("Missing values detected in LD1 or LD2.")
}

# Check factor levels
if (!is.factor(df$Class)) {
  stop("Class variable (FACTOR) should be a factor.")
}

# Remove missing values
df <- na.omit(df)

# Calculate centroids for each class
centroids <- aggregate(. ~ Class, data = df, FUN = mean)



#--------------------------------------------------STRUCTURE MATRIX

# Exclude FACTOR and Prediction variables
variables <- selected_training_data_clean[, !(names(selected_training_data_clean) %in% c("FACTOR", "Prediction"))]

# Convert variables to numeric format
variables <- as.data.frame(sapply(variables, as.numeric))

# Get the canonical function matrix
canonical_functions <- cda_model$scaling

# Transpose the canonical_functions matrix
canonical_functions_transposed <- t(canonical_functions)

# Exclude FACTOR and Prediction variables
variables <- selected_training_data_clean[, !(names(selected_training_data_clean) %in% c("FACTOR", "Prediction"))]

# Convert variables to numeric format
variables <- as.data.frame(sapply(variables, as.numeric))

# Transpose the canonical_functions matrix
canonical_functions_transposed <- t(canonical_functions)

# Get the coefficients for each variable in the canonical discriminant functions
coefficients <- canonical_functions_transposed

# Transpose the coefficients matrix for printing
coefficients_transposed <- t(coefficients)

# Print the ordered variables with their contributions and coefficients
print(coefficients_transposed)

# Define the file path and name for the CSV file
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/5_variables_contributions.csv"

# Save the cross table as a CSV file
write.csv(coefficients_transposed, file = file_path, row.names = TRUE)

# Extract the coefficients for LD1 and LD2
coeff_LD1 <- coefficients["LD1", ]
coeff_LD2 <- coefficients["LD2", ]

# Sort the absolute values of the coefficients in descending order
sorted_LD1 <- sort(abs(coeff_LD1), decreasing = TRUE)
sorted_LD2 <- sort(abs(coeff_LD2), decreasing = TRUE)

# Extract the top 5 variables for LD1 and LD2
top_variables_LD1 <- names(sorted_LD1)[1:5]
top_variables_LD2 <- names(sorted_LD2)[1:5]

# Print the top variables for LD1 and LD2
cat("Top variables for LD1:\n")
print(top_variables_LD1)
cat("Top variables for LD2:\n")
print(top_variables_LD2)

# Select the unique set of variables that contribute the most to LD1 and LD2
selected_variables <- unique(c("FACTOR", top_variables_LD1, top_variables_LD2))
print(selected_variables)

# Create a new dataset including FACTOR and the selected variables
new_training_dataset <- selected_training_data_clean[, selected_variables]

# Define the file path and name for the CSV file
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/6_new_training_dataset.csv"

# Save the cross table as a CSV file
write.csv(new_training_dataset, file = file_path, row.names = TRUE)


#--------------------------------------------------CROSS TABLE WITH PREDICTION RESULTS 

# Perform prediction on the selected_training_data_clean
selected_training_data_clean$Prediction <- predict(cda_model, newdata = selected_training_data_clean)$class

# Create a cross table
cross_table <- table(selected_training_data_clean$FACTOR, selected_training_data_clean$Prediction)

# Compute row percentages
row_percentages <- prop.table(cross_table, margin = 1) * 100

# Add row and column margins
cross_table_with_margins <- addmargins(cross_table, margin = 1:2)
row_percentages_with_margins <- addmargins(row_percentages, margin = 1)

# Combine counts and percentages
cross_table_combined <- cbind(cross_table_with_margins, row_percentages_with_margins)

# Rename the row and column names
rownames(cross_table_combined)[1:5] <- c("NOUSE", "LOW", "MEDIUM", "HIGH", "VERY HIGH")
colnames(cross_table_combined)[1:5] <- c("NOUSE", "LOW", "MEDIUM", "HIGH", "VERY HIGH")
colnames(cross_table_combined)[6] <- "Sum"
colnames(cross_table_combined)[7:11] <- c("NOUSE%", "LOW%", "MEDIUM%", "HIGH%", "VERY HIGH%")

# Print the modified cross table
print(cross_table_combined)

# Get the counts of correct classifications for each class
correct_counts <- diag(cross_table_combined[1:3, 1:3])

# Calculate the total number of observations
total_observations <- sum(cross_table_combined[1:3, 1:3])

# Calculate the global percentage of cases correctly classified
global_percentage <- sum(correct_counts) / total_observations * 100

# Print the global percentage
cat("Global Percentage of Cases Correctly Classified:", global_percentage, "%\n")

# Define the file path and name for the Excel file
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/7_results_training_all_var.csv"

# Save the cross table as a CSV file
write.csv(cross_table_combined, file = file_path, row.names = TRUE)


#--------------------------------------------------SCATTER PLOT

# Define the Darjeeling palette
darjeeling_palette <- c("#774EA3", "#0074E9", "#FF3900", "#002876", "#099E76")

# Recode the Class variable using CODE_NAME
df <- df %>%
  mutate(Class = recode(Class,
                        "1" = "NOUSE",
                        "2" = "LOW",
                        "3" = "MEDIUM",
                        "4" = "HIGH",
                        "5" = "VERY HIGH"))

# Create the scatter plot
scatter_plot_allvar <- ggplot(df, aes(x = LD1, y = LD2, color = Class)) +
  geom_point(shape = 21, size = 1.8, stroke = 0.5) +
  geom_point(data = centroids, aes(x = LD1, y = LD2), color = "#000000", size = 4, shape = 22, stroke = 0.5, fill = "#FFFF66") +
  labs(x = "LD1", y = "LD2", title = "All var") +
  scale_color_manual(name = "Class", values = darjeeling_palette) +
  theme(panel.background = element_rect(fill = "#FFFFFF")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#999999", linetype = "dotted", size = 0.1),
        panel.grid.minor = element_line(color = "#999999", linetype = "dotted", size = 0.1))

# Define the file path and name for the JPG image
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/8_scatter_plot_all_var.jpg"

# Save the scatter plot as a JPG image
ggsave(file = file_path, plot = scatter_plot_allvar, width = 10, height = 6, dpi = 600)

# Print the scatter plot
print(scatter_plot_allvar)


#--------------------------------------------------CANONICAL DISCRIMINANT ANALYSIS WITH TOP VAR

# Perform Canonical Discriminant Analysis
cda_model_topvar <- lda(FACTOR ~ ., data = new_training_dataset)

# Extract the discriminant scores for each observation
scores_topvar <- predict(cda_model_topvar)$x

# Create a data frame with scores and class labels
df_topvar <- data.frame(scores_topvar, Class = as.factor(new_training_dataset$FACTOR))

# Check for missing values in scores
if (any(is.na(df_topvar$LDA1)) || any(is.na(df_topvar$LDA2))) {
  stop("Missing values detected in LD1 or LD2.")
}

# Check factor levels
if (!is.factor(df_topvar$Class)) {
  stop("Class variable (FACTOR) should be a factor.")
}

# Remove missing values
df_topvar <- na.omit(df_topvar)

# Calculate centroids for each class
centroids <- aggregate(. ~ Class, data = df_topvar, FUN = mean)


#--------------------------------------------------SCATTER PLOT WITH TOP VAR

# Define the Darjeeling palette
darjeeling_palette <- c("#774EA3", "#0074E9", "#FF3900", "#002876", "#099E76")

# Recode the Class variable using CODE_NAME
df_topvar <- df_topvar %>%
mutate(Class = recode(Class,
                        "1" = "NOUSE",
                        "2" = "LOW",
                        "3" = "MEDIUM",
                        "4" = "HIGH",
                        "5" = "VERY HIGH"))

# Create the scatter plot
scatter_plot_topvar <- ggplot(df_topvar, aes(x = LD1, y = LD2, color = Class)) +
  geom_point(shape = 21, size = 1.8, stroke = 0.5) +
  geom_point(data = centroids, aes(x = LD1, y = LD2), color = "#000000", size = 4, shape = 22, stroke = 0.5, fill = "#FFFF66") +
  labs(x = "LD1", y = "LD2", title = "Top var") +
  scale_color_manual(name = "Class", values = darjeeling_palette) +
  theme(panel.background = element_rect(fill = "#FFFFFF")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#999999", linetype = "dotted", size = 0.1),
        panel.grid.minor = element_line(color = "#999999", linetype = "dotted", size = 0.1))


# Define the file path and name for the JPG image
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/9_scatter_plot_topvar.jpg"

# Save the scatter plot as a JPG image
ggsave(file = file_path, plot = scatter_plot_topvar, width = 10, height = 6, dpi = 600)
print(scatter_plot_topvar)

#--------------------------------------------------CROSS TABLE WITH PREDICTION RESULTS WITH TOP VAR 

# Perform prediction on the new_training_dataset
new_training_dataset$Prediction <- predict(cda_model_topvar, newdata = new_training_dataset)$class

# Create a cross table
cross_table_topvar <- table(new_training_dataset$FACTOR, new_training_dataset$Prediction, dnn = c("FACTOR", "Prediction"))

# Compute row percentages
row_percentages_topvar <- prop.table(cross_table_topvar, margin = 1) * 100

# Add row and column margins
cross_table_with_margins_topvar <- addmargins(cross_table_topvar, margin = 1:2)
row_percentages_with_margins_topvar <- addmargins(row_percentages_topvar, margin = 1)

# Combine counts and percentages
cross_table_combined_topvar <- cbind(cross_table_with_margins_topvar, row_percentages_with_margins_topvar)

# Rename the row and column names
rownames(cross_table_combined_topvar)[1:5] <- c("NOUSE", "LOW", "MEDIUM", "HIGH", "VERY HIGH")
colnames(cross_table_combined_topvar)[1:5] <- c("NOUSE", "LOW", "MEDIUM", "HIGH", "VERY HIGH")
colnames(cross_table_combined_topvar)[6] <- "Sum"
colnames(cross_table_combined_topvar)[7:11] <- c("NOUSE%", "LOW%", "MEDIUM%", "HIGH%", "VERY HIGH%")

# Print the modified cross table
print(cross_table_combined_topvar)

# Get the counts of correct classifications for each class
correct_counts_topvar <- diag(cross_table_combined_topvar[1:3, 1:3])

# Calculate the total number of observations
total_observations_topvar <- sum(cross_table_combined_topvar[1:3, 1:3])

# Calculate the global percentage of cases correctly classified
global_percentage_topvar <- sum(correct_counts) / total_observations * 100

# Print the global percentage
cat("Global Percentage of Cases Correctly Classified:", global_percentage, "%\n")

# Define the file path and name for the Excel file
file_path <- "C:/Users/nicco//R/TRAC3D/10_results_training_top_var.csv"

# Save the cross table as a CSV file
write.csv(cross_table_combined_topvar, file = file_path, row.names = TRUE)



#--------------------------------------------------APPLY AT THE TEST_DATA

# Create a subset of the test data with the selected variables
selected_test_data <- test_data[, c("FACTOR", "WORKING_TIME", selected_variables)]

# Generate unique names for the variables
unique_names <- make.unique(colnames(selected_test_data))

# Rename the variables in the selected test data
colnames(selected_test_data) <- unique_names

# Perform blind classification using the trained model
test_predictions <- predict(cda_model_topvar, newdata = selected_test_data)

# Extract the discriminant scores for each observation
scores_blind <- as.data.frame(test_predictions$x)

# Add the predicted class labels to the selected test data
selected_test_data$Predicted_Class <- test_predictions$class

# Create a data frame with scores and class labels
df_blind <- data.frame(scores_blind, Class = as.factor(selected_test_data$FACTOR))

# Define the Darjeeling palette
darjeeling_palette <- c("#774EA3", "#0074E9", "#FF3900", "#002876", "#099E76")

# Recode the Class variable using CODE_NAME
df <- df %>%
  mutate(Class = recode(Class,
                        "1" = "NOUSE",
                        "2" = "LOW",
                        "3" = "MEDIUM",
                        "4" = "HIGH",
                        "5" = "VERY HIGH"))

# Plotting the known and unknown individuals together
scatter_plot_test <- ggplot() +
  geom_point(data = df_topvar, aes(x = LD1, y = LD2, color = Class), shape = 21, size = 1.8, stroke = 0.5) +
  geom_point(data = df_blind, aes(x = LD1, y = LD2), shape = 5, size = 0.7, stroke = 0.2, color = "black") +
  labs(x = "LD1", y = "LD2", title = "Blind Classification") +
  scale_color_manual(name = "Class", values = darjeeling_palette) +
  theme(panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid.major = element_line(color = "#999999", linetype = "dotted", size = 0.4),
        panel.grid.minor = element_line(color = "#999999", linetype = "dotted", size = 0.4)) +
  theme_minimal()

# Define the file path and name for the JPG image
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/11_scatter_plot_test.jpg"

# Print and save the scatter plot as a JPG image
print(scatter_plot_test)
ggsave(file = file_path, plot = scatter_plot_test, width = 10, height = 6, dpi = 600)

# Create a data frame with the WoRKING_TIME column and predicted class labels
predictions <- data.frame(WORKING_TIME = test_data$WORKING_TIME, Predicted_Class = test_predictions$class)

# Print the predictions
print(predictions)

# Create a cross-tabulation of Predicted_Class and WORKING_TIME
cross_tab_pred <- table(predictions$Predicted_Class, predictions$WORKING_TIME)

# Calculate the percentage of individuals in each class for each WORKING_TIME
percentage <- prop.table(cross_tab_pred, margin = 2) * 100

# Combine counts and percentages
cross_tab_pred_combined <- cbind(cross_tab_pred, percentage)

# Rename the row and column names to match the previous table
rownames(cross_tab_pred_combined)[1:5] <- c("NOUSE", "LOW", "MEDIUM", "HIGH", "VERY HIGH")

# Print the percentage table
print(cross_tab_pred_combined)

# Define the file path and name for the Excel file
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/12_results.csv"

# Save the cross table as a CSV file
write.csv(cross_tab_pred_combined, file = file_path, row.names = TRUE)

# Calculate the count of correctly classified individuals
count_nouse <- cross_tab_pred_combined["NOUSE", "0"]
count_low <- sum(cross_tab_pred_combined["LOW", c("4", "8")])
count_medium <- sum(cross_tab_pred_combined["MEDIUM", c("12", "16")])
count_high <- sum(cross_tab_pred_combined["HIGH", c("20", "24", "28")])
count_very_high <- sum(cross_tab_pred_combined["VERY HIGH", c("32", "36")])

# Calculate the total number of individuals for each category
total_nouse <- sum(cross_tab_pred_combined["NOUSE", c("0", "4", "8", "12", "16", "20", "24", "28", "32", "36")])
total_low <- sum(cross_tab_pred_combined["LOW", c("0", "4", "8", "12", "16", "20", "24", "28", "32", "36")])
total_medium <- sum(cross_tab_pred_combined["MEDIUM", c("0", "4", "8", "12", "16", "20", "24", "28", "32", "36")])
total_high <- sum(cross_tab_pred_combined["HIGH", c("0", "4", "8", "12", "16", "20", "24", "28", "32", "36")])
total_very_high <- sum(cross_tab_pred_combined["VERY HIGH", c("0", "4", "8", "12", "16", "20", "24", "28", "32", "36")])

# Calculate the percentage of correctly classified individuals for each category
percentage_nouse <- count_nouse / total_nouse * 100
percentage_low <- count_low / total_low * 100
percentage_medium <- count_medium / total_medium * 100
percentage_high <- count_high / total_high * 100
percentage_very_high <- count_very_high / total_very_high * 100

# Create a data frame with the percentages
correct_class <- data.frame(Category = c("NOUSE", "LOW", "MEDIUM", "HIGH", "VERY HIGH"),
                               Percentage = c(percentage_nouse, percentage_low, percentage_medium, percentage_high, percentage_very_high))

# Print the cross table
print(correct_class)

# Define the file path and name for the Excel file
file_path <- "C:/Users/nicco/R/TRAC3D/CERDEV/13_correct_classification.csv"

# Save the cross table as a CSV file
write.csv(correct_class, file = file_path, row.names = TRUE)

