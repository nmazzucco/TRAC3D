# Load required libraries
library(readxl)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(writexl)
library(caret)
library(randomForest)
library(tidyr)
library(MASS)
library(factoextra)
library(classInt)
library(changepoint)
library(minpack.lm)
library(ggrepel)

#------------------------------#
# 1 - File I/O helpers
#------------------------------#
read_my_data <- function(path, sep = ";") {
  read.csv(path, sep = sep)
}

write_my_data <- function(data, path) {
  write.csv(data, file = path, row.names = FALSE)
}

save_plot <- function(p, fname, width = 8, height = 5, dpi = 600) {
  print(p)
  ggsave(
    filename = file.path(out_dir, fname),
    plot     = p,
    width    = width,
    height   = height,
    dpi      = dpi,
    bg       = "white"
  )
  cat("Saved plot:", file.path(out_dir, fname), "\n")
}

#------------------------------#
# 2 - Project paths
#------------------------------#
project_dir <- getwd()
data_dir    <- file.path(project_dir, "data")
out_dir     <- file.path(project_dir, "output")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

training_input_file <- file.path(data_dir, "Supplementary Material_S1.xlsx")
test_input_file     <- file.path(data_dir, "Supplementary Material_S2.xlsx")

cat("Working directory:", project_dir, "\n")
cat("Training input file:", training_input_file, "\n")
cat("Test input file:", test_input_file, "\n")
cat("Output directory:", out_dir, "\n")

#------------------------------#
# 3 - Read input files
#------------------------------#
training_data <- read_excel(training_input_file)
test_data     <- read_excel(test_input_file)

training_data <- as.data.frame(training_data)
test_data     <- as.data.frame(test_data)

cat("Training rows:", nrow(training_data), "\n")
cat("Test rows:", nrow(test_data), "\n")

#------------------------------#
# 4 - Preliminary checks
#------------------------------#

# Store ID and working time from training data if needed later
id_factor_df <- training_data[, c("ID", "WORKING_TIME")]

# Remove constant columns from training data
constant_columns <- sapply(training_data, function(col) length(unique(col[!is.na(col)])) == 1)
training_data <- training_data[, !constant_columns, drop = FALSE]

#------------------------------#
# 5 - Clean the training_set
#------------------------------#

# Check and remove columns that contain exactly the same values
duplicated_cols <- duplicated(apply(training_data[, -1, drop = FALSE], 2, paste, collapse = ","))
training_data <- training_data[, !duplicated_cols]

# Extract the column names from your data frame
column_names <- colnames(training_data)

# Format the column names with double quotation marks
formatted_column_names <- sprintf("\"%s\"", column_names)

# Print the formatted column names as a list
cat(paste(formatted_column_names, collapse = ", "))
print(head(column_names))

# Create a subset of numerical predictors
numerical_predictors <- c("Sq", "Ssk", "Sku", "Sp", "Sv", "Sz", "Sa", "Smc", "Sxp", "Sal", "Str", "Std", 
                          "Sdq", "Sdr", "Vm", "Vv", "Vmc", "Vvc", "Vvv", "Spd", "Spc", "S10z", "S5p", "S5v",
                          "Sda", "Sha", "Sdv", "Shv", "Sk", "Spk", "Svk", "Smr1", "Smr2", "Sds", "Ssc",
                          "Sfd", "Sdc", "Sbi", "Sci", "Svi", "Smean", "Sdar", "SWt", "Stdi", "Maximumdepth",
                          "Meandepth", "Meandensity")

# Subset the training data with the numerical predictors
selected_training_data <- training_data[, c("WORKING_TIME", numerical_predictors)]

# Remove rows with NULL values
selected_training_data_clean <- selected_training_data[complete.cases(selected_training_data), ]

# Identify and remove equal_cols
equal_cols <- which(apply(selected_training_data_clean, 2, function(x) length(unique(x)) == 1))
selected_training_data_clean <- selected_training_data_clean[, -equal_cols]
print(head(selected_training_data_clean))

# Count NAs per column
na_pos <- which(is.na(selected_training_data), arr.ind = TRUE)
na_report <- data.frame(
  row = na_pos[, "row"],
  col = colnames(selected_training_data)[na_pos[, "col"]],
  WORKING_TIME = selected_training_data$WORKING_TIME[na_pos[, "row"]]
)
na_report

# Remove rows with missing values
selected_training_data_clean <- na.omit(selected_training_data)

# Calculate the variances and filter numerical predictors based on non-zero variance
available_predictors <- intersect(numerical_predictors, colnames(selected_training_data_clean))
variances <- apply(selected_training_data_clean[, available_predictors, drop = FALSE], 2, var, na.rm = TRUE)
non_zero_var_predictors <- available_predictors[variances > 0]
selected_training_data_clean <- selected_training_data_clean[, c("WORKING_TIME", non_zero_var_predictors), drop = FALSE]

#-----------------------------------------#
# 6 - Eliminate the correlated predictors
#-----------------------------------------#

# Store WORKING_TIME before removing it
working_time_clean <- selected_training_data_clean$WORKING_TIME

# Remove the "WORKING_TIME" column from selected_training_data_clean
selected_training_data_clean <- selected_training_data_clean[, -which(names(selected_training_data_clean) == "WORKING_TIME"), drop = FALSE]

# Store the updated list of predictors from selected_training_data_clean
updated_predictors <- colnames(selected_training_data_clean)

# Check for constant variables and exclude them from the correlation calculation
non_constant_vars <- apply(selected_training_data_clean[, updated_predictors], 2, function(x) var(x, na.rm = TRUE) > 0)

# Select only non-constant variables
selected_training_data_clean <- selected_training_data_clean[, updated_predictors[non_constant_vars]]

# Calculate the correlation matrix
cor_matrix_updated_predictors <- cor(selected_training_data_clean)

# Identify correlations greater than 0.8
high_correlations <- which(cor_matrix_updated_predictors > 0.8 & cor_matrix_updated_predictors < 1, arr.ind = TRUE)

# Print the pairs of highly correlated predictors
if (length(high_correlations) > 0) {
  for (i in 1:nrow(high_correlations)) {
    row_idx <- high_correlations[i, 1]
    col_idx <- high_correlations[i, 2]
    predictor1 <- rownames(cor_matrix_updated_predictors)[row_idx]
    predictor2 <- colnames(cor_matrix_updated_predictors)[col_idx]
    correlation <- cor_matrix_updated_predictors[row_idx, col_idx]
    cat("Predictor", predictor1, "and", predictor2, "are highly correlated with a correlation of", correlation, "\n")
  }
} else {
  cat("No predictors are highly correlated (correlation > 0.8).\n")
}

# Function to remove correlated predictors
remove_correlated_predictors <- function(data, cor_matrix, threshold = 0.8) {
  correlated_features <- which(cor_matrix > threshold & cor_matrix < 1, arr.ind = TRUE)
  
  removed_predictors <- character()
  for (i in 1:nrow(correlated_features)) {
    feature1 <- rownames(cor_matrix)[correlated_features[i, 1]]
    feature2 <- colnames(cor_matrix)[correlated_features[i, 2]]
    
    if (!(feature1 %in% removed_predictors) && !(feature2 %in% removed_predictors) && feature1 != feature2) {
      correlation <- cor_matrix[correlated_features[i, 1], correlated_features[i, 2]]
      cat(sprintf("Predictor %s and %s are highly correlated with a correlation of %.7f\n", feature1, feature2, correlation))
      
      pvalue1 <- anova(lm(data[[feature1]] ~ data[[feature2]]))$`Pr(>F)`[1]
      pvalue2 <- anova(lm(data[[feature2]] ~ data[[feature1]]))$`Pr(>F)`[1]
      
      if (pvalue1 < pvalue2) {
        data <- data[, !colnames(data) %in% feature1]
        removed_predictors <- c(removed_predictors, feature1)
      } else {
        data <- data[, !colnames(data) %in% feature2]
        removed_predictors <- c(removed_predictors, feature2)
      }
    }
  }
  
  return(list(data = data, removed_predictors = removed_predictors))
}

result <- remove_correlated_predictors(selected_training_data_clean, cor_matrix_updated_predictors)
selected_training_data_clean <- result$data
removed_predictors <- result$removed_predictors
print(removed_predictors)

# Store selected predictors
selected_variables <- setdiff(colnames(selected_training_data_clean), "WORKING_TIME")
print(selected_variables)

# Save .csv
file_path_S2 <- file.path(out_dir, "1_selected_predictors.csv")
file_path_S3 <- file.path(out_dir, "2_selected_training_data_clean.csv")

write.csv(selected_variables, file = file_path_S2, row.names = FALSE)
write.csv(selected_training_data_clean, file = file_path_S3, row.names = TRUE)

cat("Saved predictor list to:", file_path_S2, "\n")
cat("Saved training data to:", file_path_S3, "\n")

# Check that WORKING_TIME is not present in the dataset
print(head(selected_training_data_clean))

# Add the WORKING_TIME column back to the dataset
selected_training_data_clean$WORKING_TIME <- working_time_clean


#-------------------------------------------#
# 7 - Check dataset integroty after cleaning
#-------------------------------------------#

# Add the "ID" column back to selected_training_data_clean based on row numbers
selected_training_data_clean$ID <- training_data$ID[1:nrow(selected_training_data_clean)]

# Get the unique "ID" values from selected_training_data_clean
unique_ids <- unique(selected_training_data_clean$ID)

# Initialize a vector to store the IDs with inconsistent values
inconsistent_ids <- character(0)

# Iterate through each unique ID
for (id in unique_ids) {
  # Subset rows with the same ID from both data frames
  subset_selected <- selected_training_data_clean[selected_training_data_clean$ID == id, selected_variables]
  subset_training <- training_data[training_data$ID == id, selected_variables]
  
  # Check if the subsets have different values
  if (!identical(subset_selected, subset_training)) {
    inconsistent_ids <- c(inconsistent_ids, id)
  }
}

# Print or inspect the IDs with inconsistent values
if (length(inconsistent_ids) > 0) {
  cat("IDs with inconsistent values for selected_variables:", inconsistent_ids, "\n")
} else {
  cat("All rows with the same ID have consistent values for selected_variables.\n")
}

# Remove the "ID" column from selected_training_data_clean
selected_training_data_clean <- selected_training_data_clean[, -which(names(selected_training_data_clean) == "ID")]
print(head(selected_training_data_clean))

# Store a copy for future analysis
selected_training_data_clean2 <- selected_training_data_clean


#------------------------------------------------------------------------------#
# 8 - Classify working_time using regression-based segmentation
#------------------------------------------------------------------------------#

# Create a temporary dataframe for rows with WORKING_TIME equal to 0
temp_df <- selected_training_data_clean[selected_training_data_clean$WORKING_TIME == 0, ]

# Add the new column to the temporary dataframe if it's not already present
if (!"FACTOR" %in% colnames(temp_df)) {
  temp_df$FACTOR <- 1  #
}

# Exclude rows with working time "0" from selected_training_data_clean
new_training_data <- selected_training_data_clean[selected_training_data_clean$WORKING_TIME != 0, ]
print(head(new_training_data))
str(new_training_data)

library(rpart)
library(rpart.plot)

# Regression tree with WORKING_TIME as dependent variable
tree_model <- rpart(WORKING_TIME ~ ., data = new_training_data, method = "anova")

# Plot and save tree as .jpeg
jpeg(
  filename = file.path(out_dir, "Figure_1.jpeg"),
  width = 2000,
  height = 1600,
  res = 300
)
rpart.plot(tree_model)
dev.off()

# Show again in R
rpart.plot(tree_model)

cat("Tree plot saved to:", file.path(out_dir, "Figure_1.jpeg"), "\n")

# Predict WORKING_TIME using the regression tree to define progressive groups
new_training_data$FACTOR <- predict(tree_model, new_training_data, type = "vector")

# Define progressive groups based on predicted WORKING_TIME from the tree
breaks <- quantile(new_training_data$WORKING_TIME, probs = seq(0, 1, length.out = 6))
new_training_data$FACTOR <- cut(new_training_data$WORKING_TIME, breaks = breaks, labels = c("2", "3", "4", "5", "6"), include.lowest = TRUE)

# Check your progressive classes
print(table(new_training_data$FACTOR, new_training_data$WORKING_TIME))

# Convert to factor
new_training_data$FACTOR <- factor(new_training_data$FACTOR)

# Split the data by the FACTOR and list the WORKING_TIME values in each group
split_data <- split(new_training_data$WORKING_TIME, new_training_data$FACTOR)
print(split_data)

# Store the WORKING_TIME column before removing it
stored_working_time <- new_training_data$WORKING_TIME

# Store the WORKING_TIME column before removing it
stored_FACTOR <- new_training_data$FACTOR

# Remove the "WORKING_TIME" column from selected_training_data_clean
new_training_data <- new_training_data[, -which(names(new_training_data) == "WORKING_TIME")]
print(head(new_training_data))
str(new_training_data)


#------------------------------------------------------------------------------#
# 9 - Build composite wear metric and detect change points
#------------------------------------------------------------------------------#

df_time_agg <- selected_training_data_clean %>%
  filter(WORKING_TIME != 0) %>%               # (Optional) exclude WORKING_TIME == 0
  group_by(WORKING_TIME) %>%
  summarize(
    # For each variable, choose a summary function (mean here)
    Sku        = mean(Sku, na.rm = TRUE),
    Sal        = mean(Sal, na.rm = TRUE),
    Str        = mean(Str, na.rm = TRUE),
    Std        = mean(Std, na.rm = TRUE),
    Spd        = mean(Spd, na.rm = TRUE),
    Sda        = mean(Sda, na.rm = TRUE),
    Smr1       = mean(Smr1, na.rm = TRUE),
    Sds        = mean(Sds, na.rm = TRUE),
    Sbi        = mean(Sbi, na.rm = TRUE),
    Svi        = mean(Svi, na.rm = TRUE),
    Smean      = mean(Smean, na.rm = TRUE),
    SWt        = mean(SWt, na.rm = TRUE),
    Stdi       = mean(Stdi, na.rm = TRUE),
    Maximumdepth  = mean(Maximumdepth, na.rm = TRUE),
    Meandensity= mean(Meandensity, na.rm = TRUE)
  ) %>%
  arrange(WORKING_TIME)   # Ensure rows are in ascending time order

str(df_time_agg)
print(head(df_time_agg))

# Select columns you believe are indicative of wear
wear_vars <- df_time_agg %>% 
  dplyr::select(Sku, Sal, Str, Std, Spd, Sda, Smr1, Sds, Sbi, Svi, Smean, SWt, Stdi, Maximumdepth)

# Run PCA
pca_result <- prcomp(wear_vars, scale. = TRUE)

# Barplot of loadings on PC1
pc1_loadings <- data.frame(
  variable    = rownames(pca_result$rotation),
  loading     = pca_result$rotation[, 1]
) %>%
  mutate(abs_loading = abs(loading)) %>%
  arrange(desc(abs_loading))

# Plot
p_pca_pc1 <- ggplot(pc1_loadings,
                    aes(x = reorder(variable, abs_loading),
                        y = loading)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  xlab("Surface parameter") +
  ylab("PC1 loading") +
  ggtitle("Loadings of surface parameters on PC1 (composite wear metric)")

# Save figure
pca_fig_path <- file.path(out_dir, "Figure_2.png")
ggsave(pca_fig_path, p_pca_pc1, width = 7, height = 5, dpi = 300)
cat("PCA loading plot saved to:", pca_fig_path, "\n")

# Assume pca_result$rotation[, 1] gives the loadings for the first principal component
loadings <- pca_result$rotation[, 1]

# Print the selected variables
print(selected_variables)

print(head(df_time_agg))
str(df_time_agg)

print(head(selected_training_data_clean))
str(selected_training_data_clean)

# Select columns you believe are indicative of wear
wear_vars <- df_time_agg %>% 
  dplyr::select(Maximumdepth, Smr1, Sku, Sda, Svi, Std, Sbi)

# Run PCA
pca_result <- prcomp(wear_vars, scale. = TRUE)

# Extract the first principal component
wear_metric <- pca_result$x[, 1]

library(changepoint)
cpt_mean <- cpt.mean(wear_metric, method = "PELT")

plot(cpt_mean, xaxt = "n", main = "Change Point Detection for Composite Wear Metric",
     xlab = "Working Time (hours)", ylab = "First Principal Component")

# Add a custom axis: 'at' = numeric positions, 'labels' = actual time values
axis(1, at = seq_along(df_time_agg$WORKING_TIME),
     labels = df_time_agg$WORKING_TIME)
breakpoints <- cpts(cpt_mean)
print(breakpoints)

# Converti gli indici dei changepoint in ore di lavoro
cp_times <- df_time_agg$WORKING_TIME[breakpoints]

cat("\n--- Changepoint (PELT) ---\n")
cp_table <- data.frame(
  method      = "PELT_mean",
  index       = breakpoints,
  WORKING_TIME = cp_times,
  wear_metric = wear_metric[breakpoints]
)
print(cp_table)

library(segmented)
# Suppose df_time_agg has columns WORKING_TIME and wear_metric
fit_lm <- lm(wear_metric ~ WORKING_TIME, data = df_time_agg)

# Provide an initial guess (psi) for breakpoints (e.g., near 10, 14 if you suspect them)
fit_seg <- segmented(fit_lm, seg.Z = ~ WORKING_TIME, psi = c(10, 14))
summary(fit_seg)

# prerequisites: df_time_agg, wear_metric, fit_seg, cpt_mean, and out_dir exist
stopifnot(exists("out_dir"))
fig_path <- file.path(out_dir, "Figure_3.png")

# Estrai i breakpoint stimati dal modello segmented
seg_psi <- as.data.frame(fit_seg$psi)
seg_psi$break_name <- rownames(seg_psi)

cat("\n--- Breakpoint da segmented ---\n")
seg_bp_table <- data.frame(
  method       = "segmented_lm",
  break_name   = seg_psi$break_name,
  WORKING_TIME = seg_psi$Est.,
  SE           = seg_psi$St.Err
)
print(seg_bp_table)

# (Opzionale) salva su CSV
seg_csv_path <- file.path(out_dir, "3_breakpoints_segmented_regression.csv")
write.csv(seg_bp_table, seg_csv_path, row.names = FALSE)
cat("Breakpoint segmented salvati in:", seg_csv_path, "\n")

# open device
png(fig_path, width = 1600, height = 900, res = 150, bg = "white")

# lay out panels: 1 row, 3 columns
par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1), mgp = c(2.2, 0.8, 0))

## (1) segmented package diagnostic plot
plot(fit_seg, conf.level = 0.95, main = "Segmented regression (diagnostic)")

## (2) wear vs time with segmented fit
plot(df_time_agg$WORKING_TIME, wear_metric, type = "b", pch = 19,
     xlab = "Working Time (hours)", ylab = "Wear Metric (PC1)",
     main = "Wear vs Time with segmented fit")
lines(df_time_agg$WORKING_TIME, fitted(fit_seg), col = "red", lwd = 2)
legend("topleft", inset = 0.02,
       legend = c("Raw Data", "Segmented Fit"),
       col    = c("black", "red"), pch = c(19, NA), lty = c(0, 1), lwd = c(1, 2))

## (3) changepoint plot with custom x-axis
plot(cpt_mean, xaxt = "n",
     main = "Changepoint on wear metric",
     xlab = "Working Time (hours)", ylab = "PC1")
axis(1, at = seq_along(df_time_agg$WORKING_TIME),
     labels = df_time_agg$WORKING_TIME, las = 2, cex.axis = 0.8)

# close device (AFTER plotting)
dev.off()

cat("Figure saved to:", fig_path, "\n")


#------------------------------------------------------------------------------#
# 10 - Explore parameters evolution
#------------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(ggplot2)

print(selected_variables)
key_vars <- selected_variables

# Build long-format data frame for the key variables
df_long <- df_time_agg %>%
  dplyr::select(WORKING_TIME, all_of(key_vars)) %>%
  tidyr::pivot_longer(
    cols      = all_of(key_vars),
    names_to  = "parameter",
    values_to = "value"
  )

df_long_scaled <- df_long %>%
  group_by(parameter) %>%
  mutate(z_value = (value - mean(value)) / sd(value)) %>%
  ungroup()

p_scaled <- ggplot(df_long_scaled, aes(x = WORKING_TIME, y = z_value, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ parameter, ncol = 3) +
  geom_vline(xintercept = 8,  linetype = "dashed") +
  geom_vline(xintercept = 17, linetype = "dashed") +
  labs(
    x = "Working time (hours)",
    y = "Standardized value (z-score)",
    title = "Standardized evolution of key surface parameters"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "tomato"),
    panel.grid.minor = element_blank()
  )

p_scaled

# path where you want to save it
fig_path <- file.path(out_dir, "Figure_4.png")

ggsave(
  filename = fig_path,
  plot     = p_scaled,
  width    = 9,   # adjust as needed
  height   = 7,
  dpi      = 300
)

cat("Figure saved to:", fig_path, "\n")

#------------------------------------------------------------------------------#
# 11 - Break training_set into 3 working-time classes
#------------------------------------------------------------------------------#

#-------------------------------------------------- STORE VARIABLES
vars <- selected_variables  # the 7 wear variables
print(vars)

#-------------------------------------------------- RUN CDA with 4 clusters
cda_data <- selected_training_data_clean[, c(vars, "WORKING_TIME")]
cda_data$FACTOR <- cut(
  cda_data$WORKING_TIME,
  breaks = c(-Inf, 0, 8, 16, Inf),
  labels = c("0", "2-8h", "10-16h", "18-36h")
)

# scale predictors
cda_scaled <- cda_data
cda_scaled[vars] <- scale(cda_scaled[vars])

cda_model <- lda(FACTOR ~ ., data = cda_scaled[, c(vars, "FACTOR")])

# Discriminant scores from THIS model
cda_scores <- predict(cda_model)$x

# Data frame for plotting
df <- data.frame(cda_scores, Class = cda_scaled$FACTOR)
print(head(df))

#--------------------------------------------------EXPLORE PARAMETERS THROUGH THE FACTOR

# Convert numeric FACTOR -> factor with custom labels
selected_training_data_clean$FACTOR <- cut(
  selected_training_data_clean$WORKING_TIME,
  breaks = c(-Inf, 0, 8, 16, Inf),  # Adjust these intervals as needed
  labels = c("0", "2-8h", "10-16h", "18-36h")
)

library(tidyr)
# Reshaping the data from wide to long format
long_data <- pivot_longer(selected_training_data_clean, 
                          cols = all_of(vars),
                          names_to = "variable", 
                          values_to = "value")

# Define your color palette
my_palette <- c("#F11119", "#099E76", "#FD8419", "#0074E9")

# Rename FACTOR levels
long_data$FACTOR <- factor(long_data$FACTOR,
                           levels = c("0", "2-8h", "10-16h", "18-36h"), 
                           labels = c("0", "2-8h", "10-16h", "18-36h"))

# Plotting the data
my_plot <- ggplot(long_data, aes(x = as.factor(FACTOR), y = value, fill = as.factor(FACTOR))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Boxplots of Predictors by Wear-Time Classes", x = "FACTOR", y = "Value") +
  theme_bw() +
  scale_fill_manual(values = my_palette)

# Definisci il file di output
file_path <- file.path(out_dir, "Figure_5.jpg")
ggsave(file_path, plot = my_plot, width = 10, height = 8, units = "in", dpi = 300)
cat("Figure saved to:", file_path, "\n")

# Mostra il grafico
print(my_plot)

#--------------------------------------------------------Calculate Centroids for Plotting

# Centroids for plotting
centroids <- aggregate(. ~ Class, data = df, FUN = mean)

# Structure matrix (coefficients of variables on LDs)
canonical_functions <- cda_model$scaling
coefficients_transposed <- canonical_functions   # vars x LDs
print(coefficients_transposed)

file_path <- file.path(out_dir, "4_variables_contributions.csv")
write.csv(coefficients_transposed, file = file_path, row.names = TRUE)


#------------------------------------------------------------------STRUCTURE MATRIX

# Exclude FACTOR and Prediction variables
variables <- selected_training_data_clean[, !(names(selected_training_data_clean) %in% c("WORKING_TIME","FACTOR", "Prediction"))]

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

# Definisci il file di output
file_path <- file.path(out_dir, "4_variables_contributions.csv")

# Save .csv
write.csv(coefficients_transposed, file = file_path, row.names = TRUE)

# Extract the coefficients for LD1 and LD2
coeff_LD1 <- coefficients["LD1", ]
coeff_LD2 <- coefficients["LD2", ]

# Extract proportion of discrimination explained
prop_explained <- (cda_model$svd^2) / sum(cda_model$svd^2)

# Print
prop_explained
round(prop_explained * 100, 1)

cat("LD1 explains", round(prop_explained[1] * 100, 1), "% of between-class variance.\n")
cat("LD2 explains", round(prop_explained[2] * 100, 1), "% of between-class variance.\n")

#--------------------------------------------------------CROSS TABLE WITH PREDICTION RESULTS 

# 1. Predict on the *scaled* data used to fit the model
cda_scaled$Prediction <- predict(cda_model)$class  # no newdata = ... needed

# 2. Make sure both factors have the same levels and order
cda_scaled$FACTOR <- factor(
  cda_scaled$FACTOR,
  levels = c("0", "2-8h", "10-16h", "18-36h")
)

cda_scaled$Prediction <- factor(
  cda_scaled$Prediction,
  levels = c("0", "2-8h", "10-16h", "18-36h")
)
# 3. Confusion matrix (counts)
cross_table <- table(
  True = cda_scaled$FACTOR,
  Pred = cda_scaled$Prediction
)

# 4. Row percentages
row_percentages <- prop.table(cross_table, margin = 1) * 100

# 5. Add margins (sums)
cross_table_with_margins       <- addmargins(cross_table)
row_percentages_with_margins   <- addmargins(row_percentages)

# 6. Combine counts + percentages side by side
cross_table_combined <- cbind(
  cross_table_with_margins,
  row_percentages_with_margins
)

# 7. Rename rows and columns nicely
rownames(cross_table_combined)[1:4] <- c("0h", "2-8h", "10-16h", "18-36h")
colnames(cross_table_combined)[1:4] <- c("0h", "2-8h", "10-16h", "18-36h")
colnames(cross_table_combined)[5]   <- "Sum"
colnames(cross_table_combined)[6:9] <- c("0h%", "2-8h%", "10-16h%", "18-36h%")

# 8. Print the final table
print(cross_table_combined)

# 9. Global accuracy on training set
correct_counts      <- diag(cross_table)    # diagonal of True x Pred
total_observations  <- sum(cross_table)     # total n
global_percentage   <- sum(correct_counts) / total_observations * 100

cat("Global Percentage of Cases Correctly Classified:",
    round(global_percentage, 2), "%\n")

# Definisci il file di output
file_path <- file.path(out_dir, "5_cross_table_results.csv")

# Save .csv
write.csv(
  cross_table_combined,
  file = file_path,
  row.names = TRUE,
  fileEncoding = "UTF-8"
)

#---------------------------------------------------------------SCATTER PLOT ON LD1 and LD2

# 1. Ensure consistent class order
interval_order <- c("0", "2-8h", "10-16h", "18-36h")
df$Class        <- factor(df$Class, levels = interval_order)
centroids$Class <- factor(centroids$Class, levels = interval_order)

# 2. Same palette as training plot
pal_darjeeling <- c(
  "0"       = "#F11119",  # red
  "2-8h"    = "#0074E9",  # blue
  "10-16h"  = "#099E76",  # green/teal
  "18-36h"  = "#FD8419"   # orange
)

# 3. Convex hulls for each class (same logic as hulls_train)
hulls <- df %>%
  group_by(Class) %>%
  slice(chull(LD1, LD2))

# 4. Scatter plot with *identical* aesthetics to p_train_full
scatter_plot <- ggplot() +
  # coloured hulls (filled and edged) for each class
  geom_polygon(
    data   = hulls,
    aes(LD1, LD2, fill = Class, group = Class),
    colour = "black",
    alpha  = 0.25,
    size   = 0.4
  ) +
  # points, coloured by class (fill), black outline
  geom_point(
    data   = df,
    aes(LD1, LD2, fill = Class),
    shape  = 21,
    size   = 2.0,
    stroke = 0.3,
    colour = "black",
    alpha  = 0.9
  ) +
  # centroids (one per class)
  geom_point(
    data   = centroids,
    aes(LD1, LD2),
    shape  = 23,
    size   = 3.0,
    fill   = "yellow",
    colour = "black",
    stroke = 0.5
  ) +
  scale_fill_manual(
    name   = "Wear class",      # same legend title as p_train_full
    values = pal_darjeeling,
    drop   = FALSE
  ) +
  labs(
    title = "Canonical discriminant space (training_set)",
    x = "LD1",
    y = "LD2"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank()
  )

# 5. Save as JPG
file_path <- file.path(out_dir, "Figure_6.jpg")
ggsave(
  file   = file_path,
  plot   = scatter_plot,
  width  = 15,
  height = 10,   
  dpi    = 300,
  bg     = "white"
)

print(scatter_plot)


#------------------------------------------------------------------------------#
# 12 - Apply to experimental tools, test_set
#------------------------------------------------------------------------------#

# Load necessary libraries
library(dplyr)
library(MASS)
library(randomForest)

# Print the column names of test_data to verify
print(colnames(test_data))

# Ensure the columns exist in the test_data
if (!all(c("ID", "WORKING_TIME", selected_variables) %in% colnames(test_data))) {
  stop("One or more columns are missing in the test_data dataframe.")
}

# Filter test experimental data
test_data_exp_clean <- test_data %>% dplyr::select(all_of(vars), WORKING_TIME, ID, TOOL)
print(head(test_data_exp_clean))

# Handle any NA values introduced by coercion
test_data_exp_clean <- test_data_exp_clean %>%
  mutate(across(all_of(vars), ~ ifelse(is.na(.), 0, .)))

# Remove the ID and WORKING_TIME columns before prediction
test_data_exp_for_prediction <- test_data_exp_clean %>%
  dplyr::select(-ID, -TOOL, -WORKING_TIME)

# Perform prediction using the trained CDA model
test_data_exp_clean$Predicted_Factor <- predict(cda_model, newdata = test_data_exp_for_prediction)$class

# Print the first few rows of the predictions
print(head(test_data_exp_clean))

# ---------- Impostazioni di base ----------
interval_order <- c("0", "2-8h", "10-16h", "18-36h")

# compute training means and sds from the *unscaled* CDA input
train_center <- sapply(cda_data[ , vars], mean, na.rm = TRUE)
train_scale  <- sapply(cda_data[ , vars], sd,   na.rm = TRUE)

# make a copy and scale only the predictor columns
test_data_exp_for_prediction_scaled <- test_data_exp_for_prediction
test_data_exp_for_prediction_scaled[ , vars] <-
  sweep(test_data_exp_for_prediction_scaled[ , vars], 2, train_center, "-")
test_data_exp_for_prediction_scaled[ , vars] <-
  sweep(test_data_exp_for_prediction_scaled[ , vars], 2, train_scale,  "/")

# now use the *scaled* version for prediction
test_data_exp_clean$Predicted_Factor <- predict(
  cda_model,
  newdata = test_data_exp_for_prediction_scaled
)$class

# Order of the predicted classes (rows of perc_matrix)
if (!exists("interval_order")) {
  interval_order <- c("0","1-8h","9-16h","17-26h","27-36h")
}

# Threshold for dominance (percent)
threshold <- get0("threshold", ifnotfound = 60)

# Build/refresh the cross table and percent matrix
cross_table <- table(test_data_exp_clean$Predicted_Factor, test_data_exp_clean$WORKING_TIME)
cross_table_percentages <- prop.table(cross_table, margin = 2) * 100
perc_matrix <- as.data.frame.matrix(cross_table_percentages)
perc_matrix <- perc_matrix[interval_order, , drop = FALSE]  # keep row order consistent

# ---------- Funzione unica per "dominante o somma dei più rilevanti" ----------
collapse_intervals <- function(vec_perc, levels_order, threshold = 60,
                               adjacent_only = FALSE, k_start = 2, k_max = 3) {
  # vec_perc: named numeric (names = intervalli)
  x <- as.numeric(vec_perc)
  nm <- names(vec_perc)
  
  # 1) singolo intervallo ≥ soglia
  imax <- which.max(x)
  if (!is.na(imax) && x[imax] >= threshold) {
    return(list(Dominant = nm[imax], Combined = unname(x[imax])))
  }
  
  # helper per adiacenza
  idx_of <- function(n) match(n, levels_order)
  
  if (adjacent_only) {
    # prova coppie/terne adiacenti in ordine (sliding window)
    L <- length(x)
    # prova k = k_start..k_max (es. 2 poi 3)
    best_sum <- -Inf; best_lbl <- NA_character_
    for (k in k_start:k_max) {
      if (L >= k) {
        for (i in 1:(L - k + 1)) {
          s <- sum(x[i:(i+k-1)], na.rm = TRUE)
          if (s > best_sum) {
            best_sum <- s
            best_lbl <- paste(nm[i:(i+k-1)], collapse = "+")
          }
        }
        if (best_sum >= threshold) {
          return(list(Dominant = best_lbl, Combined = best_sum))
        }
      }
    }
    # se non raggiunge la soglia, ritorna la migliore somma trovata
    return(list(Dominant = best_lbl, Combined = best_sum))
  } else {
    # non richiedere adiacenza: prendi i top-k per percentuale
    ord <- order(x, decreasing = TRUE, na.last = NA)
    x_sorted <- x[ord]; nm_sorted <- nm[ord]
    
    # prova a sommare progressivamente i top finché superi la soglia (max k_max)
    for (k in k_start:k_max) {
      if (length(x_sorted) >= k) {
        s <- sum(x_sorted[1:k], na.rm = TRUE)
        lbl <- paste(nm_sorted[1:k], collapse = "+")
        if (s >= threshold) return(list(Dominant = lbl, Combined = s))
      }
    }
    # fallback: restituisci i migliori k_start
    s <- sum(x_sorted[1:min(k_start, length(x_sorted))], na.rm = TRUE)
    lbl <- paste(nm_sorted[1:min(k_start, length(x_sorted))], collapse = "+")
    return(list(Dominant = lbl, Combined = s))
  }
}

# ---------- Risultato per ciascun TOOL (colonna = WORKING_TIME) ----------
# Distribution of predicted classes for each tool
class_distribution_tool <- test_data_exp_clean %>%
  dplyr::count(TOOL, Predicted_Factor, name = "Count") %>%
  dplyr::group_by(TOOL) %>%
  dplyr::mutate(
    Total   = sum(Count),
    Percent = Count / Total * 100
  ) %>%
  dplyr::ungroup()

print(n = 60, class_distribution_tool)


# Ensure every tool has all class levels (even if 0%)
class_distribution_tool_complete <- class_distribution_tool %>%
  tidyr::complete(
    TOOL,
    Predicted_Factor = factor(interval_order, levels = interval_order),
    fill = list(Count = 0, Total = 0, Percent = 0)
  )

# Collapse into dominant interval(s) per tool
dominant_intervals_df_4class <- class_distribution_tool_complete %>%
  dplyr::group_by(TOOL) %>%
  dplyr::summarise({
    v <- Percent
    names(v) <- as.character(Predicted_Factor)
    out <- collapse_intervals(
      v,
      levels_order  = interval_order,
      threshold     = threshold,
      adjacent_only = FALSE,   # as in your original "by TOOL" block
      k_start       = 2,
      k_max         = 3
    )
    tibble::tibble(
      Dominant_Interval = out$Dominant,
      Combined_Percent  = out$Combined
    )
  }, .groups = "drop") %>%
  dplyr::arrange(TOOL)

print(dominant_intervals_df_4class)

# ---------- Riepilogo per ciascun WORKING_TIME (final_class_df) ----------
class_distribution <- test_data_exp_clean %>%
  count(WORKING_TIME, Predicted_Factor, name = "Count") %>%
  group_by(WORKING_TIME) %>%
  mutate(Total = sum(Count), Percent = Count/Total*100) %>%
  ungroup()

final_class_df_4class <- class_distribution %>%
  tidyr::complete(WORKING_TIME, Predicted_Factor = factor(interval_order, levels = interval_order),
                  fill = list(Count = 0, Total = 0, Percent = 0)) %>%
  group_by(WORKING_TIME) %>%
  summarise({
    v <- Percent
    names(v) <- as.character(Predicted_Factor)
    out <- collapse_intervals(v, levels_order = interval_order,
                              threshold = threshold,
                              adjacent_only = TRUE, k_start = 2, k_max = 3)
    tibble(Dominant_Class = out$Dominant, Combined_Percent = out$Combined)
  }, .groups = "drop") %>%
  arrange(WORKING_TIME)

print(final_class_df_4class)

# ---------- Riepilogo per ciascun TOOL (final_class_df) ----------
write.csv(class_distribution_tool,
          file.path(out_dir, "6_class_distribution_by_tools.csv"),
          row.names = FALSE)
write.csv(final_class_df_4class,
          file.path(out_dir, "7_dominant_intervals_by_working_time_classes.csv"),
          row.names = FALSE)


library(dplyr)
library(tidyr)

# Meta table: link ID, TOOL, WORKING_TIME with predicted class
exp_meta <- test_data %>%
  dplyr::select(ID, TOOL, WORKING_TIME) %>%
  dplyr::left_join(
    test_data_exp_clean %>% dplyr::select(ID, Predicted_Factor),
    by = "ID"
  )

interval_order <- c("0", "2-8h", "10-16h", "18-36h")

tool_distribution <- exp_meta %>%
  count(TOOL, Predicted_Factor, name = "Count") %>%
  group_by(TOOL) %>%
  mutate(Total = sum(Count),
         Percent = Count / Total * 100) %>%
  ungroup() %>%
  tidyr::complete(
    TOOL,
    Predicted_Factor = factor(interval_order, levels = interval_order),
    fill = list(Count = 0, Total = 0, Percent = 0)
  )

tool_summary <- tool_distribution %>%
  group_by(TOOL) %>%
  summarise({
    v <- Percent
    names(v) <- as.character(Predicted_Factor)
    out <- collapse_intervals(
      v,
      levels_order   = interval_order,
      threshold      = threshold,   # e.g. 60
      adjacent_only  = TRUE,        # or FALSE, as you prefer
      k_start        = 2,
      k_max          = 3
    )
    tibble(
      Dominant_Class    = out$Dominant,
      Combined_Percent  = out$Combined
    )
  }, .groups = "drop") %>%
  arrange(TOOL)

print(tool_summary)


#------------------------------------------------------------------------------#
# 13 - Plot to experimental tools against the training_set
#------------------------------------------------------------------------------#

# ---------- Project CAT=4 (test) into the same LDA space ----------
# Use the *scaled* predictors, consistent with how cda_model was trained
test_scores <- predict(cda_model, newdata = test_data_exp_for_prediction_scaled)

df_test <- data.frame(
  LD1   = test_scores$x[, "LD1"],
  LD2   = test_scores$x[, "LD2"],
  Class = factor(test_data_exp_clean$Predicted_Factor, levels = interval_order),
  TOOL  = test_data$TOOL,
  ID    = test_data$ID,
  WT    = test_data$WORKING_TIME
)

# TOOL as Factor
df_test$TOOL <- factor(
  df_test$TOOL,
  levels = c(
    "EXP_1_0h",
    "EXP_2_2h",
    "EXP_3_4h",
    "EXP_4_8h",
    "EXP_5_10h",
    "EXP_6_10h",
    "EXP_7_10h",
    "EXP_8_10h",
    "EXP_9_13h",
    "EXP_10_16h",    
    "EXP_11_26h",
    "EXP_12_34h",
    "EXP_13_38h"
    ),
  ordered = TRUE
)

# ---------- Convex hulls on TRAINING classes ----------
library(dplyr); library(ggplot2)
hulls_train <- df %>% group_by(Class) %>% slice(chull(LD1, LD2))

# ---------- Combined plot ----------
p_ld <- ggplot() +
  # training hulls
  geom_polygon(
    data = hulls_train,
    aes(x = LD1, y = LD2, fill = Class, group = Class),
    alpha = 0.10, color = NA
  ) +
  # training points
  geom_point(
    data = df,
    aes(x = LD1, y = LD2, color = Class),
    size = 1.5, alpha = 0.6
  ) +
  # training centroids
  geom_point(
    data = centroids,
    aes(x = LD1, y = LD2),
    shape = 23, size = 3.5, fill = "yellow", color = "black", stroke = 0.4
  ) +
  # CAT=4 projected points
  geom_point(
    data = df_test,
    aes(x = LD1, y = LD2, shape = Class),
    size = 2.2, alpha = 0.95, stroke = 0.6, color = "black", fill = NA
  ) +
  # aesthetics
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "LDA space: Training (color/fill) + CAT=4 projections (black outline shapes)",
       x = "LD1", y = "LD2") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# Save and print
# ggsave(
#  filename = file.path(out_dir, "Figure_7.jpg"),
#  plot     = p_ld,
#  width    = 14,
#  height   = 10,
#  dpi      = 400,
#  bg       = "white"
#)

print(p_ld)

p_tool <- ggplot(df_test, aes(LD1, LD2)) +
  # training hulls (drawn on every facet)
  geom_polygon(
    data = hulls_train,
    aes(LD1, LD2, fill = Class, group = Class),
    inherit.aes = FALSE, alpha = 0.10, color = NA
  ) +
  geom_point(
    data = df, aes(LD1, LD2, color = Class),
    inherit.aes = FALSE, size = 1.2, alpha = 0.5
  ) +
  geom_point(
    data = centroids,
    aes(LD1, LD2),
    inherit.aes = FALSE, shape = 23, size = 3, fill = "yellow", color = "black", stroke = 0.4
  ) +
  # CAT=4 points (one facet per TOOL)
  geom_point(aes(shape = Class), size = 2.2, stroke = 0.6, color = "black") +
  facet_wrap(vars(TOOL), scales = "free") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "CAT=4 projections by TOOL (overlaid on training LDA space)",
       x = "LD1", y = "LD2") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# Save and print
# ggsave(
#  filename = file.path(out_dir, "Figure_6.jpg"),
#  plot     = p_tool,
#  width    = 14,
#  height   = 10,
#  dpi      = 400,
#  bg       = "white"
#)

print(p_tool)


#-----------------------------------------------------------ALTERNATIVE PLOTTING

suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

interval_order <- c("0","2-8h","10-16h","18-36h")
df$Class      <- factor(df$Class,      levels = interval_order)
df_test$Class <- factor(df_test$Class, levels = interval_order)

pal_darjeeling <- c(
  "0"       = "#F11119",  # red
  "2-8h"    = "#0074E9",  # blue
  "10-16h"  = "#099E76",  # green/teal
  "18-36h"  = "#FD8419"   # orange
)

p_tool_grey <- ggplot() +
  # training hulls: grey fill
  geom_polygon(
    data = hulls_train,
    aes(LD1, LD2, group = Class),
    fill  = "grey85",
    color = NA,
    alpha = 0.35
  ) +
  # training hull borders: coloured, thinner, semi-transparent
  geom_polygon(
    data = hulls_train,
    aes(LD1, LD2, group = Class, colour = Class),
    fill  = NA,
    size  = 0.2,       # thinner line
    alpha = 0.8        # added transparency
  ) +
  # training points: grey
  geom_point(
    data = df,
    aes(LD1, LD2),
    color = "grey55", fill = "grey55",
    shape = 21, size = 1.0, alpha = 0.30, stroke = 0.15
  ) +
  # experimental tools: coloured by class (fill)
  geom_point(
    data = df_test,
    aes(LD1, LD2, fill = Class),
    shape = 21, size = 2.3, alpha = 0.95,
    color = "black", stroke = 0.25
  ) +
  scale_fill_manual(
    name   = "Experimental class",
    values = pal_darjeeling
  ) +
  # coloured hull borders use same palette, no extra legend
  scale_colour_manual(
    values = pal_darjeeling,
    guide  = "none"
  ) +
  facet_wrap(~ TOOL, scales = "free") +
  labs(
    title = "Projections of experimental test tools",
    x = "LD1", y = "LD2"
  ) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank())

# ggsave(
#  filename = file.path(out_dir, "Figure_7.jpg"),
#  plot     = p_tool_grey,
#  width    = 14,
#  height   = 10,
#  dpi      = 300,
#  bg       = "white"
#)

print(p_tool_grey)


#-----------------------------------------------------------ALTERNATIVE PLOTTING

suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

# Make sure class order is consistent
interval_order <- c("0","2-8h","10-16h","18-36h")
df$Class       <- factor(df$Class, levels = interval_order)

# Same palette used for experimental classes
pal_darjeeling <- c(
  "0"       = "#F11119",  # red
  "2-8h"    = "#0074E9",  # blue
  "10-16h"  = "#099E76",  # green/teal
  "18-36h"  = "#FD8419"   # orange
)

# If hulls_train is not yet defined in this session, (re)compute:
# hulls_train <- df %>% dplyr::group_by(Class) %>% dplyr::slice(chull(LD1, LD2))

p_train_full <- ggplot() +
  # coloured hulls (filled and edged) for each training class
  geom_polygon(
    data = hulls_train,
    aes(LD1, LD2, fill = Class, group = Class),
    colour = "black",
    alpha  = 0.25,
    size   = 0.4
  ) +
  # training points, coloured by class
  geom_point(
    data = df,
    aes(LD1, LD2, fill = Class),
    shape = 21, size = 2.0, stroke = 0.3,
    colour = "black", alpha = 0.9
  ) +
  # centroids (one per class)
  geom_point(
    data = centroids,
    aes(LD1, LD2),
    shape = 23, size = 3.0,
    fill = "yellow", colour = "black", stroke = 0.5
  ) +
  scale_fill_manual(
    name   = "Wear class",
    values = pal_darjeeling,
    drop   = FALSE
  ) +
  labs(
    title = "Canonical discriminant space of training surfaces",
    x = "LD1",
    y = "LD2"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank()
  )

# Save as (roughly) square figure
# ggsave(
#  filename = file.path(out_dir, "Figure_8.jpg"),
#  plot     = p_train_full,
#  width    = 10,
#  height   = 10,
#  dpi      = 300,
#  bg       = "white"
#)

print(p_train_full)


#---------------------------------------------------------ALTERNATIVE PLOTTING_2

suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

# --------------------------------------------------------------------
# Ensure consistent class levels
# --------------------------------------------------------------------
interval_order <- c("0","2-8h","10-16h","18-36h")
df$Class      <- factor(df$Class,      levels = interval_order)
df_test$Class <- factor(df_test$Class, levels = interval_order)

# Colour palette for wear classes
pal_darjeeling <- c(
  "0"       = "#F11119",  # red
  "2-8h"    = "#0074E9",  # blue
  "10-16h"  = "#099E76",  # green/teal
  "18-36h"  = "#FD8419"   # orange
)

# --------------------------------------------------------------------
# Build TRAINING facet data
# --------------------------------------------------------------------
df_train_panel <- df %>%
  mutate(TOOL = "TRAINING")

hulls_train_panel <- hulls_train %>%
  mutate(TOOL = "TRAINING")

# --------------------------------------------------------------------
# Set TOOL order: TRAINING first, then experimental tools in time order
# --------------------------------------------------------------------
tool_levels <- c(
  "TRAINING",
  "EXP_1_0h",
  "EXP_2_2h",
  "EXP_3_4h",
  "EXP_4_8h",
  "EXP_5_10h",
  "EXP_6_10h",
  "EXP_7_10h",
  "EXP_8_10h",
  "EXP_9_13h",
  "EXP_10_16h",    
  "EXP_11_26h",
  "EXP_12_34h",
  "EXP_13_38h"
)

df_test$TOOL           <- factor(df_test$TOOL,           levels = tool_levels)
df_train_panel$TOOL    <- factor(df_train_panel$TOOL,    levels = tool_levels)
hulls_train_panel$TOOL <- factor(hulls_train_panel$TOOL, levels = tool_levels)

# --------------------------------------------------------------------
# Combined plot : grey background + coloured borders + TRAINING facet coloured
# --------------------------------------------------------------------
p_tool_grey <- ggplot() +
  # --- GREY FILLED HULLS (shown in ALL facets) ---
  geom_polygon(
    data = hulls_train,
    aes(LD1, LD2, group = Class),
    fill  = "grey85",
    color = NA,
    alpha = 0.35
  ) +
  # --- COLOURED BORDERS for hulls (ALL facets) ---
  geom_polygon(
    data = hulls_train,
    aes(LD1, LD2, group = Class, colour = Class),
    fill  = NA,
    size  = 0.25,     # thinner line
    alpha = 0.4       # transparent
  ) +
  # --- GREY TRAINING POINTS (background in ALL facets) ---
  geom_point(
    data = df,
    aes(LD1, LD2),
    color = "grey55", fill = "grey55",
    shape = 21, size = 1.0, alpha = 0.30, stroke = 0.15
  ) +
  # --- COLOURED TRAINING FACET (TOOL == "TRAINING") ---
  geom_polygon(
    data  = hulls_train_panel,
    aes(LD1, LD2, group = Class, fill = Class, TOOL = TOOL),
    color = "black",
    size  = 0.25,
    alpha = 0.4
  ) +
  geom_point(
    data  = df_train_panel,
    aes(LD1, LD2, fill = Class, TOOL = TOOL),
    shape = 21, size = 2.3,
    color = "black", stroke = 0.25
  ) +
  # --- EXPERIMENTAL TOOLS (all other TOOL facets) ---
  geom_point(
    data  = df_test,
    aes(LD1, LD2, fill = Class, TOOL = TOOL),
    shape = 21, size = 2.3, alpha = 0.95,
    color = "black", stroke = 0.25
  ) +
  # Fill for points + coloured training hulls
  scale_fill_manual(
    name   = "Wear class",
    values = pal_darjeeling,
    drop   = FALSE
  ) +
  # Coloured hull borders (no extra legend)
  scale_colour_manual(
    values = pal_darjeeling,
    guide  = "none"
  ) +
  facet_wrap(~ TOOL, scales = "free") +
  labs(
    x = "LD1", y = "LD2"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )

ggsave(
  filename = file.path(out_dir, "Figure_7.jpg"),
  plot     = p_tool_grey,
  width    = 14,
  height   = 10,
  dpi      = 300,
  bg       = "white"
)

print(p_tool_grey)


#------------------------------------------------------------------------------#
# 14 - From wear to straw: translate polish into cumulative straw counts
#------------------------------------------------------------------------------#

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(MASS)       # lda
  library(minpack.lm) # nlsLM
  library(mgcv)       # GAM
  library(betareg)    # beta regression
  library(ggrepel)
  library(tidyr)
})

###############################################################################
# PRECONDITIONS & TRAINING DATA PREPARATION
###############################################################################

stopifnot(exists("selected_training_data_clean2"))
stopifnot(exists("test_data"))

df_train <- selected_training_data_clean2
stopifnot("WORKING_TIME" %in% names(df_train))

cat("\nTraining data structure:\n")
print(str(df_train))

# Keep numeric predictors, drop WORKING_TIME
X_lda <- df_train %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-WORKING_TIME)

# Drop zero-variance columns (defensive)
X_lda <- X_lda %>%
  dplyr::select(where(~ var(.x, na.rm = TRUE) > 0))

# 5 progressive wear classes (quintiles of WORKING_TIME)
br   <- quantile(df_train$WORKING_TIME,
                 probs = seq(0, 1, length.out = 6),
                 na.rm = TRUE)
br   <- unique(br)
labs <- paste0("Q", seq_len(length(br) - 1))
y_cls <- cut(df_train$WORKING_TIME, breaks = br, labels = labs,
             include.lowest = TRUE)

# Keep complete rows
mask   <- complete.cases(X_lda) & !is.na(y_cls)
X_lda2 <- X_lda[mask, , drop = FALSE]
y_cls2 <- y_cls[mask]

cat("\nRows used for LDA:", nrow(X_lda2), "\n")
print(table(y_cls2))

###############################################################################
# FIT LDA AND DEFINE LD1 WEAR AXIS
###############################################################################

cda_model  <- lda(X_lda2, grouping = y_cls2)
lda_scores <- predict(cda_model)$x    # LD1, LD2, ...

# Wear axis:
LD1 <- lda_scores[, "LD1"]

# Discriminant eigenvalues and proportion of variance
eigvals    <- cda_model$svd^2
prop_var   <- eigvals / sum(eigvals)
LD1_percent <- prop_var[1] * 100

cat(sprintf("\nLD1 explains %.1f%% of discriminant variance.\n", LD1_percent))
print(prop_var)

###############################################################################
# BUILD HARVESTING TRAINING DATA (LD1 → CUMULATIVE STRAWS)
###############################################################################

RATE_AVG_H <- 3797.5  # experimentally derived average straws per hour

df_harv <- data.frame(
  LD1     = LD1,
  CUM_AVG = RATE_AVG_H * df_train$WORKING_TIME[mask],
  Class   = y_cls2
)

cat("\nRange of CUM_AVG (ground-truth straws):\n")
print(range(df_harv$CUM_AVG))

# For models that need normalisation (beta-regression)
y_min <- min(df_harv$CUM_AVG)
y_max <- max(df_harv$CUM_AVG)

###############################################################################
# MODEL COMPARISON FOR LD1 → CUM_AVG
###############################################################################

fit_model <- function(name, data) {
  x  <- data$LD1
  y  <- data$CUM_AVG
  yb <- (y - y_min) / (y_max - y_min)
  yb <- pmin(pmax(yb, 1e-6), 1 - 1e-6)
  
  if (name == "linear") {
    lm(CUM_AVG ~ LD1, data = data)
    
  } else if (name == "quadratic") {
    lm(CUM_AVG ~ poly(LD1, 2, raw = TRUE), data = data)
    
  } else if (name == "exponential") {
    c0 <- max(min(y), 0)
    A0 <- max(y) - c0 + 1e-6
    b0 <- 0.3 / (sd(x) + 1e-6)
    nlsLM(CUM_AVG ~ c + A * exp(b * LD1),
          data   = data,
          start  = list(c = c0, A = A0, b = b0),
          lower  = c(-Inf, 0, -Inf),
          control = nls.lm.control(maxiter = 1000))
    
  } else if (name == "logistic4") {
    # 4-parameter logistic: y = d + (a - d) / (1 + exp((c - LD1)/b))
    a0 <- max(y); d0 <- min(y); c0 <- stats::median(x); b0 <- diff(range(x)) / 6
    nlsLM(CUM_AVG ~ d + (a - d) / (1 + exp((c - LD1)/b)),
          data   = data,
          start  = list(a = a0, d = d0, c = c0, b = b0),
          lower  = c(-Inf, -Inf, min(x), 1e-6),
          control = nls.lm.control(maxiter = 1000))
    
  } else if (name == "gam_gauss") {
    mgcv::gam(CUM_AVG ~ s(LD1, k = 6), data = data, method = "REML")
    
  } else if (name == "beta_poly") {
    betareg::betareg(
      yb ~ poly(LD1, 2, raw = TRUE),
      data = transform(data, yb = yb),
      link = "logit"
    )
    
  } else {
    stop("Unknown model name: ", name)
  }
}

# Prediction helper (always returns CUM_AVG scale)
predict_CUM <- function(fit, name, newdata) {
  if (name %in% c("linear", "quadratic", "exponential", "logistic4", "gam_gauss")) {
    as.numeric(predict(fit, newdata = newdata))
    
  } else if (name == "beta_poly") {
    p_hat <- as.numeric(predict(fit, newdata = newdata, type = "response"))
    p_hat * (y_max - y_min) + y_min
  } else {
    stop("Unknown model name in predict_CUM: ", name)
  }
}

# Fit models and compute AIC ---------------------------------------------

model_names <- c("linear", "quadratic", "exponential",
                 "logistic4", "gam_gauss", "beta_poly")

fits   <- list()
aic_vec <- rep(NA_real_, length(model_names))
names(aic_vec) <- model_names

for (nm in model_names) {
  cat("Fitting", nm, "...\n")
  fit_nm <- try(fit_model(nm, df_harv), silent = TRUE)
  if (inherits(fit_nm, "try-error")) {
    warning("Model ", nm, " failed on full data.")
    next
  }
  fits[[nm]]  <- fit_nm
  aic_vec[nm] <- tryCatch(AIC(fit_nm), error = function(e) NA_real_)
}

cat("\nAIC for all fitted models:\n")
print(sort(aic_vec))

# 4.2 10-fold CV RMSE on CUM_AVG ---------------------------------------------

set.seed(123)
k       <- 10
n       <- nrow(df_harv)
fold_id <- sample(rep(1:k, length.out = n))

rmse_vec <- rep(NA_real_, length(model_names))
names(rmse_vec) <- model_names

for (nm in model_names) {
  if (is.null(fits[[nm]])) next
  
  sqerrs <- c()
  for (fold in 1:k) {
    tr <- df_harv[fold_id != fold, , drop = FALSE]
    te <- df_harv[fold_id == fold, , drop = FALSE]
    
    fit_cv <- try(fit_model(nm, tr), silent = TRUE)
    if (inherits(fit_cv, "try-error")) {
      sqerrs <- c(sqerrs, NA_real_)
      next
    }
    
    pred_te <- try(predict_CUM(fit_cv, nm, te), silent = TRUE)
    if (inherits(pred_te, "try-error")) {
      sqerrs <- c(sqerrs, NA_real_)
      next
    }
    
    sqerrs <- c(sqerrs, (te$CUM_AVG - pred_te)^2)
  }
  
  rmse_vec[nm] <- sqrt(mean(sqerrs, na.rm = TRUE))
}

cat("\n10-fold CV RMSE (CUM_AVG scale):\n")
print(sort(rmse_vec))

model_summary <- data.frame(
  model = model_names,
  AIC   = aic_vec[model_names],
  RMSE  = rmse_vec[model_names]
) %>% arrange(AIC)

cat("\nModel comparison summary (by AIC):\n")
print(model_summary)

# Normalised RMSE on (0,1) scale ----------------------------------------

rmse_beta_scale <- function(name, fits, data) {
  fit <- fits[[name]]
  yb_true <- (data$CUM_AVG - y_min) / (y_max - y_min)
  yb_true <- pmin(pmax(yb_true, 1e-6), 1 - 1e-6)
  
  if (name == "beta_poly") {
    yb_pred <- as.numeric(predict(fit, newdata = data, type = "response"))
  } else {
    y_pred  <- predict_CUM(fit, name, data)
    yb_pred <- (y_pred - y_min) / (y_max - y_min)
    yb_pred <- pmin(pmax(yb_pred, 1e-6), 1 - 1e-6)
  }
  
  sqrt(mean((yb_true - yb_pred)^2, na.rm = TRUE))
}

rmse_norm <- sapply(names(fits), rmse_beta_scale, fits = fits, data = df_harv)
rmse_norm_df <- data.frame(
  model           = names(fits),
  RMSE_normalised = rmse_norm
) %>% arrange(RMSE_normalised)

cat("\nNormalised RMSE on (0,1) scale:\n")
print(rmse_norm_df)

###############################################################################
# OVERLAY PLOT OF ALL MAIN MODELS (FOR SUPPLEMENTARY FIGURE)
###############################################################################

LD1_grid <- seq(min(df_harv$LD1), max(df_harv$LD1), length.out = 400)
grid     <- data.frame(LD1 = LD1_grid)

curve_df <- data.frame(
  LD1        = LD1_grid,
  beta_poly  = predict_CUM(fits[["beta_poly"]],   "beta_poly",   grid),
  gam_gauss  = predict_CUM(fits[["gam_gauss"]],   "gam_gauss",   grid),
  logistic4  = predict_CUM(fits[["logistic4"]],   "logistic4",   grid),
  exponential = predict_CUM(fits[["exponential"]], "exponential", grid)
) %>%
  tidyr::pivot_longer(-LD1, names_to = "model", values_to = "CUM")

p_compare <- ggplot(df_harv, aes(LD1, CUM_AVG)) +
  geom_point(alpha = 0.25, size = 1.3, color = "grey70") +
  geom_line(data = curve_df,
            aes(LD1, CUM, colour = model),
            linewidth = 1.1) +
  scale_colour_manual(
    values = c(
      beta_poly   = "#E64B35",
      gam_gauss   = "#4DBBD5",
      logistic4   = "#00A087",
      exponential = "#F39C12"
    ),
    labels = c(
      beta_poly   = "Beta regression",
      gam_gauss   = "GAM (Gaussian)",
      logistic4   = "4-parameter logistic",
      exponential = "Exponential"
    ),
    name = ""
  ) +
  labs(
    x = "LD1 (supervised wear axis)",
    y = "Cumulative harvested straws",
    title = "Alternative calibration models LD1 → cumulative harvest"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title      = element_text(face = "bold", hjust = 0.5)
  )

save_plot(p_compare, "Figure_8.png")

###############################################################################
# CHOSEN FINAL MODEL: LINEAR
###############################################################################

# Reuse fitted logistic4 from the comparison step
harv_model <- fits[["linear"]]
stopifnot(!is.null(harv_model))

cat("\n*** Final harvesting model: linear ***\n")
print(summary(harv_model))

# Save model to disk
saveRDS(harv_model, file = file.path(out_dir, "Harvesting_model_fitted_linear.RDS"))
cat("Model saved as:",
    file.path(out_dir, "Harvesting_model_fitted_linear.RDS"), "\n")

# Prediction function on CUM_AVG scale
predict_harvest <- function(LD1_new) {
  predict(harv_model, newdata = data.frame(LD1 = LD1_new))
}

# Plot final logistic curve + data (Figure for main text)
pred_mod <- data.frame(
  LD1 = LD1_grid,
  CUM = predict_harvest(LD1_grid)
)

p7 <- ggplot() +
  geom_point(
    data  = df_harv,
    aes(x = LD1, y = CUM_AVG),
    alpha = 0.22,
    size  = 1.6,
    color = "grey50"
  ) +
  geom_line(
    data  = pred_mod,
    aes(x = LD1, y = CUM),
    colour   = "tomato",
    linewidth = 1.4
  ) +
  labs(
    title = "Harvesting model (linear)",
    x     = "LD1 (supervised wear axis)",
    y     = "Cumulative harvested straws"
  ) +
  coord_cartesian(
    xlim = range(df_harv$LD1, na.rm = TRUE),
    ylim = range(df_harv$CUM_AVG, na.rm = TRUE)
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(margin = ggplot2::margin(t = 8)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 8)),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA)
  )

save_plot(p7, "Figure_9.png")

###############################################################################
# PROJECT TEST TOOLS INTO LD1 SPACE AND PREDICT STRAWS
###############################################################################

exp_raw <- test_data   # each row = sub-area of a tool

# Keep same predictors as in X_lda2
common <- intersect(colnames(X_lda2), colnames(exp_raw))
exp_X   <- exp_raw[, common, drop = FALSE] %>%
  mutate(across(everything(), as.numeric))

# LDA scores for sub-areas
exp_scores <- predict(cda_model, newdata = exp_X)$x
exp_LD1    <- exp_scores[, "LD1"]

# Predicted cumulative straws per sub-area
exp_pred_straws <- predict_harvest(exp_LD1)

df_exp_pred_subareas <- data.frame(
  TOOL         = exp_raw$TOOL,
  WORKING_TIME = exp_raw$WORKING_TIME,
  LD1          = exp_LD1,
  STRAWS       = exp_pred_straws
)

###############################################################################
# AGGREGATE PREDICTIONS PER TOOL
###############################################################################

df_exp_pred_tools <- df_exp_pred_subareas %>%
  group_by(TOOL) %>%
  summarise(
    N_subareas       = n(),
    # LD1
    LD1_mean         = mean(LD1, na.rm = TRUE),
    LD1_min          = min(LD1, na.rm = TRUE),
    LD1_max          = max(LD1, na.rm = TRUE),
    LD1_sd           = sd(LD1,  na.rm = TRUE),
    # STRAWS
    STRAWS_mean      = mean(STRAWS, na.rm = TRUE),
    STRAWS_min       = min(STRAWS, na.rm = TRUE),
    STRAWS_max       = max(STRAWS, na.rm = TRUE),
    STRAWS_sd        = sd(STRAWS,  na.rm = TRUE),
    # Ground-truth time (if available)
    WORKING_TIME_mean = mean(WORKING_TIME, na.rm = TRUE),
    WORKING_TIME_min  = min(WORKING_TIME, na.rm = TRUE),
    WORKING_TIME_max  = max(WORKING_TIME, na.rm = TRUE),
    .groups = "drop"
  )

# Save aggregated predictions
csv_tools <- file.path(out_dir, "8_harvested_straw_prediction_tools.csv")
write.csv(df_exp_pred_tools, file = csv_tools, row.names = FALSE)
cat("Saved tool-level harvested straw predictions to:\n", csv_tools, "\n")

###############################################################################
# PLOT: HARVESTING MODEL + TOOL MEANS
###############################################################################

df_test_for_plot <- df_exp_pred_tools %>%
  dplyr::select(TOOL, LD1_mean, STRAWS_mean)

p8 <- ggplot() +
  # 1) Training points (background cloud)
  geom_point(
    data  = df_harv,
    aes(x = LD1, y = CUM_AVG),
    alpha = 0.22,          # slightly lighter
    size  = 1.6,
    color = "grey50"
  ) +
  
  # 2) Logistic calibration curve
  geom_line(
    data  = pred_mod,
    aes(x = LD1, y = CUM),
    colour   = "tomato",
    linewidth = 1.4
  ) +
  
  # 3) Experimental tools (emphasised markers)
  geom_point(
    data  = df_test_for_plot,
    aes(x = LD1_mean, y = STRAWS_mean),
    shape = 23,            # filled diamond
    fill  = "gold",
    color = "black",
    size  = 3.8,
    stroke = 0.55
  ) +
  
  # 4) Labels for tools – slightly nudged above points
  ggrepel::geom_text_repel(
    data  = df_test_for_plot,
    aes(x = LD1_mean, y = STRAWS_mean, label = TOOL),
    size          = 3.1,
    color         = "black",
    max.overlaps  = 100,
    box.padding   = 0.25,
    point.padding = 0.2,
    min.segment.length = 0.1,
    segment.size  = 0.2,
    force           = 65,      # NEW: stronger repulsion → more spacing
    force_pull      = 5       # keeps labels from collapsing toward centre
  ) +
  
  # 5) Same axis, title, & limits as Figure 7
  labs(
    title = "Harvesting model with experimental tools",
    x     = "LD1 (supervised wear axis)",
    y     = "Cumulative harvested straws"
  ) +
  coord_cartesian(
    xlim = range(df_harv$LD1, na.rm = TRUE),
    ylim = range(df_harv$CUM_AVG, na.rm = TRUE)
  ) +
  
  # 6) Theme: classic + very faint major gridlines
  theme_classic(base_size = 10) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(margin = ggplot2::margin(t = 8)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 8)),
    # subtle grid to aid reading, but not too strong
    panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    # classic() turns off grid, so we need panel.background
    panel.background = element_rect(fill = "white", colour = NA)
  )

save_plot(p8, "Figure_10.png")

###############################################################################
# PLOT FIGURES TOGETHER
###############################################################################

suppressPackageStartupMessages({
  library(patchwork)
})

fig9 <- p7 + p8 + 
  plot_annotation(tag_levels = "A")  # panels labelled A, B

# Save combined figure
save_plot(fig9, "Figure_11.png", width = 14, height = 5)

cat("\nSaved combined figure to:\n  ",
    file.path(out_dir, "Figure_11.png"), "\n")


#------------------------------------------------------------------------------#
# 15 - Convert straw to harvested surface area
#------------------------------------------------------------------------------#

# Densities (straws per m²)
density_min <- 96   # dense field → small area for same straws
density_avg <- 145
density_max <- 268  # sparse field → large area for same straws

df_exp_pred_tools <- df_exp_pred_tools %>%
  mutate(
    AREA_min_m2 = STRAWS_min  / density_max,  # minimum area (max density)
    AREA_avg_m2 = STRAWS_mean / density_avg,  # best guess
    AREA_max_m2 = STRAWS_max  / density_min   # maximum area (min density)
  )

# Save surface estimation
csv_surf <- file.path(out_dir, "9_harvested_surface_estimation.csv")
write.csv(df_exp_pred_tools, file = csv_surf, row.names = FALSE)
cat("Saved harvested surface estimation to:\n", csv_surf, "\n")

###############################################################################
# SUMMARY TABLE
###############################################################################

summary_table <- df_exp_pred_tools %>%
  dplyr::select(
    TOOL,
    LD1_mean,
    STRAWS_mean,
    AREA_min_m2,
    AREA_avg_m2,
    AREA_max_m2
  )

cat("\nSummary table (per tool):\n")
print(summary_table)

#------------------------------------------------------------------------------#
# 16 - Convert straw into yields
#------------------------------------------------------------------------------#

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

# -------------------------------------------------
# Biological and physical parameters
# -------------------------------------------------
GRAINS_PER_STRAW <- c(avg = 48.2)  # grains per straw for Gentil Rosso wheat
GRAIN_WEIGHT_G   <- 0.0375         # g per grain (~37.5 mg) for Gentil Rosso wheat

# -------------------------------------------------
# Start from your tool-level predictions
# df_exp_pred_tools already exists
# (TOOL, STRAWS_min, STRAWS_mean, STRAWS_max, AREA_*_m2, etc.)
# -------------------------------------------------

df_yield <- df_exp_pred_tools %>%
  mutate(
    # ----- grains -----
    grains_avg = STRAWS_mean * GRAINS_PER_STRAW["avg"],

    # ----- kg of grain -----
    kg_avg = grains_avg * GRAIN_WEIGHT_G / 1000,

    # ----- quintali -----
    t_avg  = kg_avg / 1000,
  )

# Optional: keep only the columns you want in the paper table
tool_yield_table <- df_yield %>%
  transmute(
    TOOL,
    LD1_mean,
    STRAWS_min, STRAWS_mean, STRAWS_max,
    kg_avg, t_avg
  ) %>%
  arrange(LD1_mean)

print(tool_yield_table, n = Inf)

# -------------------------------------------------
# Save CSV in your OUT directory
# -------------------------------------------------
csv_path <- file.path(out_dir, "10_tool_harvest_conversions.csv")
write_csv(tool_yield_table, csv_path)

cat("\nSaved tool harvest conversions to:\n", csv_path, "\n")

#------------------------------------------------------------------------------#
# 17 - END
#------------------------------------------------------------------------------#


