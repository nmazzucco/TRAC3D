###############################################################################
# GOB HARVESTING-INTENSITY MODEL
#
# Training data CAT == 1 -> predict WORKING_TIME directly
# Archaeological data CAT == 2 -> estimate working time, harvested stems,
# grain output, and tool-level harvesting intensity.
#
# Expected folder structure:
#
# TRAC3D/
# └── CER_GOB/
#   ├── CER_GOB.R
#   ├── README.md
#   ├── RAWDATA/
#   └── OUT/
#
# Run from inside TRAC3D/CE_GOB:
#
#   Rscript CER_GOB.R
#
###############################################################################

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(caret)
  library(randomForest)
  library(tibble)
})

set.seed(123)

###############################################################################
# 0) SETTINGS AND HELPERS
###############################################################################

RATE_STEMS_PER_HOUR <- 3797.5

# Grain conversion used here:
# 0.30 g grain per harvested stem.
# If preferred, replace this with:
# GRAIN_G_PER_STEM <- 29.74 * 0.0041
GRAIN_G_PER_STEM <- 0.30

manual_predictors <- c(
  "Vmc",   # Core material volume
  "Sa",    # Arithmetic mean height
  "Sk",    # Core roughness depth
  "Vvc",   # Core void volume
  "Vvv",   # Valley void volume
  "Smr1",  # Upper material ratio
  "Smr2",  # Lower material ratio
  "Spk",   # Reduced peak height
  "Svk",   # Reduced valley depth
  "Sq",    # Root mean square height
  "Sz",    # Maximum height
  "Vm",    # Material volume
  "Vv",    # Void volume
  "Spc",   # Peak curvature
  "Spd"    # Peak density
)

numeric_text_cols <- c(
  "WORKING_TIME",
  "S5v",
  "Sda",
  "Sdv",
  "Sdar",
  "Spc"
)

save_csv <- function(x, filename) {
  write.csv(
    x,
    file.path(out_dir, filename),
    row.names = FALSE
  )
}

save_plot <- function(plot, filename, width = 8, height = 6) {
  ggsave(
    file.path(out_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

check_required_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

summarise_predictions <- function(data, group_vars) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      N_subareas = sum(!is.na(HOURS_CLAMPED)),

      HOURS_mean = mean(HOURS_CLAMPED, na.rm = TRUE),
      HOURS_median = median(HOURS_CLAMPED, na.rm = TRUE),
      HOURS_sd = sd(HOURS_CLAMPED, na.rm = TRUE),
      HOURS_mad = mad(HOURS_CLAMPED, na.rm = TRUE),

      STEMS_mean = mean(STEMS_CLAMPED, na.rm = TRUE),
      STEMS_median = median(STEMS_CLAMPED, na.rm = TRUE),
      STEMS_sd = sd(STEMS_CLAMPED, na.rm = TRUE),
      STEMS_mad = mad(STEMS_CLAMPED, na.rm = TRUE),

      .groups = "drop"
    )
}

classify_intensity <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    x <= 8 ~ "low",
    x <= 18 ~ "mid",
    x <= 28 ~ "high",
    x > 28 ~ "very high"
  )
}

###############################################################################
# 1) PATHS
###############################################################################

project_dir <- getwd()

input_file <- file.path(project_dir, "RAWDATA", "RAWDATA.xlsx")
out_dir <- file.path(project_dir, "OUT")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat("Project directory:", project_dir, "\n")
cat("Input file:", input_file, "\n")
cat("Output directory:", out_dir, "\n")

if (!file.exists(input_file)) {
  stop(
    "Input file not found. Expected file at: ",
    input_file,
    call. = FALSE
  )
}

###############################################################################
# 2) LOAD AND CLEAN DATA
###############################################################################

raw_data <- read_excel(input_file) %>%
  filter(!if_all(everything(), ~ is.na(.)))

check_required_columns(
  raw_data,
  required_cols = c("CAT", "WORKING_TIME", "TOOL")
)

# Remove columns that are fully empty or constant.
constant_columns <- sapply(
  raw_data,
  function(col) length(unique(col[!is.na(col)])) <= 1
)

raw_data <- raw_data[, !constant_columns, drop = FALSE]

# Convert known numeric-like text columns.
for (v in intersect(numeric_text_cols, names(raw_data))) {
  raw_data[[v]] <- suppressWarnings(
    as.numeric(trimws(as.character(raw_data[[v]])))
  )
}

training_data <- raw_data %>%
  filter(CAT == 1)

arch_data <- raw_data %>%
  filter(CAT == 2)

cat("Rows in training data:", nrow(training_data), "\n")
cat("Rows in archaeological data:", nrow(arch_data), "\n")

if (nrow(training_data) == 0) {
  stop("No training rows found. Expected CAT == 1.", call. = FALSE)
}

if (nrow(arch_data) == 0) {
  stop("No archaeological rows found. Expected CAT == 2.", call. = FALSE)
}

###############################################################################
# 3) PREPARE TRAINING DATA
###############################################################################

missing_predictors <- setdiff(manual_predictors, names(training_data))

if (length(missing_predictors) > 0) {
  warning(
    "These manual predictors are absent and will be ignored: ",
    paste(missing_predictors, collapse = ", ")
  )
}

final_predictors <- intersect(manual_predictors, names(training_data))

if (length(final_predictors) == 0) {
  stop("None of the manual predictors were found in the dataset.", call. = FALSE)
}

cat("Final predictors used:\n")
print(final_predictors)

train_df <- training_data %>%
  select(any_of(c("ID", "WORKING_TIME", final_predictors))) %>%
  filter(!is.na(WORKING_TIME))

for (v in final_predictors) {
  train_df[[v]] <- suppressWarnings(as.numeric(train_df[[v]]))
}

train_df <- train_df %>%
  filter(complete.cases(select(., WORKING_TIME, all_of(final_predictors))))

if (nrow(train_df) == 0) {
  stop(
    "No complete training rows after filtering WORKING_TIME and predictors.",
    call. = FALSE
  )
}

x_train <- train_df[, final_predictors, drop = FALSE]
y_train <- train_df$WORKING_TIME

cat("Training rows after complete-case filtering:", nrow(train_df), "\n")

###############################################################################
# 4) FIT RANDOM FOREST MODEL
###############################################################################

ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  savePredictions = "final"
)

mtry_grid <- unique(
  pmax(1, round(seq(2, sqrt(ncol(x_train)), length.out = 5)))
)

rf_model <- caret::train(
  x = x_train,
  y = y_train,
  method = "rf",
  trControl = ctrl,
  tuneGrid = data.frame(mtry = mtry_grid),
  ntree = 1000,
  importance = TRUE,
  metric = "RMSE"
)

cat("\nBest RF model:\n")
print(rf_model)

best_mtry <- rf_model$bestTune$mtry

cv_best <- rf_model$results %>%
  filter(mtry == best_mtry)

cat("\nCross-validated performance for best mtry:\n")
print(cv_best)

rf_importance <- varImp(rf_model)$importance %>%
  rownames_to_column("Variable") %>%
  arrange(desc(Overall))

save_csv(rf_importance, "gob_rf_variable_importance.csv")

###############################################################################
# 5) EXPERIMENTAL HARVESTING MODEL PLOT
###############################################################################

train_pred_hours <- predict(rf_model, newdata = x_train)

df_harvest_plot <- data.frame(
  HOURS_PRED = train_pred_hours,
  STEMS_OBSERVED = y_train * RATE_STEMS_PER_HOUR
)

hours_grid <- seq(
  min(df_harvest_plot$HOURS_PRED, na.rm = TRUE),
  max(df_harvest_plot$HOURS_PRED, na.rm = TRUE),
  length.out = 300
)

df_harvest_line <- data.frame(
  HOURS_PRED = hours_grid,
  STEMS_PRED = hours_grid * RATE_STEMS_PER_HOUR
)

p_harvest_model <- ggplot() +
  geom_point(
    data = df_harvest_plot,
    aes(x = HOURS_PRED, y = STEMS_OBSERVED),
    alpha = 0.22,
    size = 1.6,
    colour = "grey50"
  ) +
  geom_line(
    data = df_harvest_line,
    aes(x = HOURS_PRED, y = STEMS_PRED),
    colour = "tomato",
    linewidth = 1.4
  ) +
  coord_cartesian(
    xlim = range(df_harvest_plot$HOURS_PRED, na.rm = TRUE),
    ylim = range(df_harvest_plot$STEMS_OBSERVED, na.rm = TRUE)
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(margin = ggplot2::margin(t = 8)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 8)),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA)
  ) +
  labs(
    title = "Harvesting model from experimental training data",
    x = "Predicted working time, hours",
    y = "Cumulative harvested stems"
  )

print(p_harvest_model)

save_plot(
  p_harvest_model,
  "gob_experimental_harvesting_model.png",
  width = 7,
  height = 5
)

###############################################################################
# 6) APPLY MODEL TO ARCHAEOLOGICAL DATA
###############################################################################

arch_X <- arch_data[, final_predictors, drop = FALSE]

for (v in final_predictors) {
  arch_X[[v]] <- suppressWarnings(as.numeric(arch_X[[v]]))
}

pred_mask <- complete.cases(arch_X)

cat("\nRows available for prediction:", sum(pred_mask), "\n")
cat("Rows excluded because of missing predictors:", sum(!pred_mask), "\n")

pred_hours_raw <- rep(NA_real_, nrow(arch_X))
pred_hours_raw[pred_mask] <- predict(
  rf_model,
  newdata = arch_X[pred_mask, , drop = FALSE]
)

train_time_min <- min(y_train, na.rm = TRUE)
train_time_max <- max(y_train, na.rm = TRUE)

pred_hours_clamped <- pmin(
  pmax(pred_hours_raw, train_time_min),
  train_time_max
)

df_subareas <- data.frame(
  ID = if ("ID" %in% names(arch_data)) arch_data$ID else seq_len(nrow(arch_data)),
  TOOL = arch_data$TOOL,
  NAME = if ("NAME" %in% names(arch_data)) arch_data$NAME else NA_character_,
  HOURS_PRED = pred_hours_raw,
  HOURS_CLAMPED = pred_hours_clamped,
  STEMS_PRED = pred_hours_raw * RATE_STEMS_PER_HOUR,
  STEMS_CLAMPED = pred_hours_clamped * RATE_STEMS_PER_HOUR
)

cat("\nSubarea-level predictions:\n")
print(head(df_subareas))

save_csv(df_subareas, "gob_subarea_predictions.csv")

###############################################################################
# 7) TOOL-LEVEL AGGREGATION
###############################################################################

df_tools <- summarise_predictions(
  df_subareas,
  group_vars = "TOOL"
)

cat("\nTool-level predictions:\n")
print(df_tools, n = Inf)

save_csv(df_tools, "gob_tool_predictions.csv")

###############################################################################
# 8) TOOL-LEVEL GRAIN WEIGHT ESTIMATES
###############################################################################

df_tool_grain_kg <- df_tools %>%
  mutate(
    GRAIN_G_PER_STEM = GRAIN_G_PER_STEM,

    GRAIN_G_mean = STEMS_mean * GRAIN_G_PER_STEM,
    GRAIN_G_median = STEMS_median * GRAIN_G_PER_STEM,
    GRAIN_G_sd = STEMS_sd * GRAIN_G_PER_STEM,
    GRAIN_G_mad = STEMS_mad * GRAIN_G_PER_STEM,

    GRAIN_KG_mean = GRAIN_G_mean / 1000,
    GRAIN_KG_median = GRAIN_G_median / 1000,
    GRAIN_KG_sd = GRAIN_G_sd / 1000,
    GRAIN_KG_mad = GRAIN_G_mad / 1000
  ) %>%
  select(
    TOOL,
    N_subareas,
    HOURS_mean,
    HOURS_median,
    HOURS_sd,
    HOURS_mad,
    STEMS_mean,
    STEMS_median,
    STEMS_sd,
    STEMS_mad,
    GRAIN_G_PER_STEM,
    GRAIN_G_mean,
    GRAIN_G_median,
    GRAIN_KG_mean,
    GRAIN_KG_median,
    GRAIN_KG_sd,
    GRAIN_KG_mad
  )

cat("\nTool-level grain weight estimates:\n")
print(df_tool_grain_kg, n = Inf)

save_csv(df_tool_grain_kg, "gob_tool_grain_weight_kg.csv")

###############################################################################
# 9) TOTAL ASSEMBLAGE-LEVEL GRAIN OUTPUT
###############################################################################

df_total_grain_kg <- df_tool_grain_kg %>%
  summarise(
    N_tools = n(),

    TOTAL_STEMS_median = sum(STEMS_median, na.rm = TRUE),
    TOTAL_GRAIN_KG_median = sum(GRAIN_KG_median, na.rm = TRUE),

    MEAN_STEMS_per_tool = mean(STEMS_median, na.rm = TRUE),
    MEDIAN_STEMS_per_tool = median(STEMS_median, na.rm = TRUE),
    MIN_STEMS_per_tool = min(STEMS_median, na.rm = TRUE),
    MAX_STEMS_per_tool = max(STEMS_median, na.rm = TRUE),

    MEAN_GRAIN_KG_per_tool = mean(GRAIN_KG_median, na.rm = TRUE),
    MEDIAN_GRAIN_KG_per_tool = median(GRAIN_KG_median, na.rm = TRUE),
    MIN_GRAIN_KG_per_tool = min(GRAIN_KG_median, na.rm = TRUE),
    MAX_GRAIN_KG_per_tool = max(GRAIN_KG_median, na.rm = TRUE)
  )

cat("\nTotal assemblage-level grain output:\n")
print(df_total_grain_kg)

save_csv(df_total_grain_kg, "gob_total_grain_output.csv")

###############################################################################
# 10) INTENSITY AND STEM-COUNT CLASSES
###############################################################################

df_subareas <- df_subareas %>%
  mutate(INTENSITY_CLASS = classify_intensity(HOURS_CLAMPED))

df_tools <- df_tools %>%
  mutate(INTENSITY_CLASS = classify_intensity(HOURS_median))

df_intensity_tools <- df_tools %>%
  filter(!is.na(INTENSITY_CLASS)) %>%
  count(INTENSITY_CLASS, name = "N_tools") %>%
  mutate(
    PROP_tools = N_tools / sum(N_tools),
    PERCENT_tools = PROP_tools * 100
  )

cat("\nTool counts by intensity class:\n")
print(df_intensity_tools)

save_csv(df_intensity_tools, "gob_intensity_classes_tools.csv")


df_stem_classes <- df_tool_grain_kg %>%
  mutate(
    STEM_CLASS = case_when(
      STEMS_median < 50000 ~ "<50,000 stems",
      STEMS_median <= 100000 ~ "50,000-100,000 stems",
      STEMS_median > 100000 ~ ">100,000 stems",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(STEM_CLASS)) %>%
  count(STEM_CLASS, name = "N_tools") %>%
  mutate(
    PROP_tools = N_tools / sum(N_tools),
    PERCENT_tools = PROP_tools * 100
  )

cat("\nHarvested stem-count classes:\n")
print(df_stem_classes)

save_csv(df_stem_classes, "gob_stem_count_classes.csv")

###############################################################################
# 11) PLOTS
###############################################################################

p_intensity <- ggplot(
  df_intensity_tools,
  aes(
    x = INTENSITY_CLASS,
    y = PROP_tools,
    fill = INTENSITY_CLASS
  )
) +
  geom_col() +
  theme_classic(base_size = 12) +
  labs(
    title = "Tool intensity classes",
    x = "Intensity class",
    y = "Proportion of tools"
  )

print(p_intensity)

save_plot(
  p_intensity,
  "gob_intensity_classes_tools.png",
  width = 7,
  height = 5
)


df_tool_output_plot <- df_tool_grain_kg %>%
  mutate(
    STEM_CLASS = case_when(
      STEMS_median < 50000 ~ "<50,000 stems",
      STEMS_median <= 100000 ~ "50,000-100,000 stems",
      STEMS_median > 100000 ~ ">100,000 stems",
      TRUE ~ NA_character_
    ),
    STEM_CLASS = factor(
      STEM_CLASS,
      levels = c("<50,000 stems", "50,000-100,000 stems", ">100,000 stems")
    ),
    TOOL_ORDERED = reorder(TOOL, STEMS_median)
  )

p_tool_output_clean <- ggplot(
  df_tool_output_plot,
  aes(
    x = STEMS_median,
    y = TOOL_ORDERED,
    colour = STEM_CLASS
  )
) +
  geom_errorbarh(
    aes(
      xmin = pmax(STEMS_median - STEMS_sd, 0),
      xmax = STEMS_median + STEMS_sd
    ),
    height = 0.18,
    alpha = 0.45,
    linewidth = 0.45
  ) +
  geom_point(size = 3.1) +
  geom_vline(
    xintercept = c(50000, 100000),
    linetype = "dashed",
    linewidth = 0.45,
    colour = "grey45"
  ) +
  scale_x_continuous(
    name = "Estimated harvested stems, tool median",
    labels = scales::comma,
    sec.axis = sec_axis(
      trans = ~ . * GRAIN_G_PER_STEM / 1000,
      name = "Estimated grain output, kg",
      breaks = seq(0, 35, by = 5)
    )
  ) +
  scale_colour_manual(
    values = c(
      "<50,000 stems" = "#D55E00",
      "50,000-100,000 stems" = "#009E73",
      ">100,000 stems" = "#0072B2"
    )
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title.x.top = element_text(margin = ggplot2::margin(b = 8)),
    axis.title.x.bottom = element_text(margin = ggplot2::margin(t = 8))
  ) +
  labs(
    title = "Tool-level estimated harvested stems and grain output",
    subtitle = "Points show tool medians; horizontal bars show +/-1 SD across analysed subareas",
    y = "Tool",
    colour = "Harvesting-output class"
  )

print(p_tool_output_clean)

save_plot(
  p_tool_output_clean,
  "gob_tool_output_stems_and_kg.png",
  width = 8.5,
  height = 10
)

###############################################################################
# 12) CONTROL CHECKS AND SAVE MODEL
###############################################################################

cat("\nObserved experimental WORKING_TIME range:\n")
print(range(y_train, na.rm = TRUE))

cat("\nPredicted HOURS on archaeological data, raw:\n")
print(summary(df_subareas$HOURS_PRED))

cat("\nPredicted HOURS on archaeological data, clamped:\n")
print(summary(df_subareas$HOURS_CLAMPED))

saveRDS(rf_model, file.path(out_dir, "gob_rf_working_time_model.rds"))

cat("\nFinished.\n")
cat("Main outputs saved in:\n", out_dir, "\n")
cat("\nFinal predictors:\n")
print(final_predictors)
