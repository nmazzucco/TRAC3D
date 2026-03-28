This repository/folder contains the R script, input datasets, and all output files generated for the CERDEV analysis.

## Contents

### Input data
- `Supplementary Material_S1.xlsx`  
  Training dataset used for model construction and calibration.

- `Supplementary Material_S2.xlsx`  
  Test dataset used for external application of the trained model.

### Main R script
- `CERDEV.R`  
  Complete R script used to perform all analyses and generate all output files and figures.

## Output files

### Intermediate analytical outputs
- `1_selected_predictors.csv`  
  List of predictors retained after variance and correlation filtering.

- `2_selected_training_data_clean.csv`  
  Cleaned training dataset after preprocessing and predictor selection.

- `3_breakpoints_segmented_regression.csv`  
  Breakpoints estimated through segmented regression on the wear metric.

- `4_variables_contributions.csv`  
  Contributions of variables to the canonical discriminant functions.

- `5_cross_table_results.csv`  
  Confusion matrix and classification percentages for the training set.

- `6_class_distribution_by_tools.csv`  
  Distribution of predicted classes for each experimental tool.

- `7_dominant_intervals_by_working_time_classes.csv`  
  Dominant predicted wear intervals by working-time class.

- `8_harvested_straw_prediction_tools.csv`  
  Predicted cumulative harvested straw counts for each experimental tool.

- `9_harvested_surface_estimation.csv`  
  Estimated harvested surface area derived from predicted straw counts.

- `10_tool_harvest_conversions.csv`  
  Conversion of harvested straw predictions into estimated grain yields.

- `Harvesting_model_fitted_linear.RDS`  
  Saved fitted linear model used to convert wear signal into cumulative harvested straw counts.

## Figures

- `Figure_1.jpeg`  
  Regression tree used to classify working time through regression-based segmentation.

- `Figure_2.png`  
  Loadings of surface parameters on PC1 used as a composite wear metric.

- `Figure_3.png`  
  Segmented regression and changepoint detection plots for the composite wear metric.

- `Figure_4.png`  
  Standardized temporal evolution of key surface parameters.

- `Figure_5.jpg`  
  Boxplots of predictors by wear-time classes.

- `Figure_6.jpg`  
  Canonical discriminant space of the training dataset.

- `Figure_7.jpg`  
  Projections of experimental tools in discriminant space against the training dataset.

- `Figure_8.png`  
  Comparison of alternative calibration models linking LD1 to cumulative harvested straw counts.

- `Figure_9.png`  
  Final linear harvesting model.

- `Figure_10.png`  
  Final harvesting model with projected experimental tools.

- `Figure_11.png`  
  Combined panel showing the final harvesting model and the projection of experimental tools.

## Folder structure

The script is designed to work from a project directory containing:

```text
project_folder/
├── CERDEV.R
├── README.md
├── data/
│   ├── Supplementary Material_S1.xlsx
│   └── Supplementary Material_S2.xlsx
└── output/
