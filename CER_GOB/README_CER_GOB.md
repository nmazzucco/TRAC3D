# CER_GOB harvesting-intensity model

This folder contains the R pipeline used to estimate harvesting intensity, harvested stem counts, and grain output for the CER_GOB dataset using experimental traceological training data.

The script trains a Random Forest regression model on experimental observations (`CAT == 1`) and applies the model to archaeological observations (`CAT == 2`). Predicted working time is converted into estimated harvested stems and grain weight, then summarised at tool level.

## Folder structure

The analysis is expected to be run from inside the `CER_GOB` folder:

```text
CER_GOB/
├── CER_GOB.R
├── README.md
├── RAWDATA/
│   └── RAWDATA.xlsx
└── OUT/
```

## Main script

Run the pipeline with:

```r
source("CER_GOB.R")
```

or from a terminal:

```bash
Rscript CER_GOB.R
```

The script assumes that the current working directory is `CER_GOB/`. In RStudio, set the working directory to the folder containing `CER_GOB.R` before running the script.

Example:

```r
setwd("path/to/TRAC3D/CER_GOB")
source("CER_GOB.R")
```

## Input data

The expected input file is:

```text
RAWDATA/RAWDATA.xlsx
```

The input table must contain at least the following columns:

| Column | Description |
|---|---|
| `CAT` | Dataset category. `CAT == 1` is used as experimental training data; `CAT == 2` is treated as archaeological data. |
| `WORKING_TIME` | Observed working time in the experimental training data. |
| `TOOL` | Tool identifier used for tool-level aggregation. |

Optional columns used when available:

| Column | Description |
|---|---|
| `ID` | Subarea or observation identifier. If absent, sequential IDs are generated. |
| `NAME` | Optional descriptive label for each observation. |

The model uses the following manually selected surface-texture predictors when they are present in the dataset:

```text
Vmc, Sa, Sk, Vvc, Vvv, Smr1, Smr2, Spk, Svk, Sq, Sz, Vm, Vv, Spc, Spd
```

If some predictors are missing, the script reports a warning and removes them from the model. If none of the selected predictors are present, the script stops.

## Method summary

The pipeline performs the following steps:

1. Loads and cleans the input Excel file.
2. Removes fully empty rows and constant columns.
3. Converts known numeric-like columns to numeric values.
4. Splits the dataset into:
   - experimental training data: `CAT == 1`
   - archaeological data: `CAT == 2`
5. Trains a Random Forest model to predict `WORKING_TIME` from selected surface-texture predictors.
6. Uses repeated 5-fold cross-validation, with 5 repeats, to tune `mtry`.
7. Predicts working time for archaeological observations.
8. Clamps archaeological predictions to the observed experimental working-time range.
9. Converts predicted hours into harvested stems using:

```text
3797.5 stems per hour
```

10. Converts harvested stems into grain weight using:

```text
0.30 g grain per harvested stem
```

11. Aggregates results at tool level.
12. Classifies tools into harvesting-intensity and stem-count classes.
13. Saves CSV outputs, figures, and the trained model in `OUT/`.

## Main outputs

The script writes the following files to `OUT/`:

| Output | Description |
|---|---|
| `gob_rf_variable_importance.csv` | Random Forest variable importance. |
| `gob_subarea_predictions.csv` | Prediction results for individual archaeological observations/subareas. |
| `gob_tool_predictions.csv` | Tool-level working-time and harvested-stem estimates. |
| `gob_tool_grain_weight_kg.csv` | Tool-level grain weight estimates. |
| `gob_total_grain_output.csv` | Assemblage-level grain-output summary. |
| `gob_intensity_classes_tools.csv` | Number and proportion of tools in each working-time intensity class. |
| `gob_stem_count_classes.csv` | Number and proportion of tools in each harvested-stem class. |
| `gob_experimental_harvesting_model.png` | Experimental harvesting model plot. |
| `gob_intensity_classes_tools.png` | Bar plot of tool intensity classes. |
| `gob_tool_output_stems_and_kg.png` | Tool-level harvested-stem and grain-output plot. |
| `gob_rf_working_time_model.rds` | Saved Random Forest model object. |

## Intensity classes

The script classifies tools by median predicted working time:

| Class | Definition |
|---|---|
| `low` | `HOURS_median <= 8` |
| `mid` | `8 < HOURS_median <= 18` |
| `high` | `18 < HOURS_median <= 28` |
| `very high` | `HOURS_median > 28` |

## Stem-count classes

Tools are also classified by median estimated harvested stems:

| Class | Definition |
|---|---|
| `<50,000 stems` | `STEMS_median < 50000` |
| `50,000-100,000 stems` | `50000 <= STEMS_median <= 100000` |
| `>100,000 stems` | `STEMS_median > 100000` |

## R packages

The script requires the following R packages:

```r
install.packages(c(
  "readxl",
  "dplyr",
  "ggplot2",
  "caret",
  "randomForest",
  "tibble",
  "scales"
))
```

## Notes on reproducibility

The script uses:

```r
set.seed(123)
```

This improves reproducibility of the cross-validation and Random Forest results. Minor differences may still occur across R versions, operating systems, or package versions.

## Data and outputs

Depending on repository policy, raw data and generated outputs may be excluded from version control. If so, keep the following folders locally but do not commit their contents:

```text
RAWDATA/
OUT/
```

A typical `.gitignore` entry would be:

```gitignore
CER_GOB/RAWDATA/*.xlsx
CER_GOB/OUT/*
!CER_GOB/OUT/.gitkeep
```

## Citation / authorship

This script is part of the TRAC3D analytical workflow for estimating harvesting intensity from surface-texture data. When reusing or adapting the workflow, cite the relevant TRAC3D project outputs and associated publications.
