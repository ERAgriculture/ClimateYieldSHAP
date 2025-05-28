# ERA Data Analysis

This repository contains scripts and tools for analyzing the Evidence for Resilient Agriculture (ERA) dataset, focusing on crop yield modeling across different Agro-Ecological Zones (AEZs) in Africa.


## Project Structure

- `workflow/` - Contains the main analysis scripts
- `workflow/workflow.R` - Original workflow script with hyperparameter tuning
- `workflow/RF_bootstrap_notune_workflow.R` - Analysis script used for the manuscript with bootstrap resampling
- `workflow/config/` - YAML configuration files for different analysis scenarios
- `workflow/R/` - Helper functions used in the analysis
- `workflow/make_hexbin_grid.R` - Script for producing hexbin SHAP (Partial Dependence) plots for exploring feature-yield relationships
- `workflow/[AEZ_temp]_3x5_ind_legends.R` - Scripts for producing beeswarm SHAP (Partial Dependence) plots for exploring feature-yield relationships with visualization of two-way feature interactions
- `workflow/summary_heat_map.R` - Script for producing a summary heat map of the shape and importance of feature-yield relationships
- `data_preparation/` - Scripts for preparing and processing raw data
- `prepare_ERA_data_from_Clim.R` - Script to process climate files from ERA.Compiled

## Analysis Workflows

### Main Analysis Script (Used for Manuscript)

The primary analysis used for the manuscript is `workflow/RF_bootstrap_notune_workflow.R`. This script performs:
  
1. A comprehensive analysis of agricultural yield data across different AEZs
2. Bootstrap resampling (n=99) to quantify uncertainty in predictions and SHAP values
3. Training of Random Forest models using pre-identified optimal parameters
4. Calculation of model performance metrics (RMSE, R², MAE) on training, test, and out-of-bag samples
5. Generation of SHAP-based feature importance and interaction plots with confidence intervals

Results from this script are saved in:
- `outputs/[experiment_name]/full_results/` - Results from the full dataset model
- `outputs/[experiment_name]/bootstrap_results/` - Results from bootstrap resampling
- `outputs/[experiment_name]/bootstrap_plots/` - Visualizations with uncertainty bands

### Original Workflow Script

The `workflow/workflow.R` script is an earlier version that includes hyperparameter tuning. It performs:
  
1. Data loading and preprocessing
2. Model training with cross-validation
3. Hyperparameter optimization
4. Feature importance calculation
5. SHAP analysis for model interpretation

Results from this script are saved in:
- `model_output/[experiment_name]/tuning/` - Tuning results
- `model_output/[experiment_name]/final_workflows/` - Final trained models
- `outputs/[experiment_name]/` - Analysis results and visualizations

## Configuration

Both workflow scripts use YAML configuration files located in the `workflow/config/` folder. These config files control:
  
  - Data paths and target variables
- Model parameters and types
- Preprocessing settings
- Data splitting configurations
- Feature selection and transformation options

Example config structure:
  ```yaml
data_path: "../data_preparation/outputs/era/data_ERA_Clim_Soil.csv"
target: "yield"
weighted: true
weighted_method: "isqrt"
general_formula: "yield ~ Rain.Days.L.1 + Rain.sum + Rain.sum.Dev.Mean + Tmax.mean + 
  Rain.Days.L.1.Dev.Mean + Rain.Max.RSeq.5 + Tmax.mean.Dev.Mean"

preprocessing:
  imputation_method: "NULL"
lower_quantile: 0.00
upper_quantile: 1.00

splits:
  train_prop: 0.8
cv_folds: 5
seed: 123

models:
  - name: "rf"
type: "rand_forest"
mode: "regression"
engine: "ranger"
tune_params:
  mtry: [3, 5]
min_n: [25, 40, 65, 100]
trees: 300
max.depth: [2, 3, 5, 7]
sample.fraction: [0.5, 0.6, 0.7, 0.8]
recipe_options:
  one_hot: true
standardize: false
normalize: false
transformations: false
```

## Required Libraries

Required libraries for running the analysis are listed in `workflow/requirements.txt`.
  
## Data Preparation

The `data_preparation/prepare_ERA_data_from_Clim.R` script processes climate data from the ERA.Compiled dataset. It performs:
  
1. Loading and filtering of ERA.Compiled data
2. Calculation of climate variables using a the function local_CalcClimate (a modified version of the CalcClimate from the *ERAgON* library)
3. Integration with soil data from ERA_ISDA
4. Processing of long-term and observed climate data
5. Merging climate and soil data with ERA observations
6. Unit conversion and texture classification
7. Quality control and filtering of outliers
8. Export of processed datasets in CSV and RDS formats

The processed data files are saved in `data_preparation/outputs/era/` directory with names like `[experiment].csv`.

## Getting Started

1. Install required R packages (see `workflow/requirements.txt`)
2. Run data preparation script if starting with raw data
3. Adjust configuration in YAML files as needed
4. Run the analysis script using an IDE or from the terminal: `Rscript workflow/RF_bootstrap_notune_workflow.R [experiment_name] [AEZ]`
5. Run the figure creation scripts (`workflow/[AEZ_temp]_3x5_ind_legends.R`, `workflow/make_hexbin_grid.R`)

## Notes

- The analysis supports different Agro-Ecological Zones (AEZs): Warm.Semiarid, Warm.Subhumid, Warm.Humid, Cool.Semiarid, Cool.Subhumid, Cool.Humid
- Bootstrap resampling provides uncertainty quantification in model predictions and feature importance
- The workflow can be customized through the YAML configuration files without changing the scripts except for number of bootstraps (n_bootstraps) 
- To run the analysis from the terminal, some lines of code need to be uncommented in the RF_bootstrap_notune_workflow.R script

## Acknowledgments

This work is supported by the **Alliance of Bioversity & CGIAR**.

![Image](https://github.com/user-attachments/assets/ba7b10a7-2f4e-458a-b416-c3971a03f08e)
