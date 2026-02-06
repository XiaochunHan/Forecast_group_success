# Prefrontal Inter-Brain Coupling Forecasts Group Success in Intergroup Conflict

This repository contains the data and analysis code for the study:  
**"Prefrontal inter-brain coupling forecasts group success in intergroup conflict."**  
The research demonstrates that **inter-brain coupling**—rather than intra-brain activity—predicts group success in attacker–defender contests.

---

## 📁 Repository Structure

- **`/Fig{Number}_{Description}/`**  
  Each figure-related folder (e.g., `Fig2_accurate_neural_prediction`) contains:
  - `.Rmd` – R Markdown file with analysis and plotting code
  - `.html` – Knitted output for reference
  - `.Rdata` – Source data or large results (e.g., 5000 permutation outputs)

- **`functions_library.R`**  
  Custom R functions used across all analyses.

---

## 🛠️ Dependencies

Before running the code, install the required R packages.  
You can install them using:

```r
install.packages(c(
  "e1071",        # SVM implementation
  "caret",        # Cross-validation utilities
  "ggplot2",      # Plotting
))
```

*Note: Additional packages may be required per specific analysis; see the `load_packages()` calls in each `.Rmd` file.*

---

## 🚀 Getting Started

### 1. Clone the repository
```bash
git clone https://github.com/XiaochunHan/Forecast_group_success.git
cd Forecast_group_success
```

### 2. Open an RMarkdown file
For example, to reproduce Fig2A_All_features_ROC:
```{r svm_invest_all}
auc_all = auc_boot_cv(data_all_invest,5,"radial",1000,TRUE,'#000000')
```

### 3. View outputs
- The `.html` files provide formatted reports with code, results, and figures.
- Data for each figure are stored in the corresponding `.Rdata` file.

---

## 📊 Data Availability

- Processed data for each analysis are included in the `.Rdata` files.
- All data are anonymized and comply with institutional review board guidelines.

---

## 📄 Code Availability

- All custom functions are in `functions_library.R`.
- Analysis pipelines are fully documented in the `.Rmd` files.

---
**Last updated:** February 2026  
