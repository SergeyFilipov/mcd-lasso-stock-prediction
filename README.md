# 📈 McDonald's Stock Direction Prediction with LASSO in R

This project predicts whether McDonald's (MCD) stock price will go **up** or **down** in the following quarter using **quarterly financial statements**, **macroeconomic indicators**, and **adjusted stock prices**. The model is based on **LASSO logistic regression**, estimated with multiple lambda values. The entire workflow is built in **R**, integrating data via the SimFin API and a local macro dataset.

---

## 🎯 Goal

- Forecast **stock direction (Up/Down)** one quarter ahead.
- Use only **public and explainable** financial and macroeconomic inputs.
- Build a robust predictive pipeline with visualization and accuracy metrics.

---

## 📦 Data Sources

- 📉 McDonald's quarterly **P&L and balance sheet** data from SimFin API.
- 💹 Historical **adjusted closing prices** (quarterly average).
- 🌍 Quarterly macroeconomic indicators (inflation, unemployment, GDP growth).

---

## 🔍 Modeling Approach

- Merge and preprocess financial, macro, and price data (2018–2025 Q1).
- Engineer lagged price features and previous direction.
- Create a binary target (1 = Down, 0 = Up).
- Estimate LASSO logistic regression for 10 values of lambda (0.01–0.10).
- Evaluate accuracy, precision, recall, and F1 on training data.
- Predict for 2025 Q2 and visualize key outputs.

---

## 📊 Results

- **Best λ = 0.02**
- **Prediction for 2025 Q2:**  
  📈 **UP (growth)** with **66.33%** implied probability  
  📉 Probability of decline: **33.67%**

- **Confusion Matrix Metrics (Train Set):**  
  - Accuracy: **92.6%**  
  - Precision: **1.000**  
  - Recall: **0.800**  
  - F1-score: **0.889**

- **Direction Accuracy (based on price change):**  
  **57.7%** over historical quarters

---

## 📂 Project Structure

```
├── Plots/                         # Output visualizations
├── cache/                         # SimFin cached data
├── integrated_data.csv            # Final dataset used for modeling
├── macro_data.csv                 # Macroeconomic input (CSV)
├── lasso_lambda_coefficients.xlsx # Variable importance for 10 lambdas
├── demodata.RData                 # SimFin raw data (optional)
├── mcd_lasso_model.R              # Main R analysis script
└── README.md
```

---

## ▶️ How to Run

1. Install R packages:
```r
install.packages(c("simfinapi", "glmnet", "caret", "ggplot2", "xgboost", "randomForest",
                   "e1071", "pROC", "PRROC", "openxlsx", "imputeTS", "readr", "patchwork"))
```

2. Set your SimFin API key:
```r
sfa_set_api_key("your-api-key-here")
```

3. Run the script:
```r
source("mcd_lasso_model.R")
```

---

## 📁 Key Outputs (Plots/)

- `01_adjusted_closing_price.png` – Historical adjusted price
- `02_absolute_weights_lambda_0.01_and_0.03.png` – LASSO variable importance
- `03.1_..._estimated_probabilities_lambda_0.01_and_0.03.png` – Probabilities + errors
- `04_final_model_coefficients.png` – Final model coefficients (λ = 0.02)
- `05_roc_curve.png` – ROC curve (AUC = 1)

---

## 🧪 Other Models

Random Forest, XGBoost, and SVM were tested for comparison only. Their performance is **not central to the analysis**.

---

## 📌 Notes

- Last available quarter (2025_Q1) is excluded from training.
- Input features are quarterly and fully explainable.
- Cache and intermediate files are ignored via `.gitignore`.

---

## 📄 License

This project is open for educational and research purposes. Attribution is appreciated.
