library(simfinapi)
library(lubridate)
library(dplyr)
library(imputeTS)
library(tidyr)
library(digest)
library(glmnet)
library(readxl)
library(caret)
library(readr)
library(ggplot2)
library(patchwork)
library(pROC)
library(PRROC)
library(randomForest)
library(xgboost)
library(e1071)
library(reshape2)
library(openxlsx)


# Block 1: Set Working and Cache Directories
main_dir <- "C:\\Users\\Sergey Filipov\\Desktop\\Проект AI-Финтех\\Проект"
setwd(main_dir)
options(scipen = 999)

# Create 'cache' subdirectory if it doesn't exist
cache_dir <- file.path(main_dir, "cache")
if (!dir.exists(cache_dir)) dir.create(cache_dir)

# Set SimFin API key and cache directory
sfa_set_api_key(api_key = "download your own key")
sfa_set_cache_dir(cache_dir)


# Block 2: Download Financial Data (if TRUE)
if (TRUE) {
  
  # Load company list and extract ID for MCD
  cp <- sfa_load_companies()
  id <- cp[ticker == "MCD", id]
  
  # Load historical share prices for McDonald's
  hh <- sfa_load_shareprices(ticker = "MCD", ratios = TRUE)
  na_percent <- sapply(hh, function(x) mean(is.na(x)))  # Calculate percentage of missing values
  
  # Create table of quarters (2018–2025)
  pp <- data.frame(
    q = rep(c("Q1", "Q2", "Q3", "Q4"), 8),
    year = rep(2018:2025, each = 4)
  )
  
  # Download profit and loss statements
  pl <- list()
  for (i in 1:nrow(pp)) {
    try({
      result <- sfa_load_statements(
        ticker = "MCD",
        statement = "pl",
        period = pp[i, 1],
        fyear = pp[i, 2]
      )
      
      if (!is.null(result) && "template" %in% colnames(result)) {
        pl[[i]] <- result
      } else {
        cat(sprintf("No P&L data for %s %s\n", pp[i, 1], pp[i, 2]))
      }
    }, silent = TRUE)
  }
  
  
  # Block 3: Download balance sheet statements
  bs <- list()
  for (i in 1:nrow(pp)) {
    try({
      result <- sfa_load_statements(
        ticker = "MCD",
        statement = "bs",
        period = pp[i, 1],
        fyear = pp[i, 2]
      )
      
      if (!is.null(result) && "template" %in% colnames(result)) {
        bs[[i]] <- result
      } else {
        cat(sprintf("No balance sheet for %s %s\n", pp[i, 1], pp[i, 2]))
      }
    }, silent = TRUE)
  }
  
  # Save all retrieved data to .RData file
  save.image("demodata.RData")
}

# Load previously saved data (if available)
load("demodata.RData")


# Block 4: Combine and Clean Balance Sheet Data
bsd = do.call(rbind, bs)
bsd = as.data.frame(bsd)
bsd$year_q <- paste(bsd$fiscal_year, bsd$fiscal_period, sep = "_")

# Define keywords for columns to drop
drop_keywords <- c("source", "template", "id", "simfin", "ticker")

# Drop columns containing any of the keywords
cols_to_drop <- grep(paste0(drop_keywords, collapse = "|"), names(bsd), value = TRUE)
if (length(cols_to_drop) > 0) {
  bsd <- bsd[, !names(bsd) %in% cols_to_drop]
}


# Block 5: Merge Profit & Loss Data
pld = do.call(rbind, pl)
pld = do.call(rbind, pl[!sapply(pl, is.null)])

# Rename columns in pp to match those in pld
colnames(pp) = c("fiscal_period", "fiscal_year")

# Merge P&L data with quarter table by fiscal_period and fiscal_year
merged_df = merge(pp, pld, by = c("fiscal_period", "fiscal_year"))

# Add Year-Quarter Identifiers
pld$year_q <- paste(pld$fiscal_year, pld$fiscal_period, sep = "_")
bsd$year_q <- paste(bsd$fiscal_year, bsd$fiscal_period, sep = "_")

# Preprocess P&L Data
pld = as.data.frame(pld)

# Summary of P&L data: column names, classes, and missing values
pld.s = data.frame(nm = names(pld), cl = sapply(pld, class), na = colSums(is.na(pld)))
rownames(pld.s) = c(1:ncol(pld))

# Exclude columns that are entirely missing
n = nrow(pld)
excl = pld.s$nm[which(pld.s$na == n)]
pld = pld[, !names(pld) %in% excl]

# Recalculate summary after exclusion
pld.s = data.frame(nm = names(pld), cl = sapply(pld, class), na = colSums(is.na(pld)))
rownames(pld.s) = c(1:ncol(pld))


# Block 6: Handle Missing and Abnormal Data
# Linearly interpolate values in specific column
pld$other_non_operating_income_loss = na_interpolation(pld$other_non_operating_income_loss, option = "linear")

# Exclude columns with more than 2 missing values
excl = pld.s$nm[which(pld.s$na > 2)]
pld = pld[, !names(pld) %in% excl]

# Load and Transform Macroeconomic Data
csv_path <- "C://Users//Sergey Filipov//Desktop//Проект AI-Финтех//Проект//macro_data.csv"
macro_long <- read_csv(csv_path)
colnames(macro_long) <- tolower(colnames(macro_long))

# Convert from long to wide format and exclude data after Q1 2025
macro_wide <- macro_long %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  filter(date <= as.Date("2025-03-31")) %>%
  select(date, name, value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(year_q = paste0(year(date), "_Q", quarter(date))) %>%
  arrange(date) %>%
  group_by(year_q) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(-date)

# Merge Macroeconomic Data with P&L
pld = left_join(pld, macro_wide, by = "year_q")


# Block 7: Preprocess Historical Stock Price Data
# Aggregate historic data
dd = data.frame(date = hh$Date, adj = hh$"Adjusted Closing Price")
dd$month = month(dd$date)
dd$q = ifelse(dd$month < 4, "Q1", ifelse(dd$month < 7, "Q2", ifelse(dd$month < 10, "Q3", "Q4")))
dd$year = year(dd$date)

# Create merging key
dd$year_q = paste(dd$year, dd$q, sep = "_") 

# Aggregate adjusted prices by quarter
dda = dd %>%
  group_by(year_q) %>%
  summarise(p = mean(adj))

# Derive price direction (Up or Down)
dda$d = ifelse(dda$p < dplyr::lag(dda$p, 1), "Down", "Up")
colnames(dda)[1] = "YQ"

# Create lagged year_q
dda$year_q = dplyr::lag(dda$YQ, 1)

# Remove first row with NA in lag
dda = dda[-1, ]


# Block 8: Integrate All Data Frames
combined_df <- full_join(pld, bsd, by = "year_q", suffix = c("_pl", "_bs"))
combined_df <- full_join(combined_df, macro_wide, by = "year_q")
combined_df <- full_join(combined_df, dda, by = "year_q")
combined_df <- combined_df[, !grepl("^source|template|id|simfin|ticker", names(combined_df))]

# Save integrated data
dd.int <- combined_df
write.csv(dd.int, "integrated_data.csv")

# Copy to dd
dd <- combined_df

# 🔍 Automatically exclude the last available quarter (e.g., 2025_Q2)
last_q <- max(dd$year_q)
cat("📌 Excluding last quarter from training set:", last_q, "\n")

dd  <- dd[dd$year_q != last_q, ]
dda <- dda[dda$year_q != last_q, ]

# 🔧 Merge duplicated variables (.x and .y suffixes)
combine_duplicates <- function(df) {
  colnames_df <- colnames(df)
  suffix_x <- grep("\\.x$", colnames_df, value = TRUE)
  
  for (col_x in suffix_x) {
    col_base <- sub("\\.x$", "", col_x)
    col_y <- paste0(col_base, ".y")
    
    if (col_y %in% colnames_df) {
      df[[col_base]] <- dplyr::coalesce(df[[col_x]], df[[col_y]])
      df[[col_x]] <- NULL
      df[[col_y]] <- NULL
    }
  }
  return(df)
}

dd <- combine_duplicates(dd)


# Block 9: Feature Engineering for LASSO Logistic Regression
# Create lagged price features and lagged direction
dd$lp  <- dplyr::lag(dd$p, 1)
dd$lp2 <- dplyr::lag(dd$p, 2)
dd$lp3 <- dplyr::lag(dd$p, 3)
dd$lp4 <- dplyr::lag(dd$p, 4)
dd$l1direction <- ifelse(dplyr::lag(dd$d, 1) == "Down", 1, 0)

# Remove rows with missing target values
dd <- dd[!is.na(dd$d), ]

# Create binary target variable (1 = Down, 0 = Up)
y <- ifelse(dd$d == "Down", 1, 0)

# Remove unnecessary columns before modeling
dd_clean <- dd %>%
  select(-year_q, -YQ, -p, -d)

# Remove columns with more than 30% missing values and zero variance
dd_clean <- dd_clean[, colSums(is.na(dd_clean)) < 0.3 * nrow(dd_clean)]
dd_clean <- dd_clean[, sapply(dd_clean, function(x) length(unique(x)) > 1)]

# Fill missing numeric values using linear interpolation
dd_clean <- dd_clean %>% mutate(across(where(is.numeric), ~ na_interpolation(., option = "linear")))


# Block 10: Prepare Data Matrix for LASSO
# Create predictor matrix and scale it
x <- model.matrix(~ ., data = dd_clean)[, -1]
x <- scale(x)

# Filter out specific variables if needed
exclude = c("ttmTRUE", "value_checkTRUE", "data_model")
x = scale(x[, !(colnames(x) %in% exclude)], center = TRUE, scale = TRUE)
y = ifelse(dd$d == "Down", 1, 0)


# Block 11: Fit LASSO Logistic Regression ----
lambda = seq(from = 0.01, to = 0.10, by = 0.01)  # Create a grid of lambda values
eq = list()
for (i in 1:10){
  eq[[i]] = glmnet::glmnet(x, y, alpha = 1, lambda = lambda[i], standardize = FALSE, family = "binomial")
}
coef(eq[[i]])

# Extract estimated coefficients for each lambda
a = list()
for (i in 1:10){
  coef_i <- coef(eq[[i]])
  nonzero <- which(abs(coef_i) > 0)
  
  feature_names <- dimnames(coef_i)[[1]][nonzero]      # Get actual feature names
  weights_values <- as.numeric(coef_i[nonzero])        # Convert coefficients to numeric
  
  a[[i]] <- data.frame(
    features = feature_names,
    weights = weights_values,
    stringsAsFactors = FALSE
  )
}


# Block 12: Rank predictors by absolute coefficient magnitude
b = list()  # List of coefficients for each lambda

for (i in 1:10){
  b[[i]] = data.frame(
    features = a[[i]]$features,
    weights = round(a[[i]]$weights, 4),                 # Actual (signed) weights
    abs.weights = round(abs(a[[i]]$weights), 4),        # Absolute weights
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(abs.weights))                          # Sort by importance
}

# Remove intercept term
for (i in 1:10){
  b[[i]] = b[[i]][b[[i]]$features != "(Intercept)", ]
}
names(b) = paste("lambda", lambda, sep = "_")

# Export results to Excel
excel_file <- "lasso_lambda_coefficients.xlsx"
wb <- createWorkbook()

for (i in 1:length(b)) {
  df <- b[[i]]
  sheet_name <- paste0("lambda_", format(lambda[i], nsmall = 2))
  addWorksheet(wb, sheetName = sheet_name)
  writeData(wb, sheet = sheet_name, x = df)
}

saveWorkbook(wb, file = excel_file, overwrite = TRUE)
cat("xcel file with LASSO coefficients created:", excel_file, "\n")

# Calculate predicted probabilities for each lambda
phat = list()
for (i in 1:10) {
  phat[[i]] = predict(eq[[i]], newx = x, type = "response")
}


# Block 13: Create Actual vs. Predicted Probability Table
aux = list()
for (i in 1:10){
  aux[[i]] = data.frame(
    direction = ifelse(y == 1, "Down", "Up"),
    estimated.prob = round(as.numeric(phat[[i]]), 2)
  )
}
names(aux) = paste("lambda", lambda, sep = "_")

# Format for export/view
probs = as.data.frame(aux)
# probs = probs[, -seq(from = 3, to = 19, by = 2)]  # Remove redundant columns
colnames(probs) = gsub(".estimated.prob", "", colnames(probs))
colnames(probs)[1] = "Actual.Direction"


# Block 14: Plot Adjusted Closing Price (Base R)
tiff("01_adjusted_closing_price.png", res = 300, compression = "lzw",
     bg = "white", height = 6, width = 9, units = "in")

# Set graphical parameters
par(mar = c(6, 5, 3, 2), cex.main = 0.9, cex.axis = 0.8)

# Plot the adjusted price
plot(dda$p,
     pch = 19,
     col = "blue",
     main = "Adjusted closing price",
     axes = FALSE,
     xlab = "", ylab = "")

# X-axis labels – quarters
axis_labels <- dd$YQ[1:length(dda$p)]
axis(side = 1, at = 1:length(dda$p), labels = axis_labels, las = 2)

# Y-axis labels – rounded every 5 units
axis_ticks_y <- seq(from = round(min(dda$p), 0),
                    to = round(max(dda$p), 0), by = 5)
axis(side = 2, at = axis_ticks_y, labels = axis_ticks_y, las = 1)

# Add grid and border
box()
abline(h = axis_ticks_y, v = 1:length(dda$p), col = "gray", lty = 3)

# Save the figure
dev.off()


# Block 15: Barplot of Coefficients (Base R) — Safe TIFF Output Without windows()
tiff("02_absolute_weights_lambda_0.01_and_0.03.png", res = 300, compression = "lzw", bg = "white", height = 6, width = 12, units = "in")

par(mar = c(15, 5, 2, 2), cex.axis = 0.75, cex.main = 0.8, mfrow = c(1, 2))

barplot(b[[1]]$abs.weights,
        names.arg = b[[1]]$features,
        las = 2, col = "light blue",
        main = "Absolute weights, lambda = 0.01")

barplot(b[[3]]$abs.weights,
        names.arg = b[[3]]$features,
        las = 2, col = "light blue",
        main = "Absolute weights, lambda = 0.03")

dev.off()

# Prepare Auxiliary Vectors to Mark Prediction Errors for all lambdas (0.01 to 0.10)
a01 = ifelse(ifelse(probs$lambda_0.01 > 0.5, "Down", "Up") == probs$Actual.Direction, NA, probs$lambda_0.01)
a02 = ifelse(ifelse(probs$lambda_0.02 > 0.5, "Down", "Up") == probs$Actual.Direction, NA, probs$lambda_0.02)
a03 = ifelse(ifelse(probs$lambda_0.03 > 0.5, "Down", "Up") == probs$Actual.Direction, NA, probs$lambda_0.03)
a04 = ifelse(ifelse(probs$lambda_0.04 > 0.5, "Down", "Up") == probs$Actual.Direction, NA, probs$lambda_0.04)
a05 = ifelse(ifelse(probs$lambda_0.05 > 0.5, "Down", "Up") == probs$Actual.Direction, NA, probs$lambda_0.05)
a06 = ifelse(ifelse(probs$lambda_0.06 > 0.5, "Down", "Up") == probs$Actual.Direction, NA, probs$lambda_0.06)
a07 = ifelse(ifelse(probs$lambda_0.07 > 0.5, "Down", "Up") == probs$Actual.Direction, NA, probs$lambda_0.07)
a08 = ifelse(ifelse(probs$lambda_0.08 > 0.5, "Down", "Up") == probs$Actual.Direction, NA, probs$lambda_0.08)
a09 = ifelse(ifelse(probs$lambda_0.09 > 0.5, "Down", "Up") == probs$Actual.Direction, NA, probs$lambda_0.09)
a10 = ifelse(ifelse(probs$lambda_0.10 > 0.5, "Down", "Up") == probs$Actual.Direction, NA, probs$lambda_0.10)


# Block 16: Plot Estimated Probabilities and Highlight Errors (ggplot2)
# Lambda = 0.01
df01 <- data.frame(
  year_q = dd$YQ[1:nrow(probs)],
  prob = probs$lambda_0.01,
  actual = probs$Actual.Direction
)
df01$error <- ifelse(ifelse(df01$prob > 0.5, "Down", "Up") != df01$actual, TRUE, FALSE)

p1 <- ggplot(df01, aes(x = factor(year_q, levels = year_q), y = prob)) +
  geom_point(aes(color = error), size = 2) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  labs(title = "Estimated probability, lambda = 0.01", x = "", y = "Probability") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Lambda = 0.03
df03 <- data.frame(
  year_q = dd$YQ[1:nrow(probs)],
  prob = probs$lambda_0.03,
  actual = probs$lambda_0.03.direction
)
df03$error <- ifelse(ifelse(df03$prob > 0.5, "Down", "Up") != df03$actual, TRUE, FALSE)

p3 <- ggplot(df03, aes(x = factor(year_q, levels = year_q), y = prob)) +
  geom_point(aes(color = error), size = 2) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  labs(title = "Estimated probability, lambda = 0.03", x = "", y = "Probability") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Combine and export
combined <- p1 + p3 + plot_layout(ncol = 2)
ggsave("03.1_estimated_probabilities_lambda_0.01_and_0.03.png", combined, width = 12, height = 6, dpi = 300)

# Lambda = 0.02
df02 <- data.frame(
  year_q = dd$YQ[1:nrow(probs)],
  prob = probs$lambda_0.02,
  actual = probs$lambda_0.02.direction
)
df02$error <- ifelse(ifelse(df02$prob > 0.5, "Down", "Up") != df02$actual, TRUE, FALSE)

p2 <- ggplot(df02, aes(x = factor(year_q, levels = year_q), y = prob)) +
  geom_point(aes(color = error), size = 2) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  labs(title = "Estimated probability, lambda = 0.02", x = "", y = "Probability") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Lambda = 0.04
df04 <- data.frame(
  year_q = dd$YQ[1:nrow(probs)],
  prob = probs$lambda_0.04,
  actual = probs$lambda_0.04.direction
)
df04$error <- ifelse(ifelse(df04$prob > 0.5, "Down", "Up") != df04$actual, TRUE, FALSE)

p4 <- ggplot(df04, aes(x = factor(year_q, levels = year_q), y = prob)) +
  geom_point(aes(color = error), size = 2) +
  scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  labs(title = "Estimated probability, lambda = 0.04", x = "", y = "Probability") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Combine and export to file
combined <- p2 + p4 + plot_layout(ncol = 2)
ggsave("03.2_estimated_probabilities_lambda_0.02_and_0.04.png", combined, width = 12, height = 6, dpi = 300)


# Block 17: Finalize Prediction Table (Rebuild aux for completeness)
aux[[i]] = data.frame(
  direction = ifelse(y == 1, "Down", "Up"),
  estimated.prob = round(as.numeric(phat[[i]]), 2)
)
probs = as.data.frame(aux)


# Block 18: Predict for 2025_Q2 Using Best Lambda Model
best_lambda <- 0.02
final_model <- glmnet(x, y, alpha = 1, lambda = best_lambda, family = "binomial")

# Prepare predictor values from 2025_Q1
accounting_q1_2025 = dd.int %>%
  filter(year_q == "2025_Q1") %>%
  select(-year_q, -YQ, -d, -p) 

# Extract last 4 quarters of price data
last_p = tail(dda$p, 4)
new_obs = data.frame(
  lp  = last_p[4],
  lp2 = last_p[3],
  lp3 = last_p[2],
  lp4 = last_p[1],
  l1direction = ifelse(last_p[4] < last_p[3], 1, 0)  # Determine direction Q1 vs Q4
)

# Match columns between predictors and training matrix
accounting_vars = intersect(colnames(accounting_q1_2025), colnames(x))
predictor_row = cbind(accounting_q1_2025[, accounting_vars], new_obs)

# Keep only numeric, non-missing columns
predictor_row_clean = predictor_row[, sapply(predictor_row, function(col) {
  is.numeric(col) && !any(is.na(col))
})]

# Create model matrix and align with training feature order
x_new <- model.matrix(~ ., data = predictor_row_clean)[, -1]

# Add any missing columns with zeros to match model input structure
missing_cols = setdiff(colnames(x), colnames(x_new))
for (col in missing_cols) {
  x_new = cbind(x_new, setNames(data.frame(0), col))
}
x_new = x_new[, colnames(x)]

# Scale using training mean and sd
x_new_scaled <- scale(x_new,
                      center = attr(x, "scaled:center"),
                      scale  = attr(x, "scaled:scale"))

# Predict probability of DOWN
prob_best = predict(final_model, newx = x_new_scaled, type = "response")[1]

# Print full prediction summary
direction <- ifelse(prob_best > 0.5, "📉 Prediction with best λ: DOWN (decline)", 
                    "📈 Prediction with best λ: UP (growth)")

cat(direction, "\n")
cat(sprintf("📊 Probability of decline: %.2f%%\n", prob_best * 100))
cat(sprintf("📈 Implied probability of growth: %.2f%%\n", (1 - prob_best) * 100))


# Block 19: Extract and Save Final Model Coefficients Plot (PNG)
# Extract coefficients from the final model
coef_best <- coef(final_model)

# Identify non-zero coefficients (excluding Intercept)
nonzero <- which(abs(coef_best) > 0)
features <- rownames(coef_best)[nonzero]
weights <- abs(as.numeric(coef_best[nonzero]))

# Remove "(Intercept)" if present
keep <- features != "(Intercept)"
features <- features[keep]
weights <- weights[keep]

# Sort features by absolute weight (descending)
order_index <- order(weights, decreasing = TRUE)
features <- features[order_index]
weights <- round(weights[order_index], 2)

# Save plot to working directory
png("04_final_model_coefficients.png", width = 1000, height = 900, res = 120)

# Barplot with value labels above the bars
bars <- barplot(weights,
                names.arg = features,
                las = 1,
                col = "light blue",
                main = paste("Absolute weights, lambda =", round(best_lambda, 3)))

# Add numeric values above each bar
text(x = bars,
     y = weights,
     labels = round(weights, 2),
     pos = 3,           # position above the column
     cex = 0.8,         # text size
     col = "black")     # text color

dev.off()

# View final model coefficients
# coef(final_model)


# Block 20: Calculate and plot ROC curve on the full sample
phat_full <- predict(eq[[which.min(lambda)]], newx = x, type = "response")

roc_dir_less <- roc(y, as.numeric(phat_full), direction = "<")
roc_dir_greater <- roc(y, as.numeric(phat_full), direction = ">")

cat("AUC for direction '<':", auc(roc_dir_less), "\n")
cat("AUC for direction '>':", auc(roc_dir_greater), "\n")

# Select the ROC curve with higher AUC (usually the more logical one)
roc_full <- if (auc(roc_dir_less) >= auc(roc_dir_greater)) roc_dir_less else roc_dir_greater

# Save ROC curve plot
png("05_roc_curve.png", width = 800, height = 600)
plot(roc_full, main = "ROC Curve - Full Sample")
dev.off()

# Precision-Recall curve
fg <- phat_full[y == 1]
bg <- phat_full[y == 0]
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = TRUE)

cat("AUC-PR:", pr$auc.integral, "\n")

# Predicted classes for each lambda (0 = Up, 1 = Down)
yhat = list()
for (i in 1:10) {
  yhat[[i]] = ifelse(phat[[i]] > 0.5, 1, 0)
}


# Block 21: Calculate Accuracy and Classification Metrics for Best Lambda Model
# Assume 'best_lambda' is already selected and 'eq' contains the fitted models
best_lambda_index <- which(lambda == best_lambda)

# Get class predictions (0/1) for the best lambda model
yhat_best <- yhat[[best_lambda_index]]

# Calculate accuracy as the proportion of correct predictions
accuracy_best <- sum(y == yhat_best) / length(y)
cat("Accuracy:", round(accuracy_best * 100, 2), "%\n")

# Convert true and predicted labels to factors with explicit levels
y_factor <- factor(y, levels = c(0, 1))
yhat_factor <- factor(yhat_best, levels = c(0, 1))

# Compute confusion matrix with positive class set to "1"
conf_mat <- confusionMatrix(yhat_factor, y_factor, positive = "1")

# Print the confusion matrix and associated statistics
print(conf_mat)

# Extract precision, recall, and F1-score directly from confusion matrix statistics
precision <- conf_mat$byClass["Precision"]
recall <- conf_mat$byClass["Recall"]
f1 <- conf_mat$byClass["F1"]

# Print precision, recall, and F1-score
cat(sprintf("Precision: %.3f\nRecall: %.3f\nF1-score: %.3f\n", precision, recall, f1))


# Block 22: Compare Real Prices with Predicted Probabilities, Calculate Direction Accuracy, and Visualize Errors
# Create an unscaled matrix from the same predictors
x_unscaled <- model.matrix(~ ., data = dd_clean)[, -1]

# Refit the model using unscaled data with the same lambda
final_model_unscaled <- glmnet(x_unscaled, y, alpha = 1, lambda = best_lambda, family = "binomial")

# Predict probabilities (real, between 0 and 1)
predicted_probs_unscaled <- predict(final_model_unscaled, newx = x_unscaled, type = "response")

# Add predicted probabilities to the main dataset
dd$predicted_prob_down <- as.numeric(predicted_probs_unscaled)

# Extract actual prices by quarter
real_prices <- dda %>% select(year_q, actual_price = p)

# Create comparison dataframe
comparison_df <- dd %>%
  select(year_q, predicted_prob_down) %>%
  inner_join(real_prices, by = "year_q") %>%
  arrange(year_q) %>%
  mutate(
    predicted_direction = ifelse(predicted_prob_down > 0.5, 1, 0),
    actual_direction = ifelse(lead(actual_price) < actual_price, 1, 0),
    error = predicted_direction != actual_direction
  ) %>%
  filter(!is.na(actual_direction))  # remove the last row with NA

# Calculate directional accuracy
accuracy_direction <- mean(comparison_df$predicted_direction == comparison_df$actual_direction)
cat("Direction prediction accuracy:", round(accuracy_direction * 100, 2), "%\n")

# Compute Precision / Recall / F1-score
conf_mat <- confusionMatrix(
  factor(comparison_df$predicted_direction),
  factor(comparison_df$actual_direction),
  positive = "1"
)
precision <- conf_mat$byClass["Precision"]
recall <- conf_mat$byClass["Recall"]
f1 <- conf_mat$byClass["F1"]
cat(sprintf("Precision: %.3f\nRecall: %.3f\nF1-score: %.3f\n", precision, recall, f1))


# Block 23: Compare LASSO with Random Forest, XGBoost, and SVM
set.seed(123)

# Random Forest model
rf_model <- randomForest(x, factor(y), ntree = 100)
rf_pred <- predict(rf_model, x)
rf_cm <- confusionMatrix(rf_pred, factor(y), positive = "1")

# XGBoost model
dtrain <- xgb.DMatrix(data = x, label = y)
params <- list(objective = "binary:logistic", eval_metric = "error")
xgb_model <- xgb.train(params, dtrain, nrounds = 100, verbose = 0)
xgb_pred_prob <- predict(xgb_model, x)
xgb_pred <- ifelse(xgb_pred_prob > 0.5, 1, 0)
xgb_cm <- confusionMatrix(factor(xgb_pred), factor(y), positive = "1")

# SVM with radial kernel
svm_model <- svm(x, factor(y), kernel = "radial", probability = TRUE)
svm_pred <- predict(svm_model, x)
svm_cm <- confusionMatrix(svm_pred, factor(y), positive = "1")

# LASSO (best lambda model)
lasso_pred <- factor(yhat_best, levels = c(0,1))
lasso_cm <- confusionMatrix(lasso_pred, factor(y), positive = "1")

# Function to extract key metrics
get_metrics <- function(cm, model_name) {
  data.frame(
    Model = model_name,
    Accuracy = cm$overall["Accuracy"],
    Precision = cm$byClass["Precision"],
    Recall = cm$byClass["Recall"],
    F1 = cm$byClass["F1"]
  )
}

# Combine results
results <- rbind(
  get_metrics(lasso_cm, "LASSO"),
  get_metrics(rf_cm, "Random Forest"),
  get_metrics(xgb_cm, "XGBoost"),
  get_metrics(svm_cm, "SVM")
)
rownames(results) <- NULL

print(results)