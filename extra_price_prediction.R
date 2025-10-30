# Load and Inspect the dataset
# Load dataset
data <- read.csv("taco_sales_(2024-2025).csv", stringsAsFactors = TRUE)

#View first few rows
head(data)

#Check structure of dataset
str(data)

#Check dimensions
dim(data)

#Summary statistics
summary(data)


#Count missing values in each column
colSums(is.na(data))

#Detect outliers with IQR 
#loop through each numeric column
for(col_name in names(numeric_data)){
  #get current column values
  values <- numeric_data[[col_name]]
  #calculate Q1 lower quantile, Q3 upper quantile and IQR
  Q1 <- quantile(values, 0.25)
  Q3 <- quantile(values, 0.75)
  IQR_val <- Q3-Q1
  #Define lower and upper bounds 
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  #count outliers
  outlier_values <- values[values < lower_bound | values > upper_bound]
  outlier_count <- sum(values < lower_bound | values > upper_bound)
  #Print actual outlier values
  if (outlier_count > 0) {
    cat("Outlier values in", col_name, ":", outlier_values, "\n")
  }
  #cap outliers to bounds
  values[values < lower_bound] <- lower_bound
  values[values > upper_bound] <- upper_bound
  #update data
  data[[col_name]] <- values
  #print results
  cat("Outliers in", col_name, ":", outlier_count, "\n")
}
# Reload the original dataset from CSV
original_data <- read.csv("taco_sales_(2024-2025).csv", stringsAsFactors = TRUE)

par(mfrow = c(1, 2))  # 1 satır, 2 grafik
# Original
boxplot(original_data$Tip....,
        main = "Before Capping",
        ylab = "Tip ($)",
        col = "orange",
        outline = TRUE)
# Capped data
boxplot(data$Tip....,
        main = "After Capping",
        ylab = "Tip ($)",
        col = "lightblue",
        outline = TRUE)


# FULL CORRELATION MATRIX FOR ALL VARIABLE TYPES
library(ggcorrplot)
library(dplyr)

create_mixed_correlation_matrix <- function(data) {
  # Select NUMERIC variables (excluding ID and target variables)
  numeric_data <- data %>% 
    select(where(is.numeric)) %>%  # Select all numeric columns
    select(-Order.ID)   # Only exclude numeric columns here
  
  # Convert CATEGORICAL & BOOLEAN variables to numeric (excluding temporal variables)
  categorical_data <- data %>% 
    # Select factor, character and logical columns
    select(where(~ is.factor(.) | is.character(.) | is.logical(.))) %>%  
    # Exclude the temporal columns that shouldn't be included
    select(-Order.Time, -Delivery.Time) %>%
    # Convert all to numeric (factors become ordinal, booleans become 0/1)
    mutate(across(everything(), ~ as.numeric(factor(.)))) 
  
  # Combine numeric and encoded categorical data
  combined_data <- bind_cols(numeric_data, categorical_data)
  
  # Calculate correlation matrix (pairwise complete observations)
  cor_matrix <- cor(combined_data, use = "complete.obs")
  
  # Create visualization
  ggcorrplot(cor_matrix,
             method = "circle",    # Circle size represents correlation strength
             type = "upper",       # Show only upper triangle
             colors = c("#6D9EC1", "white", "#E46726"),  # Blue-white-orange gradient
             lab = TRUE,           # Show correlation values
             lab_size = 3,         # Size of correlation values
             tl.cex = 8) +         # Size of variable names
    labs(title = "Comprehensive Correlation Matrix",
         subtitle = "Including Numeric, Categorical and Boolean Variables") +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 10))
}

# Execute the function
create_mixed_correlation_matrix(data)

#PREPARING MODEL FOR PREDICTION(FIT THE MODEL)
library(ggplot2)
library(lattice)
library(caret)
library(dplyr)

# Convert Boolean (TRUE -> 1, FALSE -> 0)
data$Weekend.Order <- as.numeric(data$Weekend.Order)

# Target Variable (Tip)
target <- "Price...."

# Feature Selection
features <- c("Toppings.Count")
#features <- c("Delivery.Duration..min.", "Toppings.Count", 
#              "Weekend.Order","Tip....")

# Data of model
model_data <- data[, c(features, target)]

# One-hot encoding (caret::dummyVars)
dummies <- dummyVars(Price.... ~ ., data = model_data)
model_ready <- data.frame(predict(dummies, newdata = model_data))
model_ready$Price.... <- model_data$Price....

# Check
str(model_ready)
# MODEL 1 : LINEAR REGRESSION
library(caret)
# Number of folds
set.seed(123)
nfolds <- 5
folds <- createFolds(model_ready$Price...., k = nfolds)

# Vector to store RMSE and R square values 
rmse_values <- numeric(nfolds)
r2_values <- numeric(nfolds)

for (i in 1:nfolds) {
  # Division into Train/test 
  test_indexes <- folds[[i]]
  test_data <- model_ready[test_indexes, ]
  train_data <- model_ready[-test_indexes, ]
  
  # Train model
  model_lm <- lm(Price.... ~ ., data = train_data)
  
  # Predict
  predictions <- predict(model_lm, newdata = test_data)
  
  # Compute prediction accuracy metrics: (RMSE) and R square
  rmse_values[i] <- RMSE(predictions, test_data$Price....)
  r2_values[i] <- cor(predictions, test_data$Price....)^2
}
# Print results
cat("Linear Regression Performance:\n",
    "RMSE values (each fold):", round(rmse_values, 4), "\n",
    "Average RMSE:", round(mean(rmse_values), 4), "\n",
    "R-squared values (each fold):", round(r2_values, 4), "\n",
    "Average R-squared:", round(mean(r2_values), 4), "\n")

# Train final model on full data
final_model <- lm(Price.... ~ ., data = model_ready)

# Model summary
cat("\nFinal Model Summary:\n")
summary(final_model)

# Diagnostic plots
# Before plotting, adjust the margins
par(mar = c(4, 4, 2, 1))  # Bottom, Left, Top, Right margins
par(mfrow = c(2, 2))
plot(final_model)

# Actual vs Predicted plot
plot(predict(final_model), model_ready$Price....,
     main = "Actual vs Predicted Values",
     xlab = "Predicted Prices", ylab = "Actual Prices")
abline(0, 1, col = "red")  # Perfect prediction line


# MODEL 2 : DECISION TREE REGRESSION
library(rpart)
# preperation for 5-fold cross-validation 
set.seed(123)
folds <- createFolds(model_ready$Price...., k = 5, list = TRUE)
rmse_values_dt <- c()
r2_values_dt <- c()
# Loop through every fold
for(i in 1:5){
  test_indexes <- folds[[i]]
  test_data <- model_ready[test_indexes, ]
  train_data <- model_ready[-test_indexes, ]
  # Train model
  dt_model <- rpart(Price.... ~ ., data = train_data, method = "anova")
  # Predict
  predictions <- predict(dt_model, newdata = test_data)
  # Calculate RMSE ve R² 
  rmse <- sqrt(mean((predictions - test_data$Price....)^2))
  ss_total <- sum((test_data$Price.... - mean(test_data$Price....))^2)
  ss_res <- sum((test_data$Price.... - predictions)^2)
  r2 <- 1 - (ss_res / ss_total)
  rmse_values_dt <- c(rmse_values_dt, rmse)
  r2_values_dt <- c(r2_values_dt, r2)
}
# Print results
cat("Decision Tree RMSE values (each fold):", round(rmse_values_dt, 4), "\n")
cat("Average RMSE:", round(mean(rmse_values_dt), 4), "\n")
cat("Decision Tree R² values (each fold):", round(r2_values_dt, 4), "\n")
cat("Average R²:", round(mean(r2_values_dt), 4), "\n")


# MODEL 3 : NEURAL NETWORK
library(nnet)
# Normalize features except Price 
X <- model_ready[, names(model_ready) != "Price...."]
X_scaled <- as.data.frame(scale(X))  # Z-score standardizasyonu

# Add price again
X_scaled$Price.... <- model_ready$Price....

# New data set
model_ready_scaled <- X_scaled
set.seed(42)
folds <- sample(rep(1:5, length.out = nrow(model_ready_scaled)))

rmse_values_nn <- c()
r2_values_nn <- c()

for (k in 1:5) {
  train_data <- model_ready_scaled[folds != k, ]
  test_data  <- model_ready_scaled[folds == k, ]
  
  nn_model <- nnet(Price.... ~ ., data = train_data, size = 5, linout = TRUE, trace = FALSE, maxit = 500)
  
  predictions <- predict(nn_model, newdata = test_data)
  actual <- test_data$Price....
  
  rmse <- sqrt(mean((predictions - actual)^2))
  rmse_values_nn <- c(rmse_values_nn, rmse)
  
  ss_total <- sum((actual - mean(actual))^2)
  ss_res <- sum((actual - predictions)^2)
  r2 <- 1 - (ss_res / ss_total)
  r2_values_nn <- c(r2_values_nn, r2)
}

cat("Neural Network RMSE values (each fold):", round(rmse_values_nn, 4), "\n")
cat("Average RMSE:", round(mean(rmse_values_nn), 4), "\n")
cat("Neural Network R² values (each fold):", round(r2_values_nn, 4), "\n")
cat("Average R²:", round(mean(r2_values_nn), 4), "\n")

# MODEL COMPARISON
library(ggplot2)
library(gridExtra)

# Store results 
results <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Neural Network"),
  Avg_RMSE = c(mean(rmse_values), mean(rmse_values_dt), mean(rmse_values_nn)),
  Avg_R2 = c(mean(r2_values), mean(r2_values_dt), mean(r2_values_nn))
)

# RMSE Compare Graph
p1 <- ggplot(results, aes(x = Model, y = Avg_RMSE, fill = Model)) +
  geom_col() +  
  geom_text(aes(label = round(Avg_RMSE, 4)), vjust = -0.5) +  
  labs(title = "Model RMSE Comparison", 
       y = "Average RMSE",
       x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Tekrarlanan legend'ı kaldır

# R-squared Compare Graph
p2 <- ggplot(results, aes(x = Model, y = Avg_R2, fill = Model)) +
  geom_col() +
  geom_text(aes(label = round(Avg_R2, 4)), vjust = -0.5) +
  labs(title = "Model R² Comparison", 
       y = "Avarage R²",
       x = "") +
  ylim(0, 1) +  # for R²  0-1 interval
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

grid.arrange(p1, p2, ncol = 2)



