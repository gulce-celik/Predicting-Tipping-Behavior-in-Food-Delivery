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

#Create high_tip target variable (Change of research question not important parts)--------------
#Calculate the median tip value, ignoring missing values
median_tip <- median(data$Tip...., na.rm = TRUE)
#Create binary variable: 1 if tip is greater than median, 0 otherwise
data$high_tip <- ifelse(data$Tip.... > median_tip, 1, 0)
#Display distribution of new target variable
table(data$high_tip) 
library(ggplot2)
#Plot the distribution of high_tip
ggplot(data, aes(x = factor(high_tip), fill = factor(high_tip))) +
  geom_bar() + 
  scale_fill_manual(
    values = c("0" = "lightpink", "1" = "lightgreen")
  ) +
  labs(title = "Distribution of High Tip Classification",
       x = "High Tip (0 = No, 1 = Yes)",
       y= "Number of Orders"
       ) + 
  theme_minimal()
#------------------------------------------------------------------------------------------------

#Count missing values in each column
colSums(is.na(data))

#Visualize Price distribution with boxplot
boxplot(data$Price....,
        main = "Boxplot of Price",
        ylab = "Price ($)",
        col = "purple",
        outline = TRUE)

# Q-Q plot to check normality of Tip distribution
qqnorm(data$Tip...., main = " Q-Q Plot of Tip")
qqline(data$Tip...., col = "red", lwd = 2)

# Q-Q plot to check normality of Pricd distribution
qqnorm(data$Price...., main = " Q-Q Plot of Price")
qqline(data$Price...., col = "red", lwd = 2)

#Calculate mean and standard deviation
mean_price <- mean(data$Price....)
sd_price <- sd(data$Price....)


#Detect OUTLIERS
#Select only numeric columns
numeric_cols <- sapply(data,is.numeric)
numeric_data <- data[, numeric_cols]
numeric_data <- numeric_data[, names(numeric_data) != "high_tip"]
#Set up plotting area
num_plots <- length(numeric_data)
plot_rows <- ceiling(sqrt(num_plots))   
plot_cols <- ceiling(num_plots / plot_rows)  
par(mfrow = c(plot_rows, plot_cols))    
par(mar = c(4, 4, 2, 1))                
#Loop over each numeric column and draw boxplot
for(col_name in names(numeric_data)) {
  boxplot(numeric_data[[col_name]],
          main = paste("Boxplot of", col_name),
          ylab = col_name,
          col = "yellow",
          outline = TRUE)
}

#Detect outliers with Z- score 
for(col_name in names(numeric_data)) {
  #calculate meand and sd for current column
  mean_val <- mean(numeric_data[[col_name]])
  sd_val <- sd(numeric_data[[col_name]])
  z_scores <- (numeric_data[[col_name]]-mean_val) / sd_val
  outlier_count <- sum(abs(z_scores) > 3, na.rm = TRUE)
  cat("Outliers in", col_name, ":", outlier_count, "\n")
}

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

# Reload the original dataset from CSV to compare before and after
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

#Listing CATEGORICAL variables and their distributions
library(ggplot2)
library(gridExtra)
library(dplyr)
# Find categorical columns
cat_cols <- names(data)[sapply(data, is.factor)]
# Create list to store plots
plot_list <- list()
for (col in cat_cols) {
  # If category number is less than 15, draw plot for meaningful visualization
  if (n_distinct(data[[col]]) <= 15) {
    p <- ggplot(data, aes(x = .data[[col]])) +
      geom_bar(fill = "royalblue") +
      labs(title = paste("Distribution of", col),
           x = col,
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plot_list[[col]] <- p
  }
}
# Display only if there are plots
if (length(plot_list) > 0) {
  grid.arrange(grobs = plot_list, ncol = 2)
} else {
  cat("No categorical variable with ≤15 unique values for barplot.\n")
}



# Define HetGini function to calculate Gini Index
HetGini <- function(x) {
  freq <- prop.table(table(x))       # Compute relative frequencies
  1 - sum(freq^2)                    # Gini Index formula
}
# Select available original CATEGORICAL columns
cat_cols <- c("Restaurant.Name", "Taco.Type", "Taco.Size", "Location")
# Loop through each categorical column and calculate Gini Index
for (col in cat_cols) {
  gini_val <- HetGini(data[[col]])  # Apply Gini function to column
  cat("Gini Index for", col, ":", round(gini_val, 4), "\n")
}

#CORRELATION MATRIX FOR NUMERICAL FEATURES
# Load library for correlation plot
library(corrplot)

# Select only numeric columns
num_cols <- names(data)[sapply(data, is.numeric)]

# Exclude ID columns if necessary
num_cols <- setdiff(num_cols, c("Order.ID"))

# Subset numeric data
numeric_data <- data[, num_cols]

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Visualize the correlation matrix
corrplot(cor_matrix,
         method = "color",       # colored tiles
         type = "upper",         # only upper triangle
         order = "hclust",       # clustered variables
         tl.cex = 0.7,           # text size
         number.cex = 0.7,       
         addCoef.col = "black",  # add correlation values
         tl.col = "black",
         title = "Correlation Matrix for Numerical Features",
         mar = c(0, 0, 1, 0))       # axis text color


#Series of BOXPLOTS to analyze how different CATEGORICAL variables affect TIP amounts
library(ggplot2)
library(gridExtra)

# 1. Taco.Type vs Tip
plot1 <- ggplot(data, aes(x = Taco.Type, y = Tip....)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Tip by Taco Type", x = "Taco Type", y = "Tip ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Taco.Size
plot2 <- ggplot(data, aes(x = Taco.Size, y = Tip....)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Tip by Taco Size", x = "Taco Size", y = "Tip ($)") +
  theme_minimal()

# 3. Location
plot3 <- ggplot(data, aes(x = Location, y = Tip....)) +
  geom_boxplot(fill = "lightpink") +
  labs(title = "Tip by Location", x = "Location", y = "Tip ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Restaurant.Name
plot4 <- ggplot(data, aes(x = Restaurant.Name, y = Tip....)) +
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Tip by Restaurant", x = "Restaurant Name", y = "Tip ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Weekend.Order
plot5 <- ggplot(data, aes(x = factor(Weekend.Order), y = Tip....)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Tip by Weekend Order",
       x = "(FALSE=Weekday,TRUE=Weekend)",
       y = "Tip ($)") +
  theme_minimal()

# All in one page 
grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 3)


# FULL CORRELATION MATRIX FOR ALL VARIABLE TYPES
library(ggcorrplot)
library(dplyr)

create_mixed_correlation_matrix <- function(data) {
  # Select NUMERIC variables (excluding ID and target variables)
  numeric_data <- data %>% 
    select(where(is.numeric)) %>%  # Select all numeric columns
    select(-Order.ID, -high_tip)   # Only exclude numeric columns here
  
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

#Convert boolean feature (TRUE -> 1, FALSE -> 0)
data$Weekend.Order <- as.numeric(data$Weekend.Order)

#Target Variable (Tip)
target <- "Tip...."

#Feature Selection based on trends and correlation
features <- c("Distance..km.", "Delivery.Duration..min.", "Toppings.Count", 
              "Price....", "Weekend.Order", 
              "Taco.Type", "Taco.Size", "Location", "Restaurant.Name")

#Data of model
model_data <- data[, c(features, target)]

# One-hot encoding (caret::dummyVars)
dummies <- dummyVars(Tip.... ~ ., data = model_data)
model_ready <- data.frame(predict(dummies, newdata = model_data))
model_ready$Tip.... <- model_data$Tip....

#Check
str(model_ready)

# MODEL 1 : LINEAR REGRESSION
library(caret)
# Fold sayısı
set.seed(123)
nfolds <- 5
folds <- createFolds(model_ready$Tip...., k = nfolds)

# RMSE'leri saklamak için vektör
rmse_values <- numeric(nfolds)
r2_values <- numeric(nfolds)

for (i in 1:nfolds) {
  # Train/test ayırma
  test_indexes <- folds[[i]]
  test_data <- model_ready[test_indexes, ]
  train_data <- model_ready[-test_indexes, ]
  
  # Modeli eğit
  model_lm <- lm(Tip.... ~ ., data = train_data)
  
  # Tahmin yap
  predictions <- predict(model_lm, newdata = test_data)
  
  # Hata metriği (RMSE)
  rmse_values[i] <- RMSE(predictions, test_data$Tip....)
  r2_values[i] <- cor(predictions, test_data$Tip....)^2
}
# Sonuçları yazdır
cat("Linear Regression Performance:\n",
    "RMSE values (each fold):", round(rmse_values, 4), "\n",
    "Average RMSE:", round(mean(rmse_values), 4), "\n",
    "R-squared values (each fold):", round(r2_values, 4), "\n",
    "Average R-squared:", round(mean(r2_values), 4), "\n")

# Train final model on full data
final_model <- lm(Tip.... ~ ., data = model_ready)

# Model summary
cat("\nFinal Model Summary:\n")
summary(final_model)

# Diagnostic plots
# Before plotting, adjust the margins
par(mar = c(4, 4, 2, 1))  # Bottom, Left, Top, Right margins
par(mfrow = c(2, 2))
plot(final_model)


# MODEL 2 : DECISION TREE REGRESSION
library(rpart)
library(rpart.plot) # for decision tree visualization

# 5-fold cross-validation preperation
set.seed(123)
folds <- createFolds(model_ready$Tip...., k = 5, list = TRUE)
rmse_values_dt <- c()
r2_values_dt <- c()
# Loop through for every fold
for(i in 1:5){
  test_indexes <- folds[[i]]
  test_data <- model_ready[test_indexes, ]
  train_data <- model_ready[-test_indexes, ]
  # Train model
  dt_model <- rpart(Tip.... ~ ., data = train_data, method = "anova")
  # Predict
  predictions <- predict(dt_model, newdata = test_data)
  # Calculate RMSE and R²
  rmse <- sqrt(mean((predictions - test_data$Tip....)^2))
  ss_total <- sum((test_data$Tip.... - mean(test_data$Tip....))^2)
  ss_res <- sum((test_data$Tip.... - predictions)^2)
  r2 <- 1 - (ss_res / ss_total)
  rmse_values_dt <- c(rmse_values_dt, rmse)
  r2_values_dt <- c(r2_values_dt, r2)
}
# Print results
cat("Decision Tree RMSE values (each fold):", round(rmse_values_dt, 4), "\n")
cat("Average RMSE:", round(mean(rmse_values_dt), 4), "\n")
cat("Decision Tree R² values (each fold):", round(r2_values_dt, 4), "\n")
cat("Average R²:", round(mean(r2_values_dt), 4), "\n")

# Train final model on full data
final_dt_model <- rpart(Tip.... ~ ., data = model_ready, method = "anova")

# Model summary
cat("\nFinal Model Summary:\n")
print(final_dt_model) 

# Variable importance
cat("\nVariable Importance:\n")
print(final_dt_model$variable.importance)

# Diagnostic plots
par(mfrow = c(1, 2)) 

# Set smaller margins before plotting
par(mar=c(2,2,2,1))  # Bottom, Left, Top, Right margins
par(mfrow=c(1,2))
# 1. Visualize decision tree structure
rpart.plot(final_dt_model, 
           type = 4, 
           extra = 101, 
           box.palette = "Blues",
           main = "Decision Tree Structure")


# MODEL 3 : NEURAL NETWORK
library(nnet)
# Normalize features except Tip 
X <- model_ready[, names(model_ready) != "Tip...."]
X_scaled <- as.data.frame(scale(X))  # Z-score standardizasyonu

# Add Tip feature again 
X_scaled$Tip.... <- model_ready$Tip....

# New data set
model_ready_scaled <- X_scaled
set.seed(42)
folds <- sample(rep(1:5, length.out = nrow(model_ready_scaled)))

rmse_values_nn <- c()
r2_values_nn <- c()

for (k in 1:5) {
  train_data <- model_ready_scaled[folds != k, ]
  test_data  <- model_ready_scaled[folds == k, ]
  
  nn_model <- nnet(Tip.... ~ ., data = train_data, size = 5, linout = TRUE, trace = FALSE, maxit = 500)
  
  predictions <- predict(nn_model, newdata = test_data)
  actual <- test_data$Tip....
  
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

library(NeuralNetTools)
# clean visualization window
dev.new(width = 25, height = 8)
par(mar = c(5, 10, 4, 2)) 

plotnet(nn_model,
        circle_cex = 5,
        circle_col = "#E1F0F7",    # Light blue node color
        bord_col = "gray40",       # Node border color
        
        # Connection aesthetics
        pos_col = "#2A788E",       # Positive weights 
        neg_col = "#DE77AE",       # Negative weights
        max_sp = TRUE)             # Max vertical space


# MODEL COMPARISON
library(ggplot2)

# RESULTS
results <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Neural Network"),
  RMSE = c(mean(rmse_values), 
           mean(rmse_values_dt),
           mean(rmse_values_nn)),
  R2 = c(mean(r2_values),
         mean(r2_values_dt),
         mean(r2_values_nn))
)

# for plotting
rmse_df <- data.frame(Model = results$Model, RMSE = results$RMSE)
r2_df <- data.frame(Model = results$Model, R2 = results$R2)

#model_names <- c("Linear Regression", "Decision Tree", "Neural Network")
#rmse_values <- c(1.0506, 1.0657, 1.2481)
#r2_values <- c(0.1414, 0.1092, -0.2317)

#rmse_df <- data.frame(Model = model_names, RMSE = rmse_values)
#r2_df   <- data.frame(Model = model_names, R2 = r2_values)

# GRAPH FOR RMSE 
plot_rmse <- ggplot(rmse_df, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(RMSE, 3)), vjust = -0.5, size = 4) +
  labs(title = "Model RMSE Comparison", y = "RMSE", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# GRAPH FOR R² 
plot_r2 <- ggplot(r2_df, aes(x = Model, y = R2, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(R2, 3)), vjust = -0.5, size = 4) +
  labs(title = "Model R² Comparison", y = "R²", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# SHOW THEM IN ONE PAGE
library(gridExtra)
grid.arrange(plot_rmse, plot_r2, ncol = 2)


# DIFFERENT SELECTION FOR FEATURES----------------------------------------------------
library(ggplot2)
library(lattice)
library(caret)
library(dplyr)

# Convert Boolean (TRUE -> 1, FALSE -> 0)
data$Weekend.Order <- as.numeric(data$Weekend.Order)

# Target variable (Tip)
target <- "Tip...."

# FEATURE SELECTION
features <- c("Delivery.Duration..min.", "Toppings.Count", 
              "Price....", "Weekend.Order", 
              "Taco.Type", "Location")

# Data of model
model_data <- data[, c(features, target)]

# One-hot encoding (caret::dummyVars)
dummies <- dummyVars(Tip.... ~ ., data = model_data)
model_ready <- data.frame(predict(dummies, newdata = model_data))
model_ready$Tip.... <- model_data$Tip....

# Check
str(model_ready)

# MODEL 1 : LINEAR REGRESSION
library(caret)
# Fold sayısı
set.seed(123)
nfolds <- 5
folds <- createFolds(model_ready$Tip...., k = nfolds)

# RMSE'leri saklamak için vektör
rmse_values <- numeric(nfolds)
r2_values <- numeric(nfolds)

for (i in 1:nfolds) {
  # Train/test ayırma
  test_indexes <- folds[[i]]
  test_data <- model_ready[test_indexes, ]
  train_data <- model_ready[-test_indexes, ]
  
  # Modeli eğit
  model_lm <- lm(Tip.... ~ ., data = train_data)
  
  # Tahmin yap
  predictions <- predict(model_lm, newdata = test_data)
  
  # Hata metriği (RMSE)
  rmse_values[i] <- RMSE(predictions, test_data$Tip....)
  r2_values[i] <- cor(predictions, test_data$Tip....)^2
}
# Sonuçları yazdır
cat("Linear Regression Performance:\n",
    "RMSE values (each fold):", round(rmse_values, 4), "\n",
    "Average RMSE:", round(mean(rmse_values), 4), "\n",
    "R-squared values (each fold):", round(r2_values, 4), "\n",
    "Average R-squared:", round(mean(r2_values), 4), "\n")

# Train final model on full data
final_model <- lm(Tip.... ~ ., data = model_ready)

# Model summary
cat("\nFinal Model Summary:\n")
summary(final_model)

# Diagnostic plots
# Before plotting, adjust the margins
par(mar = c(4, 4, 2, 1))  # Bottom, Left, Top, Right margins
par(mfrow = c(2, 2))
plot(final_model)


# MODEL 2 : DECISION TREE REGRESSION
library(rpart)
# 5-fold cross-validation preperation
set.seed(123)
folds <- createFolds(model_ready$Tip...., k = 5, list = TRUE)
rmse_values_dt <- c()
r2_values_dt <- c()
# Loop through for every fold
for(i in 1:5){
  test_indexes <- folds[[i]]
  test_data <- model_ready[test_indexes, ]
  train_data <- model_ready[-test_indexes, ]
  # Train model
  dt_model <- rpart(Tip.... ~ ., data = train_data, method = "anova")
  # Predict
  predictions <- predict(dt_model, newdata = test_data)
  # Calculate RMSE and R²
  rmse <- sqrt(mean((predictions - test_data$Tip....)^2))
  ss_total <- sum((test_data$Tip.... - mean(test_data$Tip....))^2)
  ss_res <- sum((test_data$Tip.... - predictions)^2)
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
# Normalize features except Tip 
X <- model_ready[, names(model_ready) != "Tip...."]
X_scaled <- as.data.frame(scale(X))  # Z-score standardizasyonu

# Add Tip feature again 
X_scaled$Tip.... <- model_ready$Tip....

# New data set
model_ready_scaled <- X_scaled
set.seed(42)
folds <- sample(rep(1:5, length.out = nrow(model_ready_scaled)))

rmse_values_nn <- c()
r2_values_nn <- c()

for (k in 1:5) {
  train_data <- model_ready_scaled[folds != k, ]
  test_data  <- model_ready_scaled[folds == k, ]
  
  nn_model <- nnet(Tip.... ~ ., data = train_data, size = 5, linout = TRUE, trace = FALSE, maxit = 500)
  
  predictions <- predict(nn_model, newdata = test_data)
  actual <- test_data$Tip....
  
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


#----------------------------COMPARISON
library(ggplot2)
# RESULTS
results <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Neural Network"),
  RMSE = c(mean(rmse_values), 
           mean(rmse_values_dt),
           mean(rmse_values_nn)),
  R2 = c(mean(r2_values),
         mean(r2_values_dt),
         mean(r2_values_nn))
)
# for plotting
rmse_df <- data.frame(Model = results$Model, RMSE = results$RMSE)
r2_df <- data.frame(Model = results$Model, R2 = results$R2)

#model_names <- c("Linear Regression", "Decision Tree", "Neural Network")
#rmse_values <- c(1.0407, 1.0479, 1.2243)
#r2_values <- c(0.1558, 0.1365, -0.1878)

# RMSE 
plot_rmse <- ggplot(rmse_df, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(RMSE, 3)), vjust = -0.5, size = 4) +
  labs(title = "Model RMSE Comparison", y = "RMSE", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# R²
plot_r2 <- ggplot(r2_df, aes(x = Model, y = R2, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(R2, 3)), vjust = -0.5, size = 4) +
  labs(title = "Model R² Comparison", y = "R²", x = "") +
  theme_minimal() +
  theme(legend.position = "none")

library(gridExtra)
grid.arrange(plot_rmse, plot_r2, ncol = 2)

