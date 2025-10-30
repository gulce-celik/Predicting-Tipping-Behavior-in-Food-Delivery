# 🍔 Predicting Tipping Behavior in Food Delivery  
*Business Intelligence Project – Master-Level Course (Erasmus, May–June 2025)*  

## 📘 Overview
This project analyzes **tipping behavior in taco deliveries** and predicts **tip amounts** using regression models.  
The study explores which order and delivery features most influence tipping and evaluates multiple models to identify the most effective approach.

---

## 🎯 Objectives
- Predict **Tip Amount ($)** using features such as taco type, delivery time, price, and location.  
- Compare the performance of **Linear Regression**, **Decision Tree**, and **Neural Network** models.  
- Understand the **relationship between tipping behavior** and available order features.

---

## 📊 Dataset
- **Source:** Taco Sales Dataset (2024–2025)  
- **Size:** 1,000 records  
- **Target Variable:** `Tip ($)`  
- **Main Features:**  
  - `Price`, `Taco Type`, `Taco Size`, `Toppings Count`, `Delivery Duration`, `Weekend Flag`, `Location`, `Restaurant`  
- **No missing values**  
- Outliers detected and capped using **Z-Score** and **IQR** methods  

---

## ⚙️ Methodology
### 🧹 Data Preprocessing
- Outlier detection and capping (Z-score, IQR)  
- One-hot encoding for categorical variables  
- Boolean and time features processed and normalized  
- **Z-score normalization** applied for Neural Network training  

### 🤖 Modeling
Implemented and compared three regression algorithms using **R**:  
- **Linear Regression**  
- **Decision Tree Regression**  
- **Neural Network (1 hidden layer)**  

All models evaluated with **5-fold Cross Validation**, using **RMSE** and **R²** as metrics.  
Two different **feature sets** were tested for comparative analysis.  

---

## 📈 Results
| Model | RMSE | R² | Notes |
|:------|:----:|:--:|:------|
| Linear Regression | ~1.04 | ~0.16 | Best performance, interpretable |
| Decision Tree | ~1.05 | ~0.13 | Slight overfitting in small feature sets |
| Neural Network | ~1.22 | -0.19 | Poor generalization |

🔹 **Linear Regression** performed best overall.  
🔹 Weak correlations between tip and other variables suggest missing behavioral or psychological factors.  
🔹 A secondary test predicting **Price ($)** achieved **R² > 0.90**, confirming model validity.

---

## 💡 Insights
- Tip amount is **weakly correlated** with price, delivery duration, and toppings.  
- Suggests influence from external factors like customer mood, driver behavior, or delivery experience.  
- Data pipeline and modeling approach are **robust and reproducible**.

---

## 🚀 Tools & Technologies
- **Language:** R  
- **Libraries:** `caret`, `rpart`, `nnet`, `ggplot2`, `dplyr`  
- **Techniques:** Regression Modeling, Outlier Detection, One-Hot Encoding, Cross-Validation  
- **Environment:** RStudio  

---

## 📚 References
- [NBER – Social Preferences & Tipping Study](https://www.nber.org/papers/w26380)  
- [Kaggle Taco Sales Dataset (2024–2025)](https://www.kaggle.com/datasets/atharvasoundankar/taco-sales-dataset-20242025)  
- [DataCamp – Linear Regression in R](https://www.datacamp.com/tutorial/linear-regression-R)

---

## ✨ Author
**Gülce Çelik**  
📧 gulcecelik.contact@gmail.com  
🎓 Supervised by *Prof. Giovanna Maria Dimitri*  
University of Siena, Department of Information Engineering and Mathematical Sciences  

