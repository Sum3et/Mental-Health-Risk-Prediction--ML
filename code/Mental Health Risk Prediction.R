# ======================================================
# Mental Health Risk Prediction using Machine Learning
# ======================================================

# 1. Load Required Libraries
library(rpart)
library(caret)

# Load raw data for visualization (no scaling)
raw_data <- read.csv("C://Users//Rohit//Downloads//mental_health_dataset.csv")

raw_data$mental_health_risk <- ifelse(
  raw_data$mental_health_risk == "Low",
  "Low",
  "High"
)
raw_data$mental_health_risk <- as.factor(raw_data$mental_health_risk)

# 2. Load Dataset
data <- read.csv("C://Users//Rohit//Downloads//mental_health_dataset.csv")

# 3. Target Variable Processing
data$mental_health_risk <- ifelse(
  data$mental_health_risk == "Low",
  "Low",
  "High"
)

data$mental_health_risk <- as.factor(data$mental_health_risk)
# Check distribution
table(data$mental_health_risk)

# 4. Remove Data Leakage Variables
data$depression_score <- NULL
data$anxiety_score <- NULL

# 5. Scale Numeric Features
num_cols <- sapply(data, is.numeric)
data[num_cols] <- scale(data[num_cols])

# 6. Train-Test Split
set.seed(123)
index <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[index, ]
test  <- data[-index, ]

# 7. BASELINE MODEL: Logistic Regression
model <- glm(
  mental_health_risk ~ .,
  data = train,
  family = "binomial"
)
# Model summary (interpretability)
summary(model)

# 8. Predictions & Evaluation
prob <- predict(model, test, type = "response")

pred <- ifelse(prob > 0.5, "High", "Low")
pred <- factor(pred, levels = levels(test$mental_health_risk))

# Confusion Matrix & Accuracy
cm <- table(Predicted = pred, Actual = test$mental_health_risk)
accuracy <- mean(pred == test$mental_health_risk)

print(cm)
print(accuracy)

# 9. COMPARISON MODEL: Decision Tree
dt_model <- rpart(
  mental_health_risk ~ .,
  data = train,
  method = "class"
)

dt_pred <- predict(dt_model, test, type = "class")

dt_cm <- table(Predicted = dt_pred, Actual = test$mental_health_risk)
dt_accuracy <- mean(dt_pred == test$mental_health_risk)

print(dt_cm)
print(dt_accuracy)

# 10. Evaluation Metrics
conf_lr <- confusionMatrix(pred, test$mental_health_risk)
conf_dt <- confusionMatrix(dt_pred, test$mental_health_risk)

conf_lr$byClass[c("Precision", "Recall", "F1")]
conf_dt$byClass[c("Precision", "Recall", "F1")]

# 11. GRAPHS
# ---------- 11.1 Class Distribution ----------
png("class_distribution.png")

barplot(
  table(data$mental_health_risk),
  col = c("skyblue", "salmon"),
  main = "Class Distribution of Mental Health Risk",
  xlab = "Mental Health Risk",
  ylab = "Count"
)

dev.off()

# ---------- 11.2 Confusion Matrix ----------
png("confusion_matrix.png")

fourfoldplot(
  cm,
  color = c("red", "green"),
  main = "Confusion Matrix - Logistic Regression"
)

dev.off()

# ------------11.3 Scatter Plot-----------
png("stress_vs_sleep.png")

plot(
  raw_data$sleep_hours,
  raw_data$stress_level,
  col = ifelse(raw_data$mental_health_risk == "High", "red", "blue"),
  pch = 19,
  xlab = "Sleep Hours",
  ylab = "Stress Level",
  main = "Stress Level vs Sleep Hours"
)

legend(
  "topright",
  legend = c("Low Risk", "High Risk"),
  col = c("blue", "red"),
  pch = 19
)
dev.off()

# 12. Cross-Validation
control <- trainControl(method = "cv", number = 5)

cv_model <- train(
  mental_health_risk ~ .,
  data = data,
  method = "glm",
  family = "binomial",
  trControl = control
)

cv_model
