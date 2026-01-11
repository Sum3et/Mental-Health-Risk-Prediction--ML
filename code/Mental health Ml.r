data <- read.csv("C://Users//Rohit//Desktop//Mental Health Risk Prediction-ML//data/mental_health_data.csv")

table(data$Mental_Health_Risk)

# Convert target to factor
data$Mental_Health_Risk <- as.factor(data$Mental_Health_Risk)

# Train-test split
set.seed(123)
index <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[index, ]
test <- data[-index, ]

# Train Logistic Regression model
model <- glm(Mental_Health_Risk ~ ., data = train, family = "binomial")

# Prediction
prob <- predict(model, test, type = "response")
pred <- ifelse(prob > 0.5, 1, 0)
pred <- as.factor(pred)

# Evaluation
cm <- table(Predicted = pred, Actual = test$Mental_Health_Risk)
accuracy <- mean(pred == test$Mental_Health_Risk)

print(cm)
print(accuracy)

png("confusion_matrix.png")
fourfoldplot(cm, color = c("red", "green"))
dev.off()


