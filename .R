# Explore the data
summary(student_data)

# Building a Linear Regression model using the train data
# Target variable: FinalGrades
# Features: StudyHours, QuizScores, ForumPosts, and PreviousGrades
model <- lm(FinalGrades ~ StudyHours + QuizScores + ForumPosts + PreviousGrades, data = train_data)

# Making predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluation metrics
# Compute the mean squared error and R-squared
mse <- mean((predictions - test_data$FinalGrades)^2)
rsquared <- summary(model)$r.squared

# Print evaluation metrics
cat("Mean Squared Error:", mse, "\n")
cat("R-squared:", rsquared, "\n")

Model Accuracy based on Prediction Interval

# Get the predictions and prediction intervals
pred_int <- predict(model, newdata = test_data, interval = "prediction")

# Extract lower and upper bounds of the prediction interval
lower_bound <- pred_int[, "lwr"]
upper_bound <- pred_int[, "upr"]

# Actual values from the test data
actual_values <- test_data$FinalGrades

# Check if the actual values fall within the prediction interval
correct_predictions <- actual_values >= lower_bound & actual_values <= upper_bound

# Compute accuracy
accuracy <- sum(correct_predictions) / length(correct_predictions)

# Print accuracy
cat("Model Accuracy using Prediction Interval:", accuracy, "\n")
