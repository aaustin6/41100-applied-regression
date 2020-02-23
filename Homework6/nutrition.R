nutrition = read.csv(file = "nutrition.csv")

### Basic Model ###
par(mfrow=c(1,2)) # Plot + Residuals
basic_relationship = nutrition$woh ~ nutrition$age
plot(
  basic_relationship,
  xlab = "Age (Months)",
  ylab = "Weight to Height",
  main = "Simple W-to-H Ratio and Age",
  pch = 18
)
basic_model = lm(formula = basic_relationship)
abline(basic_model, col=c("red"))
summary(basic_model)
anova(basic_model)

# Check the Model
residual_relationship = basic_model$residuals ~ nutrition$age
residual_model = lm(formula = residual_relationship)
plot(
  residual_relationship,
  xlab = "Age (Months)",
  ylab = "Residuals",
  main = "Simple W-to-H Residuals",
  pch = 18
)
abline(residual_model, col=c("red"))
summary(residual_model)

hist(rstudent(basic_model), col = c("black"))
qqnorm(rstudent(basic_model), col = c("black"), pch = 18)
abline(a = 0, b = 1, col = c("red"))

### Transformed Model ###
transformed_relationship = log(nutrition$woh) ~ log(nutrition$age)
plot(
  transformed_relationship,
  xlab = "Log of Age (Months)",
  ylab = "Log of Weight to Height",
  main = "Log Model of W-to-H Ratio & åAge",
  pch = 18
)
transformed_model = lm(formula = transformed_relationship)
abline(transformed_model, col=c("red"))
anova(transformed_model)
summary(transformed_model)

transformed_residual_relationship = transformed_model$residuals ~ nutrition$age
transformed_residual_model = lm(formula = transformed_residual_relationship)
plot(
  transformed_residual_relationship,
  xlab = "Age (Months)",
  ylab = "Residuals",
  main = "Log W-to-H Residuals",
  pch = 18
)
abline(transformed_residual_model, col=c("red"))

hist(rstudent(transformed_model), col = c("black"))
qqnorm(rstudent(transformed_model), col = c("black"))
abline(a = 0, b = 1, col = c("red"))

### Polynomial Model ###
par(mfrow=c(1,2)) # Plot + Residuals
nutrition$age_squared = nutrition$age ^ 2
nutrition$age_cubed = nutrition$age ^ 3
polynomial_relationship = nutrition$woh ~ nutrition$age + nutrition$age_squared + nutrition$age_cubed
plot(
  nutrition$woh ~ nutrition$age,
  xlab = "Age (Months)",
  ylab = "Weight to Height Ratio",
  main = "2 Polynomial Model",
  pch = 18
)
polynomial_model = lm(formula = nutrition$woh ~ nutrition$age + nutrition$age_squared + nutrition$age_cubed)
lines(nutrition$age, fitted(polynomial_model), col = c("red"))
summary(polynomial_model)
anova(polynomial_model)

polynomial_residual_relationship = polynomial_model$residuals ~ nutrition$age
polynomial_residual_model = lm(formula = polynomial_residual_relationship)
plot(
  polynomial_residual_relationship,
  xlab = "Age (Months)",
  ylab = "Residuals",
  main = "2 Polynomial Model Residuals",
  pch = 18
)
abline(polynomial_residual_model, col=c("red"))

hist(rstudent(polynomial_model), col = c("blue"))
qqnorm(rstudent(polynomial_model), col = c("black"), main = "Q-Q of Polynomial Model")
abline(a = 0, b = 1, col = c("red"))

# Dummy Model
nutrition$under_7 = as.numeric(nutrition$age < 7)
nutrition$mixed = nutrition$under_7 * nutrition$age
dummy_relationship = nutrition$woh ~
  nutrition$age +
  nutrition$under_7 +
  nutrition$mixed

par(mfrow=c(1,2)) # Plot + Residuals
plot(
  nutrition$woh ~ nutrition$age,
  xlab = "Age",
  ylab = "Weight to Height",
  main = "Dummy Model W-to-H vs. Age",
  pch = 18
)
dummy_model = lm(dummy_relationship, data = nutrition)
lines(nutrition$age, fitted(dummy_model), col = c("red"))
summary(dummy_model)
anova(dummy_model)

# Residual Plot
dummy_residual_relationship = dummy_model$residuals ~ nutrition$age
plot(
  dummy_residual_relationship,
  xlab = "Age",
  ylab = "Residuals",
  main = "Dummy W-to-H Residuals",
  pch = 18
)
abline(lm(dummy_residual_relationship), col = c("red"))

hist(rstudent(polynomial_model), col = c("black"))

### Model Comparison and Evaluation ###

# AIC for Basic/Polynomial
anova(basic_model, polynomial_model)
basic_polynomial_aic = c(
  basic_model = extractAIC(basic_model)[2],
  polynomial_model = extractAIC(polynomial_model)[2]
)
print(basic_polynomial_aic)

# AIC for Basic/Dummy/Polynomial
anova(basic_model, polynomial_model, dummy_model)
basic_dummy_aic = c(
  basic_model = extractAIC(basic_model)[2],
  polynomial_model = extractAIC(polynomial_model)[2],
  dummy_model = extractAIC(dummy_model)[2]
)
print(basic_dummy_aic)


basic_plot = plot(
  nutrition$woh ~ nutrition$age,
  xlab = "Age (Months)",
  ylab = "Weight to Height Ratio",
  main = "Model fit for W-to-H Ratio and Age",
  pch = 18
)
abline(basic_model, col=c("red"))
lines(nutrition$age, fitted(polynomial_model), col = c("blue"))
lines(nutrition$age, fitted(dummy_model), col = c("purple"))
legend(
  50,
  0.6,
  legend=c("Simple", "Polynomial", "Dummy"),
  col=c("Red", "Blue", "Purple"),
  lty = 1,
  cex = 0.8,
)

# Prediction Interval Plot
par(mfrow=c(1,1))
basic_plot = plot(
  nutrition$woh ~ nutrition$age,
  xlab = "Age (Months)",
  ylab = "Weight to Height Ratio",
  main = "95% Prediction Intervals for W-to-H Ratio and Age",
  pch = 18
)
basic_prediction = predict(
  basic_model,
  interval = "prediction",
  level =.95,
)
polynomial_prediction = predict(
  polynomial_model,
  interval = "prediction",
  level =.95,
)
dummy_prediction = predict(
  dummy_model,
  interval = "prediction",
  level =.95,
)
prediction_data = data.frame(
  basic_low = basic_prediction[,"lwr"],
  basic_high = basic_prediction[,"upr"],
  polynomial_low = polynomial_prediction[,"lwr"],
  polynomial_high = polynomial_prediction[,"upr"],
  dummy_low = dummy_prediction[,"lwr"],
  dummy_high = dummy_prediction[,"upr"]
)
lines(nutrition$age, prediction_data$basic_low, col = c("red"))
lines(nutrition$age, prediction_data$basic_high, col = c("red"))
lines(nutrition$age, prediction_data$polynomial_low, col = c("blue"))
lines(nutrition$age, prediction_data$polynomial_high, col = c("blue"))
lines(nutrition$age, prediction_data$dummy_low, col = c("purple"))
lines(nutrition$age, prediction_data$dummy_high, col = c("purple"))

# Evaluate all three Residual Plots
par(mfrow=c(1,3))
plot(
  residual_relationship,
  xlab = "Age",
  ylab = "Residuals",
  main = "Simple Model Residuals",
  pch = 18
)
abline(residual_model, col = c("red"))

plot(
  polynomial_residual_relationship,
  xlab = "Age",
  ylab = "Residuals",
  main = "Polynomial Model Residuals",
  pch = 18
)
abline(polynomial_residual_model, col = c("red"))

plot(
  dummy_residual_relationship,
  xlab = "Age",
  ylab = "Residuals",
  main = "Dummy Model Residuals",
  pch = 18
)
abline(lm(dummy_residual_relationship), col = c("red"))

# Evaluate all three Q-Q Polots
par(mfrow=c(1,3))
qqnorm(rstudent(basic_model), col = c("black"), main = "Q-Q of Simple Model")
abline(a = 0, b = 1, col = c("red"))
qqnorm(rstudent(polynomial_model), col = c("black"), main = "Q-Q of Polynomial Model")
abline(a = 0, b = 1, col = c("red"))
qqnorm(rstudent(dummy_model), col = c("black"), main = "Q-Q of Dummy Model")
abline(a = 0, b = 1, col = c("red"))
