nutrition = read.csv(file = "nutrition.csv")

### Basic Model ###
basic_relationship = nutrition$woh ~ nutrition$age
plot(
  basic_relationship,
  xlab = "Age (Months)",
  ylab = "Weight to Height",
  main = "Relationship between W-to-H Ratio and Age",
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
  xlab = "Age",
  ylab = "Residuals",
  main = "Weight-to-Height and Age Residuals",
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
  main = "Log Relationship b/w LW-to-H Ratio and Age",
  pch = 18
)
transformed_model = lm(formula = transformed_relationship)
abline(transformed_model, col=c("red"))
anova(transformed_model)
summary(transformed_model)

# Check the model
hist(rstudent(transformed_model), col = c("black"))
qqnorm(rstudent(transformed_model), col = c("black"))
abline(a = 0, b = 1, col = c("red"))

### Polynomial Model ###
nutrition$age_squared = nutrition$age ^ 2
polynomial_relationship = nutrition$woh ~ nutrition$age + nutrition$age_squared
plot(
  nutrition$woh ~ nutrition$age_squared,
  xlab = "Age (Months Squared)",
  ylab = "Weight to Height Ratio",
  main = "Polynomial Relationship b/w W-to-H Ratio and Age",
  pch = 18
)
polynomial_model = lm(formula = nutrition$woh ~ nutrition$age + nutrition$age_squared)
lines(nutrition$age_squared, fitted(polynomial_model), col = c("red"))
summary(polynomial_model)
anova(polynomial_model)

polynomial_residual_relationship = polynomial_model$residuals ~ nutrition$age
polynomial_residual_model = lm(formula = polynomial_residual_relationship)

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

plot(
  nutrition$woh ~ nutrition$age,
  xlab = "Age",
  ylab = "Weight to Height",
  main = "Relationship b/w W-to-H Ratio and Age using Dummy Vars",
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
  main = "Residuals of W-to-H Ratio and Age using Dummy Vars",
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
# TODO: need to get the X-values and then plot the low/high values
# lines(basic_prediction)

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
