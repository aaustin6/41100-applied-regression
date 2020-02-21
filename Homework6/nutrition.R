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
  main = "Residual Plot",
  pch = 18
)
abline(residual_model, col=c("red"))
summary(residual_model)

hist(rstudent(basic_model), col = c("black"))
qqnorm(rstudent(basic_model), col = c("black"), pch = 18)
abline(a = 0, b = 1, col = c("red"))

### Transformed Model ###
transformed_relationship = sqrt(nutrition$woh) ~ sqrt(nutrition$age)
plot(
  transformed_relationship,
  xlab = "Sqrt of Age (Months)",
  ylab = "Sqrt of Weight to Height",
  main = "Relationship b/w Transformed W-to-H Ratio and Age",
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
  ylab = "Weight to Height Squared",
  main = "Polynomial Relationship b/w W-to-H Ratio and Age",
  pch = 18
)
polynomial_model = lm(formula = nutrition$woh ~ nutrition$age + nutrition$age_squared)
lines(age_squared,fitted(polynomial_model), col = c("red"))
summary(polynomial_model)
anova(polynomial_model)

hist(rstudent(polynomial_model), col = c("black"))
qqnorm(rstudent(polynomial_model), col = c("black"))
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

# Check our Model
dummy_residuals_relationship = dummy_model$residuals ~ nutrition$age
plot(
  dummy_residuals_relationship,
  xlab = "Age",
  ylab = "Residuals",
  main = "Residuals of W-to-H Ratio and Age using Dummy Vars",
  pch = 18
)
abline(lm(dummy_residuals_relationship), col = c("red"))

hist(rstudent(polynomial_model), col = c("black"))
qqnorm(rstudent(polynomial_model), col = c("black"))
abline(a = 0, b = 1, col = c("red"))

# AIC
anova(basic_model, dummy_model)
model_aic = c(
  basic_model = extractAIC(basic_model)[2],
  dummy_model = extractAIC(dummy_model)[2]
)
print(model_aic)
