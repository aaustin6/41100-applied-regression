cheese = read.csv(file = "cheese.csv")
# Plot our data
boxplot(
 log(cheese$vol) ~ cheese$disp,
 xlab = "Display (Y/N)",
 ylab = "Log of Sales",
 main = "Display Effect on Log of Sales Volume",
 pch = 1,
)

display_model = lm(formula = log(cheese$vol) ~ cheese$disp)
abline(display_model, col=c("red"))
anova(display_model)

# Histogram of the data
hist(rstudent(display_model), col = 4)
qqnorm(rstudent(display_model), col = 4)
abline(a=0, b=1)

# Plot the residuals to check our model
boxplot(
  display_model$residuals ~ cheese$disp,
  ylab = "Residuals",
  xlab = "Display",
  main = "Display vs. Residuals",
  pch = 1,
)
display_residuals_model = lm(display_model$residuals ~ cheese$disp)
abline(display_residuals_model, col=c("red"))

# Vol vs. Price is useless
boxplot(
  cheese$vol ~ cheese$price,
  xlab = "Price Elasticity",
  ylab = "Log of Sales",
  main = "Log of Price",
  pch = 1,
)

# Calculate log of vol and price, update data table to avoid multiple recalculations
log.vol = round(log(cheese$vol), digits = 4)
log.price = round(log(cheese$price), digits = 4)
cheese = cbind(cheese, log.vol, log.price)

# Setup our price elasticity models
price_model = lm(formula = cheese$log.vol ~ cheese$log.price + cheese$disp)
price_display_model = lm(formula =
  cheese$log.vol[cheese$disp == 1] ~
  cheese$log.price[cheese$disp == 1]
)
price_no_display_model = lm(formula =
  cheese$log.vol[cheese$disp == 0] ~
  cheese$log.price[cheese$disp == 0]
)

plot(
  cheese$log.vol ~ cheese$log.price,
  xlab = "Log of Price",
  ylab = "Log of Sales",
  main = "Price Elasticity",
  col = ifelse(cheese$disp == 1,'red','black'),
  pch = 1,
  cex = 0.5,
)
abline(price_display_model, col=c("red"))
abline(price_no_display_model, col=c("black"))
legend(
  0.23,
  6.5,
  legend=c("With Ads", "Without Ads"),
  col=c("Red", "Black"),
  lty = 1,
  cex = 0.8,
)


# Another LM of the difference betwen the two

# Attempt to subtract the differences between both models and
x_values = seq(min(cheese$log.price), max(cheese$log.price), length.out = 200)
predicted_values = data.frame(x_values)
fitted_values = function(model, x_vector) {
  b0 = model$coefficients[1]
  b1 = model$coefficients[2]
  return (b0 + b1 * x_vector)
}

y_display = fitted_values(price_display_model, x_values)
y_no_display = fitted_values(price_no_display_model, x_values)
predicted_values = cbind(predicted_values, difference = y_display - y_no_display)

difference_model = lm(predicted_values$difference ~ predicted_values$x_values)
plot(
  cbind(y_display, y_no_display) ~ x_values
)
plot(
  predicted_values$difference ~ predicted_values$x_values,
  ylab = "Log of Predicted Regression Differences",
  xlab = "Log of Price",
  main = "Price Elasticity — Differences vs. Log of Price",
  pch = 1,
  cex = 0.5,
)

