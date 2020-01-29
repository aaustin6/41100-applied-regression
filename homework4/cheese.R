getwd()
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
hist(rstudent(display_model), col = 7)

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
