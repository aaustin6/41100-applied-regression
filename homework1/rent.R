getwd()
housing = read.csv(file = "rent.csv")

# (i) Produce a boxplot for the marginal distribution of Rent, and compare this to boxplots for
# conditional distributions for Rent given each level of AC, and for Rent given the different numbers of Rooms.
# What can you say about the effect of these variables on rent?
par(mfrow=c(1,3))
boxplot(
  housing$Rent,
  ylab="Rent",
  main="Marginal Distribution"
)
boxplot(
  housing$Rent ~ housing$Rooms,
  xlab="Rooms",
  ylab=NULL,
  col=8,
  main="Rent | Rooms",
)
boxplot(
  housing$Rent ~ housing$AC,
  xlab="AC",
  ylab=NULL,
  col=8,
  main="Rent | AC",
)

# (ii) Reconsider the questions in (i) through ANOVA rather than plots. That is, do two ANOVAs to find the SSR, SSE, and SST for Rent grouped either by AC and Rooms.
# • In each case, what % of total variation is explained by the grouping?
# • Do you think that these factors make a meaningful difference to rent?
# • How does your conclusion compare to the F value and Pr(> F) on the ANOVA table?
# • Compare your results to the boxplots in (i).
room_model = lm(formula = housing$Rent ~ housing$Rooms)
anova(room_model)

ac_model = lm(formula = housing$Rent ~ housing$AC)
anova(ac_model)

# (iii) Now investigate the effect of apartment size (SqFt) on rent.
# Calculate the correlation between SqF t and Rent and use this to fit the regression line Rent = b0 + b1SqF t + e.
# What does b1 tell you about the influence of SqFt? What would you say if asked to predict at SqFt = 0?
model = lm(formula = housing$Rent ~housing$SqFt)
model_correlation = cor(housing$SqFt, housing$Rent)
# summary(model)

# (iv) Consider the results from your regression in (iii).
# Plot the data and regression line.
# Plot the residuals both as a histogram and against SqFt.
# Do you see any problems?
# Could you get a better model by ignoring some observations? If so, re-fit the model.
par(mfrow=c(1,1))
plot(
  housing$SqFt,
  housing$Rent,
  main = "SqFt vs. Rent",
  xlab ="SqFt",
  ylab ="Rent",
  col = "blue",
)
abline(model)

# Residuals
plot(
  housing$SqFt,
  residuals(model),
  main = "SqFt vs. Rent Residuals",
  xlab = "SqFt",
  ylab = "Residuals",
  col = "black",
)
abline(h=0, col=8, lwd=2)

# Refit our model, excluding values 3 standard deviations above the mean.
new_sqft_max= mean(housing$SqFt) + sd(housing$SqFt) * 3
subset_housing = subset(housing, SqFt < new_sqft_max)
new_model = lm(formula = subset_housing$Rent ~ subset_housing$SqFt)
new_model_correlation = cor(subset_housing$SqFt, subset_housing$Rent)

plot(
  subset_housing$SqFt,
  subset_housing$Rent,
  main = "SqFt vs. Rent (excluding outliers)",
  xlab = "SqFt",
  ylab = "Rent",
  col = "blue",
)
abline(new_model)

plot(
  subset_housing$SqFt,
  residuals(new_model),
  main = "SqFt vs. Rent Residuals (excluding outliers)",
  xlab = "SqFt",
  ylab = "Residuals",
  col = "black",
)
abline(h=0, col=8, lwd=2)

