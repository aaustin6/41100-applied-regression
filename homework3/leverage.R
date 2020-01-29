getwd()
leverage = read.csv(file = "leverage.csv")

plot(
  leverage$SPX ~ leverage$VIX,
  xlab = "VIX",
  ylab = "SPX",
  main = "VIX vs. SPX Raw Data",
  pch = 1,
)

# Calculate the daily return for each column.
returns_spx <- diff(log(leverage$SPX))
returns_vix <- diff(log(leverage$VIX))
plot(
  returns_vix,
  returns_spx,
  xlab = "VIX % Return",
  ylab = "SPX % Returns",
  main = "SPX vs. VIX Daily Log Returns",
)
model = lm(returns_spx ~ returns_vix)
abline(model, col=c("red"))

# Double-check our Model
plot(
  model$residuals ~ returns_vix,
  ylab = "Residuals",
  xlab = "VIX % Returns",
  main = "VIX % Returns vs. Residuals",
  pch = 1,
)
residuals_model = lm(model$residuals ~ returns_vix)
abline(residuals_model, col=c("red"))
# sum(model$residuals)
# cor(model$residuals, returns_vix)

# Calculate coefficients using standard deviation and correlation
b1 = cor(returns_vix, returns_spx) * (sd(returns_spx)/sd(returns_vix))
b0 = mean(returns_spx) - mean(returns_vix) * b1

# Commands for answering particular questions about the model:
#
# sd(returns_spx)
# sd(returns_vix)
# cor(returns_vix, returns_spx)
# model$coefficients["(Intercept)"] == b0
# model$coefficients["returns_vix"] == b1

# (iii)
model_summary = summary(model)
model_anova = anova(model)

ssr = model_anova["returns_vix","Sum Sq"]
sse = model_anova["Residuals", "Sum Sq"]
sst = ssr + sse
# cbind(ssr, sse, sst)
# print(model_summary)
# print(model_anova)

# (iv)
model_correlation = sqrt(model_summary$r.squared)
model_correlation_matches = - model_correlation == cor(returns_vix, returns_spx)
paste("Model r-squared and correlation match:", model_correlation_matches)

################################################################################

# HM3
# https://stats.stackexchange.com/questions/259812/95-confidence-interval-for-mean-of-a-large-sample
# (i) Predict the variance and use it to give a 90% prediction interval
# for a 10% increase in VIX, taking uncertainty of your estimates B0 and B1
# into account.
predict_confidence = 0.95
predict_vix_return = 0.10
predict_df = length(model_summary$residuals) - 2
predict_variance = sqrt(
  sum(model_summary$resid ^ 2) / predict_df
)
model_summary$sigma == predict_variance

predict_b0 = model_summary$coefficients[,"Estimate"]["(Intercept)"]
predict_b1 = model_summary$coefficients[,"Estimate"]["returns_vix"]
predict_sb0 = model_summary$coefficients[,"Std. Error"]["(Intercept)"]
predict_sb1 = model_summary$coefficients[,"Std. Error"]["returns_vix"]
t_dist = qt(.950, df = predict_df)

predict_ci = c(predict_b0 - predict_sb0 * t_dist, predict_b0 + predict_sb0 * t_dist) # 90% CI for intercept

confint(model, level=0.9)

Xf <- data.frame(size=.10)
prediction = predict(model, newdata=Xf, se.fit=TRUE, interval="prediction", level=.9)

predict_zb0 = predict_b0 / predict_sb0 # number of standard errors away from the null
# c(b0 - sb0*t025, b0 + sb0*t025)

plot(
  prediction$fit[, "lwr"],
  prediction$fit[, "upr"],
  pch = 1,
)

# predict_y =
#  model$coefficients["(Intercept)"] +
#  (model$coefficients["returns_vix"] * predict_vix_return)
# confit(model, level = 0.95)


# TODO: (ii) Plot a summary of the predictive distribution (mean and 90% ) for Vix returns
# ranging from -20% to 20%

# TODO: (iii) What range of VIX returns would be plausible for the expected S&P 500
# Return to the rise of 2% based on your prediction interval?


# Y ~ N(predict_y, predict_vix_return)
# PI = Y +/- stdev
# predict_interval = qnorm(
#   predict_confidence,
#   mean = predict_y,
#   sd = predict_stdev,
# )
# predict_lower = predict_y - predict_interval
# predict_upper = predict_y + predict_interval
# predict_final = cbind(predict_lower, predict_y, predict_upper)
