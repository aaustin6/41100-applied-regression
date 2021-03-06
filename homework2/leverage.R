getwd()
leverage = read.csv(file = "leverage.csv")

plot(
  leverage$SPX ~ leverage$VIX,
  xlab = "VIX",
  ylab = "SPX",
  main = "VIX vs. SPX Raw Data",
  pch = 1,
)

# TODO: Do we need to annualize the returns?
# Calculate the daily return for each column.
returns_spx <- diff(log(leverage$SPX))
returns_vix <- diff(log(leverage$VIX))

# (i) and (ii)
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

# (v)
# https://stats.stackexchange.com/questions/259812/95-confidence-interval-for-mean-of-a-large-sample
predict_confidence = 0.95 # used for a two-tail test of 90% - see stack exchange link
predict_stdev = 0.025
predict_vix_return = 0.10
predict_y =
    model$coefficients["(Intercept)"] +
    (model$coefficients["returns_vix"] * predict_vix_return)

# Y ~ N(predict_y, predict_vix_return)
# PI = Y +/- stdev
predict_interval = qnorm(
  predict_confidence,
  mean = predict_y,
  sd = predict_stdev,
)
predict_lower = predict_y - predict_interval
predict_upper = predict_y + predict_interval
predict_final = cbind(predict_lower, predict_y, predict_upper)
