getwd()

funds = read.csv(file = "vanguard.csv")

row_count = dim(funds)[1]
column_count = dim(funds)[2]
fund_columns = column_count - 2 # last two columns are spx and t-bills

returns_annualized_spx =
  diff(log(funds$SPX)) * 52 -
  funds$TBILL[2:(row_count)] / 100

returns_annualized_funds =
  52 * log(funds[2:row_count, 2:fund_columns]) -
  52 * log(funds[1:row_count - 1, 2:fund_columns]) -
  funds$TBILL[2:row_count] / 100

# Build a matrix of the linear regression models
capm_model = lm(as.matrix(returns_annualized_funds) ~ returns_annualized_spx)
print(capm_model)

plot(
  capm_model$coeff[2,],
  capm_model$coeff[1,],
  ylab = "alpha",
  xlab = "beta",
  col = 0,
)
text(
  x = capm_model$coeff[2,],
  y = capm_model$coeff[1,],
  labels = names(funds)[2:column_count],
  col = c("blue"),
  cex = 0.75,
)
abline(v = 0, col = c("black"), lty = 2)
abline(h = 0, col = c("black"), lty = 2)
