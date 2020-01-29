model_b0 = -0.008194
model_b1 = 1.690067

model_sb0 = 0.003680
model_sb1 = 0.136379

model_tb0 = -2.227
model_tb1 = 12.392

model_pb0 = 0.0282
model_pb1 = 2 * 10^-16

model_n = 103
model_rsquared = 0.6033
model_standard_error = 0.03734


# (i) What were the degrees of freedom used in calculating sε? What are the SSE and SSR?

model_df = model_n - 2
model_sst =  0.355
model_ssr = model_rsquared * model_sst  # r-squared = SSR / SST
model_sse = model_sst - model_ssr

# (ii) Compute the sample variance for Y (s^2 of Y) and sample correlation between X and Y (rXY).

model_sample_variance = model_sse / model_df # slide 5
model_sample_correlation = sqrt(model_rsquared) # lecture 2, slide 34 r-squared is squared correlation
model_sample_covariance = model_sample_correlation / (model_sb0 * model_sb1)

# (iii) Suppose that the expected market excess return for the next week is 2.7%. What is the expected
# return for the firm next week, given that the risk free rate is 0.03%/52?

# E(ri) - rf = Bi(E(rm) - rf) # lecture 2, slide 54
market_excess_return = 0.027
risk_free_return = 0.0003 / 52
predicted_return = model_b0 + model_b1 * (market_excess_return - risk_free_return) # do we need b0?

# (iv) Suppose further that X ̄ = 0.0001 and sX = 0.02711201. Construct the 90% forecast interval for the estimate in (iii).


# (v) Construct the 95% confidence interval for the slope of the true regression line β1

# (vi) A senior analyst estimates that the firm’s beta is roughly 1.52. Justify/Refute this result using
# an appropriate hypothesis test.

# (vii) When explaining the firm’s analysis to an investor, a junior analyst suddenly found that he
# misplaced Y and X in the above regression. How would he explain to the investor about R2?

# (viii) Which of the four sectors does the firm most likely belong to, Basic materials, Technology,
# Utility, or Financial sector?


