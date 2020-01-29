flu = read.csv(file = "flu.csv")

is_winter = (flu$Week < 9 | flu$Week > 47)
flu = cbind(flu, is_winter)
# winter = subset(flu, Week < 9 | Week > 47)
# not_winter = subset(flu, Week > 8 & Week < 48 )

boxplot(
  flu$All.Deaths ~ flu$is_winter,
  xlab = "Winter",
  ylab = "Deaths",
  main = "Seasons vs. Deaths",
  pch = 1,
)
winter_model = lm(formula = flu$All.Deaths ~ flu$is_winter)
abline(winter_model)

ordered_flu = flu[order(is_winter),]

plot(
  ordered_flu$Percent.of.Deaths.Due.to.Pneumonia.and.Influenza ~ ordered_flu$Week,
  xlab = "Week of the Year",
  ylab = "% of Death to stuff",
  pch = 1,
)
