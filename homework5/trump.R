tweets = read.csv(file = "trump.csv")

# (i) If Robinson’s conclusion is true, do you expect tweeting sources to
# affect the influence of Trump’s tweets, as evaluated by favorite and retweet counts?
# What does the data tell us?
par(mfrow=c(1,2))
boxplot(
  log(tweets$favoriteCount) ~ tweets$source,
  xlab = "Device",
  ylab = "Log(Favorite Count)",
  main = "Tweet Favorites by Device",
)
favorite_model = lm(formula = log(tweets$favoriteCount) ~ tweets$source)
abline(favorite_model, col=c("red"))

boxplot(
  log(tweets$retweetCount) ~ tweets$source,
  xlab = "Device",
  ylab = "Log(Retweets)",
  main = " Retweets by Device",
)

retweet_model = lm(formula = log(tweets$retweetCount) ~ tweets$source)
abline(favorite_model, col=c("red"))

# (ii) (Optional) If Robinson’s conclusion is true, do you expect tweets from different sources to
# use rhetorical strategies differently? Use exclamation and hashtag to test your hypothesis.

# (iii) Do exclamation and hashtag influence readers’ responses to Trump’s tweets?
# What if conditioning on tweeting hours and inclusion of pictures/links?
par(mfrow=c(1,1))
plot(
  log(tweets$favoriteCount) ~ tweets$exclamation,
  xlab = "Exclamation",
  ylab = "Log(Favorite Count)",
  main = "Tweet Favorites by Exclamation",
)
exclamation_model = lm(formula = log(tweets$favoriteCount) ~ tweets$exclamation)
abline(exclamation_model, col=c("red"))

# HashTags
plot(
  log(tweets$favoriteCount) ~ tweets$hashtag,
  xlab = "HashTags",
  ylab = "Log(Favorite Count)",
  main = "Tweet Favorites by HashTags",
)
hashtag_model = lm(formula = log(tweets$favoriteCount) ~ tweets$hashtags)
abline(hashtag_model, col=c("red"))

# Hour of Day
boxplot(
  log(tweets$favoriteCount) ~ tweets$hour,
  xlab = "Hour of Day",
  ylab = "Log(Favorites)",
  main = " Favorites by Hour",
  pch = 1,
  cex = 0.5,
)
hour_model = lm(formula = log(tweets$favoriteCount) ~ tweets$hour)
abline(hour_model, col=c("red"))
# There's a better way of fitting this data maybe?
# hours_squared = tweets$hour^2
# hour_multiple_model = lm(formula = log(tweets$favoriteCount) ~ tweets$hour + hours_squared)
# abline(hour_multiple_model, col=c("blue"))

# Pictures
boxplot(
  log(tweets$favoriteCount) ~ tweets$picture,
  xlab = "Picture",
  ylab = "Log(Favorites)",
  main = " Trump Tweet Favorites by Picture/Link",
)
picture_model = lm(formula = log(tweets$favoriteCount) ~ tweets$picture)
abline(picture_model, col=c("red"))

multiple_model = lm(
  log(tweets$favoriteCount) ~ tweets$source +
  tweets$picture +
  tweets$exclamation +
  tweets$hashtag
)
anova(multiple_model)

# (iv) Does switching devices change the influence in (iii)?
# State and test your hypothesis.

# (v) Do you see any problems in the analysis above? If yes, how could you improve it?
# What additional data would you need?

