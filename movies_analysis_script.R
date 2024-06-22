# Uploading data

movies <- read.csv("~/GitHub/Movies-analysis/data/movies.csv", row.names=1)
head(movies)

data <- movies[,2:4]
head(data)

# Statistical parameters

n <- length(data$Year.of.Release)

# Year of release

year <- data$Year.of.Release

mean_year <- mean(year)
hmean_year <- sum(1/year) / n
sd_year <- sd(year)
min_year <- min(year)
max_year <- max(year)
median_year <- median(year)
quan_year <- quantile(year)
range_year <- max_year - min_year
mode_year <- as.numeric(names(sort(table(year), decreasing = T)[1]))
cv_year <- sd_year / mean_year * 100
var_year <- var(year)

# Run time in minutes

duration <- data$Run.Time.in.minutes

mean_duration <- mean(duration)
hmean_duration <- sum(1/duration) / length(duration)
sd_duration <- sd(duration)
min_duration <- min(duration)
max_duration <- max(duration)
median_duration <- median(duration)
quan_duration <- quantile(duration)
range_duration <- max_duration - min_duration
mode_duration <- as.numeric(names(sort(table(duration), decreasing = TRUE)[1]))
cv_duration <- sd_duration / mean_duration * 100
var_duration <- var(duration)

# Movie rating

rate <- data$Movie.Rating

mean_rate <- mean(rate)
hmean_rate <- sum(1/rate) / n
sd_rate <- sd(rate)
min_rate <- min(rate)
max_rate <- max(rate)
median_rate <- median(rate)
quan_rate <- quantile(rate)
range_rate <- max_rate - min_rate
mode_rate <- as.numeric(names(sort(table(rate), decreasing = T)[1]))
cv_rate <- sd_rate / mean_rate * 100
var_rate <- var(rate)

# Plots

# Histograms

par(mfcol = c(1,3))

hist(year, main = "Histogram of Release Years \nof Movies", xlab = "Year", 
     ylab = "Frequency", col = "red", breaks = 30)
hist(duration, main = "Histogram of Movie Durations", 
     xlab = "Duration in Minutes", ylab = "Frequency", col = "blue", 
     breaks = 30)
hist(rate, main = "Histogram of Movie Ratings", xlab = "Rating (1-10)", 
     ylab = "Frequency", col = "purple", breaks = 30)


# Density plots

par(mfcol = c(1,3))

plot(density(year), main = "Density Plot of Release Years", xlab = "Year", 
     ylab = "Density", col = 'red')
plot(density(duration), main = "Density Plot of Movie Durations", 
     xlab = "Duration", ylab = "Density", col = 'blue')
plot(density(rate), main = "Density Plot of Ratings", xlab =  "Rating", 
     ylab = "Density", col = 'purple')


# Empirical CDF plots

par(mfcol = c(1,3))

plot(ecdf(year), main = "Empirical CDF of Release Years", xlab = "Year", 
     ylab = "ECDF", col = 'red', cex = 0.75)
plot(ecdf(duration), main = "Empirical CDF of Movie Durations", xlab = 
       "Duration", ylab = "ECDF", col = 'blue', cex = 0.75)
plot(ecdf(rate), main = "Empirical CDF of Ratings", xlab = "Rating", 
     ylab = "ECDF", col = 'purple', cex = 0.75)


# Box plots

par(mfcol = c(1,3))

boxplot(year, col = 'red', main = "Box Plot of Release Year")
boxplot(duration, col = 'blue', main = "Box Plot of Duration")
boxplot(rate, col = 'purple', main = "Box Plot of Ratings")


# Scatter plots with trend lines

par(mfcol = c(1,3))

plot(year, rate, main = "Trend of Release Year\nvs. Rating", 
     xlab = "Release Year", ylab = "Rating", col = "red")
lines(lowess(year, rate), col = "black", lwd = 2)
plot(year, duration, main = "Trend of Release Year\nvs. Duration", 
     xlab = "Release Year", ylab = "Duration", col = "blue")
lines(lowess(year, duration), col = "black", lwd = 2)
plot(duration, rate, main = "Trend of Duration\nvs. Rating", 
     xlab = "Duration", ylab = "Rating", col = "purple")
lines(lowess(duration, rate), col = "black", lwd = 2)


# Statistical hypothesis testing

# Hypothesis about the average movie rating
# Null hypothesis (H0): The average movie rating is equal to 7.
# Alternative hypothesis (H1): The average movie rating is greater than 7.
# alpha = 0.1

t.test(rate, mu = 7, alternative = "greater", conf.level = 0.9)


# Hypothesis about the correlation between the release year of a movie and its duration
# Null hypothesis (H0): There is no significant correlation between the release year of a movie and its duration.
# Alternative hypothesis (H1): There is a significant correlation between the release year of a movie and its duration.
# alpha = 0.01

cor.test(year, duration, conf.level = 0.99)


# Null hypothesis (H0): The ratio of the variance of duration to the variance of release year is 2.
# Alternative hypothesis (H1): The ratio of the variance of duration to the variance of release year is less than 2.

var.test(duration, year, ratio=2, alternative = "less")


