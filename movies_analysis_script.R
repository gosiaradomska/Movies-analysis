movies <- read.csv("~/GitHub/Movies-analysis/data/movies.csv", row.names=1)

data <- movies[,2:4]

setwd("C:/Users/gosia/Documents/GitHub/Movies-analysis/data")
write.csv(data, "data.csv", row.names = FALSE)

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
mode_year <- as.numeric(names(sort(table(year), decreasing = T)[1])) # dominanta
cv_year <- sd_year / mean_year * 100 # wsp. zmiennosci
var_year <- var(year) # wariancja

# Run time in minutes

time <- data$Run.Time.in.minutes

mean_time <- mean(time)
hmean_time <- sum(1/time) / n
sd_time <- sd(time)
min_time <- min(time)
max_time <- max(time)
median_time <- median(time)
quan_time <- quantile(time)
range_time <- max_time - min_time
mode_time <- as.numeric(names(sort(table(time), decreasing = T)[1]))
cv_time <- sd_time / mean_time * 100
var_time <- var(time)

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
