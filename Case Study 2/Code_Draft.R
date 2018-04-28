install.packages('gmodels')
install.packages('e1071')
library(gmodels)
library(dplyr)
library(plyr)
library(e1071)
library(car)

# Import Dataset
video <- read.table("videogame.txt", header = TRUE)

# Data Cleaning
video[video == 99] <- NA


# Scenario 1
# Point Estimate
population <- 314
sample <- nrow(video)
nrow(video)
time_frac <- length(which(video$time != 0))/sample
time_frac

# Finite Sample Corrector CI
cor_fac <- sqrt((population-sample)/population)
std_err_FSC <- sqrt(time_frac*(1-time_frac)/(sample-1))*cor_fac
print(paste("Confidence Interval: (", time_frac-1.96*std_err_FSC, ", ", time_frac+1.96*std_err_FSC, ")"))

# Test for normality (Need to be added)
if_played <- c()
for(i in 1:nrow(video)) {
  if (!is.na(video[i, 'time'])){
    if(video[i,'time']== 0) # Dislike video games
      if_played <- c(if_played, 0)
    else
      if_played <- c(if_played, 1)
  }
  else
    if_played <- c(if_played, NA)
}
video <- cbind(video, if_played)

# CLT
std_err_CLT <- sqrt(time_frac*(1-time_frac)/sample)
print(paste("Confidence Interval: (", time_frac-2*std_err_CLT, ", ", time_frac+2
            *std_err_CLT, ")"))

# Bootstrap
boot.population <- rep(video$if_played, length.out = 314)
sample1 <- sample(boot.population, size = 91, replace = FALSE)
set.seed(189289)
B = 400
boot.sample <- array(dim = c(B, 91))
for (i in 1:B) {
  boot.sample[i, ] <- sample(boot.population, size = 91, replace = FALSE)
}
boot.mean <- apply(X = boot.sample, MARGIN = 1, FUN = mean)
# Point Estimate
mean(boot.mean)
int.boot <- c(quantile(boot.mean, 0.025), quantile(boot.mean, 0.975))
int.boot

#boot.kurtosis <- apply(X = boot.sample, MARGIN =1, FUN = kurtosis)
#int.boot <- c(quantile(boot.kurtosis, 0.025), quantile(boot.kurtosis, 0.975))
#width <- 1.96 * sqrt(time_frac*(1-time_frac)*(population-sample)/((sample-1)*population))
#int.exact <- c(time_frac - width, time_frac + width)
#int.exact

# Scenario 2


# Scenario 3
# Sample Graph
hist(video$time, breaks = 15, probability = FALSE, density = 15, col = 3, border = 3, 
     xlab = "Amount of Time Playing", ylab= "Frequency in the population", 
     main = "Frequency of people playing video games in different length of time")
abline(v = mean(video$time), col = "blue", lwd = 2)

# Point Estimate
time_mean <- mean(video$time)
time_mean
# Finite Sample Corrector CI

# CLT
std_err_CLT <- sqrt(var(video$time)/sample)
print(paste("Confidence Interval: (", time_mean-2*std_err_CLT, ", ", time_mean+2
            *std_err_CLT, ")"))

# Bootstrap Simulation
boot.population <- rep(video$time, length.out = 314)
sample1 <- sample(boot.population, size = 91, replace = FALSE)
set.seed(189289)
B = 400
boot.sample <- array(dim = c(B, 91))
for (i in 1:B) {
  boot.sample[i, ] <- sample(boot.population, size = 91, replace = FALSE)
}
boot.mean <- apply(X = boot.sample, MARGIN = 1, FUN = mean)

# Histogram of Bootstrap
hist(boot.mean, breaks = 15, probability = TRUE, density = 20, col = 3, border = 3, 
     xlab = "Mean time playing Video Games", ylab= "Density in the population", 
     main = "Distribution of Mean time playing video games")
lines(density(boot.mean, adjust = 2), col = 2)
abline(v = mean(boot.mean), col = "blue", lwd = 2)
# Point Estimate
mean(boot.mean)
# Bootstrap CI
int.boot <- c(quantile(boot.mean, 0.025), quantile(boot.mean, 0.975))

# Kurtosis Histogram (Need to be revised)
require(e1071)
kurtosis_ = NULL
for(i in 1:1000){
  kurtosis_[i] = kurtosis(rnorm(400))
}
m = qplot(kurtosis_,geom ='histogram')
m+geom_histogram(aes(fill = ..count..))
# Skewness Histogram
skewness_ = NULL
for(i in 1:1000){
  skewness_[i] = skewness(rnorm(400))
}
m = qplot(skewness_,geom ='histogram')
m+geom_histogram(aes(fill = ..count..))

# Scenario 4 (EC)

# Scenario 5
# Clean out the "Never played data"
video.clean <- video[which(video$like != 1),]
# Regroup the 'like' value
video.clean$like[video.clean$like == 2 | video.clean$like == 3] <- "Like"
video.clean$like[video.clean$like == 4 | video.clean$like == 5] <- "Dislike"
video.w <- video.clean[which(!is.na(video.clean$work)),]
# Regroup the 'sex' value
video.w$sex[video.w$sex == 0 ] <- "Female"
video.w$sex[video.w$sex == 1] <- "Male"
# Regroup the 'work' value
video.w$work[video.w$work > 0 ] <- "Work"
video.w$work[video.w$work == 0] <- "No Work"
# Regroup the 'own' value
video.w$own[video.w$own == 0 ] <- "No PC"
video.w$own[video.w$own == 1] <- "Own PC"

# Cross tabulations between like and sex
CrossTable(video.w$like, video.w$sex)
chisq.test(table(video.w$like, video.w$sex))
# Cross tabulations between like and work
CrossTable(video.w$like, video.w$work)
chisq.test(table(video.w$like, video.w$work))
# Cross tabulations between like and own
CrossTable(video.w$like, video.w$own)
chisq.test(table(video.w$like, video.w$own))

# Bar Graph
counts <- table(video.w$work, video.w$like)
barplot(counts, main = "Preference on Video Games by Employment Status",
        xlab='Response to Video Games', ylab = 'Frequency Count',
        col=c('black','white'), legend = rownames(counts), beside=TRUE)

counts <- table(video.w$sex, video.w$like)
barplot(counts, main = "Preference on Video Games by Gender",
        xlab='Response to Video Games', ylab = 'Frequency Count',
        col=c('black','white'), legend = rownames(counts), beside=TRUE)

counts <- table(video.w$own, video.w$like)
barplot(counts, main = "Preference on Video Games by If Owning a PC",
        xlab='Response to Video Games', ylab = 'Frequency Count',
        col=c('black','white'), legend = rownames(counts), beside=TRUE)

# Scenario 6 (EC)
