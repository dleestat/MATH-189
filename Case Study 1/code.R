## Project 1: Babies' Weight Versus Smoking Parents
## -------------Import Datasets-----------------
babies <- read.table("babies.txt", header = TRUE)
babies23 <- read.table("babies23.txt", header = TRUE)

##---------------Data Cleaning------------------
babies_cleaned <- data.frame()
babies_cleanedin.ind <- which(babies['smoke'] != 9)
babies_cleaned <- babies[babies_cleanedin.ind,]
babies_cleanedin.ind <- which(babies_cleaned['weight'] != 999)
babies_cleaned <- babies_cleaned[babies_cleanedin.ind,]

babies23_cleanedin.ind <- which(babies23['smoke'] != 9)
babies23_cleaned <- babies23[babies23_cleanedin.ind,]

##-----------------Standardized data---------------
std_bwt <- (babies_cleaned$bwt- mean(babies_cleaned$bwt))/sd(babies_cleaned$bwt)
babies_cleaned <- cbind(babies_cleaned, std_bwt)

std_wt <- (babies23_cleaned$wt- mean(babies23_cleaned$wt))/sd(babies23_cleaned$wt)
babies23_cleaned <- cbind(babies23_cleaned, std_wt)

##------------------Data Subsetting--------------------
babies_nosk.ind <- which(babies_cleaned['smoke'] == 0)
babies_nonsmoker <- babies_cleaned[babies_nosk.ind,]
babies_smoker <- babies_cleaned[-babies_nosk.ind,]

babies23_nosk.ind <- which(babies23_cleaned['smoke'] == 0)
babies23_sk.ind <- which(babies23_cleaned['smoke'] == 1)
babies23_nfsk.ind <- which(babies23_cleaned['smoke'] == 2 | babies23_cleaned['smoke'] == 3)
babies23_nonsmoker <- babies23_cleaned[babies23_nosk.ind,]
babies23_smoker <- babies23_cleaned[babies23_sk.ind,]
babies23_stopped_smoker <- babies23_cleaned[babies23_nfsk.ind,]

babies23_skgl.ind <- which(babies23_cleaned['smoke'] != 0)
babies23_gensmoker <- babies23_cleaned[babies23_skgl.ind,]

babies23_gennosk.ind <- which(babies23_cleaned['smoke'] != 1)
babies23_gen_nonsmoker <- babies23_cleaned[babies23_gennosk.ind,]

##-----------------Export CSV File-----------------
write.csv(babies_cleaned, "babies_cleaned.csv")
write.csv(babies23_cleaned, "babies23_cleaned.csv")

##---------Numerical value Computation-------------
# Mean, SD, and Quantile Value of Babies Weight babies.txt Datasets
summary(babies_cleaned)

summary(babies23_smoker$wt)
summary(babies23_nonsmoker$wt)
summary(babies23_stopped_smoker$wt)
summary(babies23_gensmoker$wt)
summary(babies23_gen_nonsmoker$wt)
summary(babies23_smoker$wt)

##-----------------MonteCarloSimulation---------------------
n = length(babies_cleaned)
D_obs = chisq.test(babies_cleaned$bwt) # observed value of the test statistic
B = 1000 # number of Montecarlo samples
D_mc = numeric(B) # will store the simulated test statistics
for (b in 1:B) {
  X = sample(1:6, n, TRUE)
  D_mc[b] = chisq.test(table(X))$bwt
}
pval = (sum(D_mc >= D_obs) + 1)/(B + 1)
pval

##----------------------t-test--------------------------
t.test(babies_cleaned$std_bwt[babies_cleaned$smoke=="0"], babies_cleaned$std_bwt[babies_cleaned$smoke=="1"])
t.test(babies23_cleaned$std_wt[babies23_cleaned$smoke == '0'], babies23_cleaned$std_wt[babies23_cleaned$smoke != "0"])
t.test(babies23_gen_nonsmoker$gestation, babies23_smoker$gestation)
##----------------------F-test--------------------------
var.test(babies_cleaned$std_bwt[babies_cleaned$smoke == "0"],babies_cleaned$std_bwt[babies_cleaned$smoke == "1"])
var.test(babies23_cleaned$std_wt[babies23_cleaned$smoke == "0"],babies23_cleaned$std_wt[babies23_cleaned$smoke != "0"])

##-----------------Incidence Testings--------- ----------
##------------ Chi Square Test of Independence-----------
# Chi Square Test of Independence for weight set as 88.2
weightCheck_88 <-  c()
for(i in 1:nrow(babies23_cleaned)){
  if (babies23_cleaned[i,'wt'] < 88.2){
    weightCheck_88 <- c(weightCheck_88, 0)
  } else {
    weightCheck_88 <- c(weightCheck_88, 1)
  }
}
babies23_cleaned <- cbind(babies23_cleaned, weightCheck_88)
chisq.test(table(babies23_cleaned$smoke, babies23_cleaned$weightCheck_88))

#Chi Square Test of Independence for weight set as 85
weightCheck_85 <-  c()
for(i in 1:nrow(babies23_cleaned)){
  if (babies23_cleaned[i,'wt'] < 85){
    weightCheck_85 <- c(weightCheck_85, 0)
  } else {
    weightCheck_85 <- c(weightCheck_85, 1)
  }
}
babies23_cleaned <- cbind(babies23_cleaned, weightCheck_85)
chisq.test(table(babies23_cleaned$smoke, babies23_cleaned$weightCheck_85))

# Chi Square Test of Independence for weight set as 81
weightCheck_81 <-  c()
for(i in 1:nrow(babies23_cleaned)){
  if (babies23_cleaned[i,'wt'] < 81){
    weightCheck_81 <- c(weightCheck_81, 0)
  } else {
    weightCheck_81 <- c(weightCheck_81, 1)
  }
}
babies23_cleaned <- cbind(babies23_cleaned, weightCheck_81)
chisq.test(table(babies23_cleaned$smoke, babies23_cleaned$weightCheck_81))

# Chi Square Test of Independence for weight set as 100
weightCheck_100 <-  c()
for(i in 1:nrow(babies_cleaned)){
  if (babies_cleaned[i,'bwt'] < 100){
    weightCheck_100 <- c(weightCheck_100, 0)
  } else {
    weightCheck_100 <- c(weightCheck_100, 1)
  }
}
babies_cleaned <- cbind(babies_cleaned, weightCheck_100)
chisq.test(table(babies_cleaned$smoke, babies_cleaned$weightCheck_100))


#-------------------Fifth Question----------------------
gesCheck1 <-  c()
for(i in 1:nrow(babies23_cleaned)){
  if (babies23_cleaned[i,'gestation'] < 252){
    gesCheck1 <- c(gesCheck1, 0)
  } else {
    gesCheck1 <- c(gesCheck1, 1)
  }
}
babies23_cleaned <- cbind(babies23_cleaned, gesCheck1)

babies23_gest.ind <- which(babies23_smoker['gestation'] < 252)
babies23_eb_sm <- babies23_smoker[babies23_gest.ind,]
eb_sm <- nrow(babies23_eb_sm)
num_sm <- nrow(babies23_smoker)
gest_frequency_sm <- eb_sm/num_sm

babies23_gest2.ind <- which(babies23_gen_nonsmoker['gestation'] < 252)
babies23_eb_nsm <- babies23_gen_nonsmoker[babies23_gest2.ind,]
eb_nsm <- nrow(babies23_eb_nsm)
num_nsm <- nrow(babies23_gen_nonsmoker)
gest_frequency_nsm <- eb_nsm/num_nsm

gestation_nonsmokers <- nrow(babies23_gen_nonsmoker)
gestation_total <- nrow(babies23_cleaned)
frequency_gestation_smokers <- gestation_smokers/gestation_total
frequency_gestation_nonsmokers <- gestation_nonsmokers/gestation_total
print(frequency_gestation_nonsmokers)
print(frequency_gestation_smokers)
##-----------------Data Visualization------------------
## Histogram
# Add smoke status to babies data frame
status <-  c()
for(i in 1:nrow(babies_cleaned)){
  if (babies_cleaned[i,'smoke'] ==0){
    status <- c(status, 'no')
  } else {
    status <- c(status, 'yes')
  }
}
babies_cleaned <- cbind(babies_cleaned, status)

status23 <-  c()
for(i in 1:nrow(babies23_cleaned)){
  if (babies23_cleaned[i,'smoke'] == 0 ){
    status23 <- c(status23, 'Non-Smokers')
  } else if (babies23_cleaned[i,'smoke'] ==1){
    status23 <- c(status23, 'Smokers')
  } else{
    status23 <- c(status23, 'Stopped-Smokers')
  }
}
babies23_cleaned <- cbind(babies23_cleaned, status23)

smoke_status <-  c()
for(i in 1:nrow(babies23_cleaned)){
  if (babies23_cleaned[i,'smoke'] == 1 ){
    smoke_status <- c(smoke_status, 'Smokers')
  } else {
    smoke_status <- c(smoke_status, 'Non-Smokers')
  }
}
babies23_cleaned <- cbind(babies23_cleaned, smoke_status)

# import packages
library(ggplot2)
library(plyr)
# Calculate mean
mu <- ddply(babies_cleaned, "smoke", summarise, grp.mean=mean(std_bwt))
mu23 <- ddply(babies23_cleaned, "smoke_status", summarise, grp23.mean = mean(std_wt))

# Basic histogram
ggplot(babies_nonsmoker, aes(x=bwt)) + geom_histogram(binwidth= 8)
# Change colors
p<-ggplot(babies_nonsmoker, aes(x=bwt)) + geom_histogram(color="black", fill="white")

# Histogram with density plot
ggplot(babies_nonsmoker, aes(x=bwt)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ geom_vline(aes(xintercept=mean(bwt)),
                                                     color="blue", linetype="dashed", size=1)

ggplot(babies_smoker, aes(x=bwt)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ geom_vline(aes(xintercept=mean(bwt)),
                                                     color="blue", linetype="dashed", size=1)

ggplot(babies23_nonsmoker, aes(x=wt)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ geom_vline(aes(xintercept=mean(wt)),
                                                     color="blue", linetype="dashed", size=1)

ggplot(babies23_smoker, aes(x=wt)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+ geom_vline(aes(xintercept=mean(wt)),
                                                     color="blue", linetype="dashed", size=1)

# Comparison Histogram
ggplot(babies_cleaned, aes(x=std_bwt, color=status, fill=status)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Weight density plot",x="Standardized Weight", y = "Density")+
  theme_classic()

ggplot(babies23_cleaned, aes(x= std_wt, color= status23, fill= status23)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu23, aes(xintercept=grp23.mean),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Density Comparison - Standardized Baby Birth Weights of Mothers With Different Smoking Status",x="Standardized Birth Weight", y = "Density Among Population", size = 0)+
  theme_classic()

ggplot(babies23_cleaned, aes(x= std_wt, color= smoke_status, fill= smoke_status)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu23, aes(xintercept=grp23.mean),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Density Comparison - Standardized Baby Birth Weights of Mothers With Different Smoking Status",x="Standardized Birth Weight", y = "Density Among Population", size = 0)+
  theme_classic()

## Boxplot (Use to compare median)
boxplot(bwt~smoke, babies_cleaned)
boxplot(std_wt~smoke, babies23_cleaned, main = "BoxPlot- Standardized Baby Birth Weights of Mothers With Different Smoking Status", xlab = "Mothers' smoking status", ylab = "Babies' Birth Weight")
boxplot(wt~smoke_status, babies23_cleaned, main = "BoxPlot- Standardized Baby Birth Weights of Mothers With Different Smoking Status", xlab = "Mothers' smoking status", ylab = "Babies' Birth Weight")
# IQR is the length of the edge of the box, and anything
# beyond the Whisker will be the outlier cases

# Skewness and Kurtosis
install.packages('moments')
library(moments)
skewness(babies_nonsmoker$bwt)
kurtosis(babies_nonsmoker$bwt)
skewness(babies_smoker$bwt)
kurtosis(babies_smoker$bwt)

skewness(babies23_nonsmoker$wt)
kurtosis(babies23_nonsmoker$wt)
skewness(babies23_smoker$wt)
kurtosis(babies23_smoker$wt)
skewness(babies23_stopped_smoker$wt)
kurtosis(babies23_stopped_smoker$wt)
# If the distribution is flatter, it has larger kurtosis
# If any of the two data is very different than the normal
# distributrion, it is less likely to be a normal distribution

## QQPlot
library(car)
qqnorm(babies_nonsmoker$bwt, pch = 1, frame = FALSE)
qqline(babies_nonsmoker$bwt, col = "steelblue", lwd = 2)
qqPlot(babies_nonsmoker$bwt)

qqnorm(babies_smoker$bwt, pch = 1, frame = FALSE)
qqline(babies_smoker$bwt, col = "steelblue", lwd = 2)
qqPlot(babies_smoker$bwt, xlab = "Theoretical Quantiles",
       ylab = "Observed Quantiles", main = "QQ-Plot for Birth Weights of Babies Born to Smoking Mothers")

qqnorm(babies23_gen_nonsmoker$std_wt, pch = 1, frame = FALSE)
qqline(babies23_gen_nonsmoker$std_wt, col = "steelblue", lwd = 2)
qqPlot(babies23_gen_nonsmoker$std_wt, xlab = "Theoretical Quantiles",
       ylab = "Observed Quantiles", main = "QQ-Plot for Birth Weights of Babies Born to non-Smoking Mothers")

qqplot(babies23_cleaned$std_wt[babies23_cleaned$smoke==1], babies23_cleaned$std_wt[babies23_cleaned$smoke!= 1],
       xlab = "Quantile of Standardized Birth Weights of Babies born to smokers",
       ylab = "Quantile of Standardized Birth Weights of Babies born to non-smokers", main = "QQ-Plot Comparison for Babies' Birth Weights Based on Mothers' Smoking Status")
qqline(babies23_cleaned$std_wt[babies23_cleaned$smoke==1], babies23_cleaned$std_wt[babies23_cleaned$smoke!= 1])
