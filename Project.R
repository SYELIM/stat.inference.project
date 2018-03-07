##Part 1
set.seed(12345678) #Set the seed for reproducibility 
lambda <- 0.2 #Given value of lambda
n <- 40 #number of exponentials
sim_num <- 1000 #number of simuations

#Conduct expoential distribution and check for its mean
exp_dist <- matrix(rexp(n*sim_num, lambda), sim_num)
exp_means <- rowMeans(exp_dist)

##Conduct mean calculation for 1000 simulations of the exponential distribution
means <- NULL
for (i in 1: sim_num)means <- c(means, mean(rexp(n, lambda)))
hist(means, main= "Exponential Distribution Mean", breaks = 30) ##Make histogram of the result

##Sample mean vs. theoretical mean
mu <- 1/lambda
mu #Theoretical mean

sample_mn <- mean(exp_means)
sample_mn #Mean of sample means
#histogram
hist(means, main= "Exponential Distribution Mean", breaks = 30)
abline(v=mu, col="red",lwd=3)
abline(v=sample_mn, col="green", lwd=3)

##Sample variance (and std. dev.) vs. theoretical variance (and std.dev.)
var <- (1/lambda)^2/n
var #Theoretical variance
sigma <- 1/lambda/sqrt(n)
sigma #theoretical standard deviation 

sample_var <- var(exp_means)
sample_var #sample variance
sample_sd <- sd(exp_means)
sample_sd #sample standard deviation

#Distribution approximately normal?
hist(means, prob=TRUE, main= "Exponential Distribution Mean", breaks = 30)
lines(density(means), lwd=2, col="red")

##Part 2
library(datasets)
data(ToothGrowth)

#Summary of the dataset
dim(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)

#Exploratory data analysis
boxplot(len~supp, data=ToothGrowth, col="gold", main= "Tooth Growth", xlab= "Suppliment", ylab= "Tooth length(mm)")
boxplot(len~dose, data=ToothGrowth, col="green", main= "Tooth Growth", xlab= "Dose (mg/day)", ylab= "Tooth length (mm)")
boxplot(len~supp*dose, data=ToothGrowth, col=(c("purple","lightblue")), main="Tooth Growth", xlab="Suppliment and Dose")

#CI and hypothesis testing using t-test
#Null hypothesis: There is no difference between the two types of suppliment (orange juice (OJ) and ascorbic acid(VC))
#Compare tooth growth by suppliment
t.test(len~supp, data=ToothGrowth)

#Compare tooth growth by different doses
#Null hypothesis: There is no difference between the dose levels.
dose_0.5_1.0 <- subset(ToothGrowth, dose %in% c(0.5,1.0))
t.test(len~dose, data=dose_0.5_1.0)

dose_0.5_2.0 <- subset(ToothGrowth, dose %in% c(0.5,2.0))
t.test(len~dose, data=dose_0.5_2.0)

dose_1.0_2.0 <- subset(ToothGrowth, dose %in% c(1.0, 2.0))
t.test(len~dose, data=dose_1.0_2.0)

