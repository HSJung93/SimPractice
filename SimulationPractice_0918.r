# Web Stanford Edu Bios221 Lab 3

# set.seed is important
set.seed(20200918)
vecpoisson=rpois(100,5)
mean(vecpoisson)

# for loops in R is slower than apply fth.
# the sum of exponential gives a gamma dist.
reps <- 50000
nexps <- 5
rate <- .1

system.time(
    x1 <- replicate(reps, sum(rexp(n=nexps,rate=rate)))
)
system.time(
    x1 <- sapply(1:reps, function(i){sum(rexp(n=nexps,rate=rate))})
)
system.time(
    x1 <- lapply(1:reps, function(i){sum(rexp(n=nexps,rate=rate))})
)

# when we apply a very simple function(e.g., a sum), the fastest way is often to just make a matrix fo all the simulations and then apply that function to the matix appropriately
system.time(
    x1 <- apply(matrix(rexp(n=nexps*reps,rate=rate), nrow=nexps),2,sum)
)
system.time(
    x1 <- colSums(matrix(rexp(n=nexps*reps,rate=rate), nrow=nexps))
)

head(x1)

require(ggplot2)
ggplot(data.frame(x1),aes(x1)) +
geom_histogram(aes(y=..density..)) +
stat_function(fun=function(x)dgamma(x,shape=nexps, scale=1/rate), color='red', size=2)

# generating normal random variables
samples = rnorm(1000, 0, 1)

# generating random mixtures of normal data
## ver1
sampa=rnorm(1000000,0,1)
sampb=rnorm(1500000,3,1)
combined=c(sampa,sampb)
plt=ggplot(data.frame(combined),aes(x=combined)) + stat_bin(binwidth=.25, position="identity")
plt

## ver2
pop1=rnorm(200000)
pop2=rnorm(100000,1,2)
combined=c(pop1,pop2)
plot=ggplot(data.frame(
    data=c(combined,pop1,pop2), 
    labels=rep(c("combined", "pop1", "pop2"), c(3e5,2e5,1e5))
    ), 
    aes(x=data)) +
    stat_bin(aes(fill=labels), position="identity", binwidth=.25, alpha=.5) +
    theme_classic()
plot

# monte carlo simulation
### 2 fair dice, prob. that their sum is at least 7
### simulating many throws of two fair dice, and then computing the fraction of those trials whose sum is at leat 7
### write a fuction that simulates the trials and returns TRUE if the sume is at least 7(we call this an event)
isEvent = function(numDice, numSides, targetValue, numTrials){
   apply(matrix(sample(1:numSides, numDice*numTrials, replace = T), nrow=numDice),2,sum) >= targetValue 
}
outcomes=isEvent(2,6,7,10000)
mean(outcomes)

# gamma mixture of poissons
### generate the means of the poisson distribution by sampling from a gamma dist.
lambdas = rgamma(10000, shape=2, scale=3)
samples = rep(0, 10000)
for (i in 1:10000){ samples[i] = rpois(1, lambdas[i])}

# power calculation
### the power of a statistical test is the prob. that the test rejects the null hypothesis it the alternative is true. 
### there is a rarely a closed form for the power, so we resort to simulation. 
### how many samples do we need to achieve a certain amount of power?


### how many samples are needed to distinguish between the means of two normal distribution?
### n(1, .5) vs n(2, .5) with a power of at least .8 at the 0.05 significance level.
compute_power = function(n, sigma, numTrials){
    sampa = matrix(rnorm(n*numTrials, 1, sigma), ncol=numTrials)
    sampb = matrix(rnorm(n*numTrials, 2, sigma), ncol=numTrials)
    statistics = (apply(sampa, 2, mean) - apply(sampb, 2, mean))/ sqrt(2*sigma^2/n) 
    return (mean(abs(statistics) >= qnorm(.975) ))
}

compute_power(3, .5, 10000)
compute_power(4, .5, 10000)
