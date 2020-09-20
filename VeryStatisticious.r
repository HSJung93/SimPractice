# Very statisticious
### https://aosmith.rbind.io/2018/04/23/simulate-simulate-part-2/#an-actual-mixed-model-with-fixed-effects-this-time

# part 1: linear model

## A single simulation from a two-group model
library(purrr) # v. 0.3.4
library(broom) # v. 0.5.6
library(dplyr) # v. 1.0.0
library(ggplot2) # v. 3.3.1

set.seed(20200920)

ngroup = 2
nrep = 10
b0 = 5
b1 = -2
sd = 2

( group = rep( c("group1", "group2"), each = nrep) )
( eps = rnorm(n = ngroup*nrep, mean = 0, sd = sd) )
( growth = b0 + b1*(group == "group2") + eps )
dat = data.frame(group, growth)

growthfit = lm(growth ~ group, data = dat)
summary(growthfit)

## Make a fuction for the simulation 
twogroup_fun = function(nrep = 10, b0 = 5, b1 = -2, sigma = 2) {
     ngroup = 2
     group = rep( c("group1", "group2"), each = nrep)
     eps = rnorm(n = ngroup*nrep, mean = 0, sd = sigma)
     growth = b0 + b1*(group == "group2") + eps
     simdat = data.frame(group, growth)
     growthfit = lm(growth ~ group, data = simdat)
     growthfit
}

set.seed(20200920)
twogroup_fun()

### changing the default values to the function arguments
twogroup_fun(sigma = 1)

## Repeat the simulation many times
### This is a task for the replicate() function, which repeatedly calls a function and saves the output. I use simplify = FALSE so the output is a list. This is convenient for going through to extract elements from the models later. Also see purrr::rerun() as a convenient version of replicate().
sims = replicate(n = 1000, twogroup_fun(), simplify = FALSE )
sims[[1]]

## Extracting results from the linear model
### To get the coefficients and statistical tests of coefficients we can use tidy() from package broom.
tidy(growthfit)
summary(growthfit)$sigma

## Extract simulation results
### To extract any results I’m interested in I will loop through the list of models, which I’ve stored in sims, and pull out the element of interest. I will use functions from the map family from package purrr for looping through the list of models. I’ll use functions from dplyr for any data manipulation and plot distributions via ggplot2.

### Estimated differences in mean response
sims %>%
     map_df(tidy) %>%
     filter(term == "groupgroup2") %>%
     ggplot( aes(x = estimate) ) +
          geom_density(fill = "blue", alpha = .5) +
          geom_vline( xintercept = -2)

### Some models even get the sign of the coefficient wrong. If we had only a single model from a single sample, as we would when collecting data rather than simulating data, we could very well get a fairly bad estimate for the value we are interested in. See Gelman and Carlin’s 2014 paper, Beyond Power Calculations: Assessing Type S (Sign) and Type M (Magnitude) Errors if you are interested in further discussion.

### Estimated standard deviation
sims %>%
     map_dbl(~summary(.x)$sigma) %>%
     data.frame(sigma = .) %>%
     ggplot( aes(x = sigma) ) +
          geom_density(fill = "blue", alpha = .5) +
          geom_vline(xintercept = 2)

### The standard deviation is underestimated a bit more than 50% of the time. This is not uncommon.
sims %>%
     map_dbl(~summary(.x)$sigma) %>%
     {. < 2} %>%
     mean()

## Extract hypothesis test results
### If the goal of a simulation is to get an idea of the statistical power of a test we could look at the proportion of times the null hypothesis was rejected given a fixed alpha level (often 0.05, but of course it can be something else).
sims %>%
     map_df(tidy) %>%
     filter(term == "groupgroup2") %>%
     pull(p.value) %>%
     {. <  0.05} %>%
     mean()
### pull() extract a column from a dataframe




# part 2: linear mixed model
### I’ve seen instances where residual autocorrelation isn’t detectable when I know it exists (because I simulated it) or I have skewed residuals and/or unequal variances when I simulated residuals from a normal distribution with a single variance.

## A single simulatino for the two-level model
set.seed(20200920)

### true params
nstand = 5
nplot = 4
mu = 10

### stand-level variance vs observation-level random effect variance
sds = 2
sd = 1

( stand = rep(LETTERS[1:nstand], each = nplot) )

### I can make a plot variable, as well, although it’s not needed for modeling since we have a single value per plot. It is fairly common to give plots the same name in each stand (i.e., plots are named 1-4 in each stand), but I’m a big believer in giving plots unique names.
( plot = letters[1:(nstand*nplot)] )
( standeff = rnorm(nstand, 0, sds) )
( standeff = rep(standeff, each = nplot) )
( ploteff = rnorm(nstand*nplot, 0, sd) )

( dat = data.frame(stand, standeff, plot, ploteff) )

( dat$resp = with(dat, mu + standeff + ploteff ) )

library(lme4) # v. 1.1-21

### The results for the estimated overall mean and standard deviations of random effects in this model look pretty similar to my defined parameter values.
fit1 = lme4::lmer(resp ~ 1 + (1|stand), data = dat)
fit1

## Make a function for the simulation  
twolevel_fun = function(nstand = 5, nplot = 4, mu = 10, sigma_s = 2, sigma = 1) {
     standeff = rep( rnorm(nstand, 0, sigma_s), each = nplot)
     stand = rep(LETTERS[1:nstand], each = nplot)
     ploteff = rnorm(nstand*nplot, 0, sigma)
     resp = mu + standeff + ploteff
     dat = data.frame(stand, resp)
     lme4::lmer(resp ~ 1 + (1|stand), data = dat)
}

set.seed(20200920)
twolevel_fun()

## Repeat the simulation many times
sims = replicate(100, twolevel_fun(), simplify = FALSE )
sims[[100]]

## Extract results from the linear mixed model
### !!! Error: No tidy method for objects of class lmerMod
library(broom) # v. 0.5.6

broom::tidy(fit1)

tidy(fit1, effects = "fixed")

tidy(fit1, effects = "ran_pars", scales = "vcov")

## Explore the effect of sample size on variance estimation
library(purrr) # v. 0.3.3
suppressPackageStartupMessages( library(dplyr) ) # v. 0.8.3
library(ggplot2) # v. 3.2.1

stand_sims = c(5, 20, 100) %>%
     set_names() %>%
     map(~replicate(1000, twolevel_fun(nstand = .x) ) )

stand_vars = stand_sims %>%
     modify_depth(2, ~tidy(.x, effects = "ran_pars", scales = "vcov") ) %>%
     map_dfr(bind_rows, .id = "stand_num") %>%
     filter(group == "stand")

head(stand_vars)
?isSingular
ggplot(stand_vars, aes(x = estimate) ) +
     geom_density(fill = "blue", alpha = .25) +
     facet_wrap(~stand_num) +
     geom_vline(xintercept = 4)

stand_vars = mutate(stand_vars, stand_num = forcats::fct_inorder(stand_num) )

add_prefix = function(string) {
     paste("Number stands:", string, sep = " ")
}

groupmed = stand_vars %>%
     group_by(stand_num) %>%
     summarise(mvar = median(estimate) )

ggplot(stand_vars, aes(x = estimate) ) + 
     geom_density(fill = "blue", alpha = .25) +
     facet_wrap(~stand_num, labeller = as_labeller(add_prefix) ) +
     geom_vline(aes(xintercept = 4, linetype = "True variance"), size = .5 ) +
     geom_vline(data = groupmed, aes(xintercept = mvar, linetype = "Median variance"),
                size = .5) +
     theme_bw(base_size = 14) +
     scale_linetype_manual(name = "", values = c(2, 1) ) +
     theme(legend.position = "bottom",
           legend.key.width = unit(.1, "cm") ) +
     labs(x = "Estimated Variance", y = NULL)

stand_vars %>%
     group_by(stand_num) %>%
     summarise_at("estimate", 
                  list(min = min, mean = mean, med = median, max = max) )

stand_vars %>%
     group_by(stand_num) %>%
     summarise(mean(estimate < 4) )

### Every time I do this sort of simulation I am newly surprised that even large samples tend to underestimate variances slightly more often than overestimate them.

## An actual mixed model(with fixed effects this time)
nstand = 5
nplot = 4
b0 = -1
b1 = .005
b2 = .1
sds = 2
sd = 1

set.seed(20200920)
stand = rep(LETTERS[1:nstand], each = nplot)
standeff = rep( rnorm(nstand, 0, sds), each = nplot)
ploteff = rnorm(nstand*nplot, 0, sd)

### I will simulate the explanatory variables by randomly drawing from uniform distributions via runif(). I change the minimum and maximum values of the uniform distribution as needed to get an appropriate spread for a given variable. If the distribution of your explanatory variables are more skewed you could use a different distribution (like the Gamma distribution).

### First I simulate values for elevation. This variable only five values, as it is a stand-level variable. I need to repeat each value for the four plots measured in each stand like I did when making the stand variable.
( elevation = rep( runif(nstand, 1000, 1500), each = nplot) )
( slope = runif(nstand*nplot, 2, 75) )
( resp2 = b0 + b1*elevation + b2*slope + standeff + ploteff )

### Now we can fit a mixed model for resp2 with elevation and slope as fixed effects, stand as the random effect and the residual error term based on plot-to-plot variation. (Notice I didn’t put these variables in a dataset, which I usually like to do to keep things organized and to avoid problems of vectors in my environment getting overwritten by mistake.)
lmer(resp2 ~ elevation + slope + (1|stand) )

# part 3: The Poisson edition
### working with a generalized linear mixed model for count data. This model was linear on the log scale. If something is quadratic on the log scale (the scale of the model), what does the relationship look like on the original scale (the scale of the data)?

### Instead of defining the distribution of the errors we’ll now directly define the distribution of the response variable. (For a more formal coverage of the statistical model for generalized linear (mixed) models see Stroup’s Rethinking the Analysis of Non-Normal Data in Plant and Soil Science.) 

### I’m going to use a Poisson generalized linear model for my simulation, so the response variable will be discrete counts. In my statistical model I first define a response variable that comes from the Poisson distribution.

### We will assume that the relationship between the mean of the response and any explanatory variables is linear on the log scale. This can be described as using a log link, since the log is the function that “links” the mean to the linear predictor. If you’re coming from the world of linear models you may be used to describing the relationship between the response variable and any explanatory variables, not the relationship between the mean of the response variable and explanatory variables.

## Code for a single simulation
b0 = .5
b1 = .5
b2 = 5

set.seed(20200920)
x = runif(100, min = 0, max = 1)
head(x) # First six values of x

lambda = exp(b0 + b1*x + b2*x^2)
head(lambda)

### It is this step where we add “Poisson errors” to the mean to generate the response variable. For a fixed x variable, the variation for each simulated y value around the mean is based on the Poisson variance. For linear model simulations we usually add variability to the mean by simulating the errors directly from a normal distribution with a mean of 0. Since the variance is based on the mean in the Poisson distribution, adding the variability isn’t so obvious. I’ve seen this referred to as adding “Poisson noise”, but “Poisson errors” may be a better term.
y = rpois(100, lambda = lambda) 
head(y)

plot(x, log(y))
plot(x, y)

# part4: A binomial generalized linear mixed model
### I will be simulating data “manually”. However, also see the simulate() function from package lme4. I find this function particularly useful if I want to simulate data based on a fitted model, but it can also be used in situations where you don’t already have a model.

library(lme4) # v. 1.1-23
library(purrr) # v. 0.3.4
library(ggplot2) # v. 3.3.2

### We assume that the relationship between the mean of the response and explanatory variables is linear on the logit scale so I use a logit link function when writing out the linear predictor. The logit is the same as the log odds.

### If you are newer to generalized linear mixed models you might want to take a moment and note of the absence of epsilon in the linear predictor.

## A single simulation for a binomial GLMM
### Note that y/num_samp is the proportion of plants that survived, which is what we are interested in. In binomial models in R you often use the number of successes and the number of failures (total trials minus the number of successes) as the response variable instead of the actual observed proportion.
set.seed(20200920)

### The estimate difference from the model will be expressed as odds, so I calculated the odds and then the difference in odds as an odds ratio based on my chosen proportions per group.
codds = .5/(1 - .5)
todds = .85/(1 - .85)
todds/codds

### Since the model is linear on the scale of log odds I took the log of the odds ratio above to figure out the additive difference between treatments on the model scale.
log(todds/codds)

### Since the model is linear on the scale of log odds I took the log of the odds ratio above to figure out the additive difference between treatments on the model scale.
log(codds)

b0 = 0
b1 = 1.735
site_var = 0.5
n_sites = 10

## Creating the study design variables
site = rep(LETTERS[1:n_sites], each = 2)
plot = paste(site, rep(1:2, times = n_sites), sep = "." )
treatment = rep( c("treatment", "control"), times = n_sites)
dat = data.frame(site, plot, treatment)
dat

## Simulate the random effect
( site_eff = rep( rnorm(n = n_sites, 
                        mean = 0, 
                        sd = sqrt(site_var) ), 
                  each = 2) )

## Calculate log odds
( log_odds = with(dat, b0 + b1*(treatment == "treatment") + site_eff ) )

## Convert log odds to proportions
### Right now I’ve gotten to the point where I have logit(p_t). To get the true proportions, p_t, I need to inverse-logit the log odds. In R, function plogis() performs the inverse logit.

( prop = plogis(log_odds) )
dat$num_samp = 50

### I’ve been in situations where I wanted the binomial sample size to vary per observation. In that case, you may find sample() useful, using the range of binomial sample sizes you are interested in as the first argument.

### num_samp = sample(40:50, size = 20, replace = TRUE)

## Generate the response variable
( dat$y = rbinom(n = n_sites*2, size = dat$num_samp, prob = prop) )

## Fit a model
mod = glmer(cbind(y, num_samp - y) ~ treatment + (1|site), 
            data = dat,
            family = binomial(link = "logit") )
mod

## Make a function for the simulation
bin_glmm_fun = function(n_sites = 10,
                        b0 = 0,
                        b1 = 1.735,
                        num_samp = 50,
                        site_var = 0.5) {
     site = rep(LETTERS[1:n_sites], each = 2)
     plot = paste(site, rep(1:2, times = n_sites), sep = "." )
     treatment = rep( c("treatment", "control"), times = n_sites)
     dat = data.frame(site, plot, treatment)           
     
     site_eff = rep( rnorm(n = n_sites, mean = 0, sd = sqrt(site_var) ), each = 2)
     
     log_odds = with(dat, b0 + b1*(treatment == "treatment") + site_eff)
     prop = plogis(log_odds)
     dat$num_samp = num_samp
     dat$y = rbinom(n = n_sites*2, size = num_samp, prob = prop)
     
     glmer(cbind(y, num_samp - y) ~ treatment + (1|site),
           data = dat,
           family = binomial(link = "logit") )
}

set.seed(20200920)
bin_glmm_fun()

## Repeat the simulation many times
sims = replicate(1000, bin_glmm_fun(), simplify = FALSE )
sims[[100]]

## Extract results from the binomial GLMM
### he sum of the squared Pearson residuals divided by the residual degrees of freedom is an estimate of over/underdispersion. This seems OK to use in the scenario I’ve set up here since my binomial sample sizes are fairly large and my proportions are not too close to the distribution limits.
overdisp_fun = function(model) {
     sum( residuals(model, type = "pearson")^2)/df.residual(model)
}
overdisp_fun(mod)

## Explore estimated dispersion
### I want to look at the distribution of dispersion estimates from the 1000 models. This involves looping through the models and using overdisp_fun() to extract the estimated dispersion from each one. I put the result in a data.frame since I’ll be plotting the result with ggplot2. I use purrr helper function map_dfr() for the looping.
alldisp = map_dfr(sims, ~data.frame(disp = overdisp_fun(.x) ) )

ggplot(alldisp, aes(x = disp) ) +
     geom_histogram(fill = "blue", 
                    alpha = .25, 
                    bins = 100) +
     geom_vline(xintercept = 1) +
     scale_x_continuous(breaks = seq(0, 2, by = 0.2) ) +
     theme_bw(base_size = 14) +
     labs(x = "Disperson",
          y = "Count")

mean(alldisp$disp > 1)

mean(alldisp$disp > 1.5)

### For this scenario, at least, I learned that it is rare to observe substantial overdispersion when the model isn’t overdispersed. That seems useful.