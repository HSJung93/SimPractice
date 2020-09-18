# R progarmming for data science book Ch.20
set.seed(20200918)

# generalized linear model: Poisson log linear model
x <- rnorm(100)
log.mu <- .5+.3*x
y <- rpois(100, exp(log.mu))
plot(x,y)