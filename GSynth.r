# Panel Data Simulation for GSynth
### https://www.cambridge.org/core/journals/political-analysis/article/generalized-synthetic-control-method-causal-inference-with-interactive-fixed-effects-models/B63A8BD7C239DD4141C67DA10CD0E4F3

# Setting params for panel and treatment informations
### 50 Units and 30 Time periods panel data will be simulated. 5 Units out of 50 units and 10 out of 30 time periods are treated. 
N = 50
N_tr = 5
T = 30
T_0= 20

# Making fixed effects and treatment variables
### time fixed effects are made up of normal dist. Actually, normal dist. is used when simulating random effects. 
(Units.factor = rep(as.character(1:N) , T))
(Units = rep(runif(N, -sqrt(3), sqrt(3)), T))
(Times.factor = rep(1:T, each=N))
(Times = rep(rnorm(T, 0, 1), each = N) )
(Treat.factor = c( rep("control", N*T_0) , rep(c(rep("treatment", N_tr) , rep("control", (N - N_tr))), (T-T_0))))

###(lambda_i1 = runif(1, -sqrt(3), sqrt(3)) ) for check
###(lambda_i2 = runif(1, -sqrt(3), sqrt(3)) )

# Making preliminary dataframe with basic factor variables
dat1 = data.frame(Units.factor, Units, Times.factor, Times, Treat.factor)

# Let's add the unit fixed effect for the treated with uniform distribution as the way illustrated in the paper
dat1$Units[dat1$Treat.factor == "treatment"] = runif(1, sqrt(3) - 2*w*sqrt(3) , 3*sqrt(3) - 2*w*sqrt(3) )

# Making two factors and two factor loadings
## factors are made of uniform distribution with weight w = .8. it represent uncertainty difference between treated and control data 
w = .8
dat1$lambda_i1 = rep(0, N*T)
dat1$lambda_i1 = rep(runif(N, sqrt(3) - 2*w*sqrt(3) , 3*sqrt(3) - 2*w*sqrt(3) ), T)
dat1$lambda_i1[dat1$Treat.factor == "treatment"] = rep(runif(N_tr, -sqrt(3), -sqrt(3)), T-T_0)

dat1$lambda_i2 = rep(0, N*T)
dat1$lambda_i2 = rep(runif(N, sqrt(3) - 2*w*sqrt(3) , 3*sqrt(3) - 2*w*sqrt(3) ), T)
dat1$lambda_i2[dat1$Treat.factor == "treatment"] = rep(runif(N_tr, -sqrt(3), -sqrt(3)), T-T_0)

## make error term with normal dist. for simulating treatment effects
e = rnorm(N*(T-T_0), 0, 1)

## make treatment effect delta. treatment starts at period 20
dat1$delta = rep(0, N*T)
dat1$delta = c( rep(0, N*T_0) , rep(1:10, each = N) + e )
dat1$delta

## factors are made up of normal distribution
dat1$f_1t = rep(rnorm(N, 0, 1), each = T)
dat1$f_2t = rep(rnorm(N, 0, 1), each = T)

# Simulate X from factor and factor loadings
## setting params and errors for simulating x variables with the other varaibles
b1 = 1
b2 = 3
eta1 = rnorm(N*T, 0, 1)
eta2 = rnorm(N*T, 0, 1)

dat1$x1 = with(dat1, 1 + lambda_i1*f_1t + lambda_i2*f_2t + lambda_i1 + lambda_i2 + f_1t + f_2t + eta1 )
dat1$x2 = with(dat1, 1 + lambda_i1*f_1t + lambda_i2*f_2t + lambda_i1 + lambda_i2 + f_1t + f_2t + eta2 )

# Simulate Y from error e and all the other variables generated previously. 
eps = rnorm(N*T, 0, 1)
dat1$Y = with(dat1, delta*(Treat.factor == "treatment") + x1*b1 + x2*b2 + lambda_i1*f_1t + lambda_i2*f_2t + Units + Times + 5 + eps )

# Plot x and Y variables
plot(dat1$x1, dat1$Y)