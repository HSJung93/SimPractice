
# Panel Data Simulation for PanelMatch
### https://imai.fas.harvard.edu/research/files/tscs.pdf

set.seed(20200920)

# Making Z and Fixed Effect Variables
## setting params for basic variables
N = 162
T = 51
Z_bin_prob = .5

## simulating r.v
(X_zero = rep("control", n = N*T))

## unit fixed effect
(Units = rep(runif(N, 0, 1),T))
(Units.factor = rep(as.character(1:N) , T))

## time fixed effect
### time factor is distirbuted with """EACH""" value of the seq(1:T). it matters when making x variables from previous z and fixed effect variables
(Times = rep(runif(T, 0, 1), each = N))
(Times.factor = rep(as.character(1:T), each=N))

## covariate conti. bin.
(Z_conti = runif(n = N*T, min=0, max=1))
(Z_bin = rbinom(n = N*T, size = 1, prob = Z_bin_prob))

## function for making lag variables
lag_vector = function(vector){
    lag_output = c(0, vector[1:(length(vector)-1)])
    lag_output
}

## lag variables
Z_conti.lag = lag_vector(Z_conti)
Z_bin.lag = lag_vector(Z_bin)
Z_conti.lag2 = lag_vector(Z_conti.lag)
Z_bin.lag2 = lag_vector(Z_bin.lag)

# Simulating X Varibles
## zero vector for simulating x variables
### we will make x variables from previous z and fixed effect variables
X = rep(0, N*T) 
X.lag = rep(0, N*T)
X.lag2 = rep(0, N*T)

## make dataframe from variables above
dat.stage1 = data.frame(Units, Units.factor, Times, Times.factor, Z_conti, Z_bin, Z_conti.lag, Z_bin.lag, Z_conti.lag2, Z_bin.lag2, X, X.lag, X.lag2)

## setting params for making X variables form Z and Fixed effect variables.
### coefficient of z conti.
ksi.zc = -.06

### coeff. of z bin.
psi.zb = .03

### coeff. of time correlation 
rho = .01

### coeff. of x
beta.x = - 0.75

## make x variables from previous z and fixed effect variables
treatment_generator = function(dat.stage1, t ){

    for (i in 1:t){

        ### setting params for making x variables from previous z and fixed effect variables. 
        ### because time factor is distributed with N times EACH values of seq 1:T, we can make x variables from  
        start = (t-1)*N + 1
        end = t*N 

        ### because X variables are made of zero vectors temporally, whe can use lag variables of X even in first data gernerating stage.
        lagged = with(dat.stage1[start:end, ], Units + Times + ksi.zc*Z_conti + psi.zb*Z_bin + rho*ksi.zc*Z_conti.lag + rho*psi.zb*Z_bin.lag + rho^2*ksi.zc*Z_conti.lag2 + rho^2*psi.zb*Z_bin.lag2 + beta.x*X.lag + rho*beta.x*X.lag2 )

        ### X which is a vector of binary treatment values can be generated with inverse logit fucntion and binomial fucntion 
        inverse.logit = plogis(lagged)
        dat.stage1[start:end, ]$X = rbinom(n = N, size = 1, prob = inverse.logit)
        
        dat.stage1[(start+N):(end+N),]$X.lag = dat.stage1[start:end, ]$X
        dat.stage1[(start+2*N):(end+2*N),]$X.lag2 = dat.stage1[start:end, ]$X # X's values of period t is the same with X.lags2's values of (t+2N) period after.
    }

    ### the output will be the dataframe with generated X variables
    dat.stage1
}

dat.output = treatment_generator(dat.stage1, T)

# Simulating Y Variables
## subsetting dataframe
### the length of the dat.output exceeds the length of the input dataframe because of the lag terms, so we make new dataframe dat.y with right length.
dat.y = dat.output[1:(N*T), ]

## making epsilon for simulating Y from the other varibles
dat.y$eps = rnorm(N*T, 0, 1)

## setting params for simulating Y form X and Z and Fixed effects terms.
ksi.zc.y = -.6
psi.zb.y = .3
rho.y = .4
beta.y = -7.5

dat.y$Y = with(dat.y, Units + Times + ksi.zc.y*Z_conti + psi.zb.y*Z_bin + rho.y*ksi.zc.y*Z_conti.lag + rho.y*psi.zb.y*Z_bin.lag + rho.y^2*ksi.zc.y*Z_conti.lag2 + rho^2*psi.zb.y*Z_bin.lag2 + beta.y*X + rho.y*beta.y*X.lag + rho.y^2*beta.y*X.lag2 + eps)
### eps was omitted in previous code. eps was added at 20200924

# Plot the treatment variable and the dependant variable.
plot(dat.y$X, dat.y$Y)