
# Panel Data Simulation for PanelMatch
### https://imai.fas.harvard.edu/research/files/tscs.pdf

set.seed(20200920)

N = 162
T = 51
Z_bin_prob = .5

(X_zero = rep("control", n = N*T))
(Units = rep(runif(N, 0, 1),T))
(Units.factor = rep(as.character(1:N) , T))
(Times = rep(runif(T, 0, 1), each = N))
(Times.factor = rep(as.character(1:T), each=N))
(Z_conti = runif(n = N*T, min=0, max=1))
(Z_bin = rbinom(n = N*T, size = 1, prob = Z_bin_prob))

lag_vector = function(vector){
    lag_output = c(0, vector[1:(length(vector)-1)])
    lag_output
}

Z_conti.lag = lag_vector(Z_conti)
Z_bin.lag = lag_vector(Z_bin)
Z_conti.lag2 = lag_vector(Z_conti.lag)
Z_bin.lag2 = lag_vector(Z_bin.lag)

X = rep(0, N*T) 
X.lag = rep(0, N*T)
X.lag2 = rep(0, N*T)

dat.stage1 = data.frame(Units, Units.factor, Times, Times.factor, Z_conti, Z_bin, Z_conti.lag, Z_bin.lag, Z_conti.lag2, Z_bin.lag2, X, X.lag, X.lag2)

ksi.zc = -.06
psi.zb = .03
rho = .01
beta.x = - 0.75

treatment_generator = function(dat.stage1, t ){

    for (i in 1:t){
        start = (t-1)*N + 1
        end = t*N 

        lagged = with(dat.stage1[start:end, ], Units + Times + ksi.zc*Z_conti + psi.zb*Z_bin + rho*ksi.zc*Z_conti.lag + rho*psi.zb*Z_bin.lag + rho^2*ksi.zc*Z_conti.lag2 + rho^2*psi.zb*Z_bin.lag2 + beta.x*X.lag + rho*beta.x*X.lag2 )

        inverse.logit = plogis(lagged)

        dat.stage1[start:end, ]$X = rbinom(n = N, size = 1, prob = inverse.logit)
        dat.stage1[(start+N):(end+N),]$X.lag = dat.stage1[start:end, ]$X
        dat.stage1[(start+2*N):(end+2*N),]$X.lag2 = dat.stage1[start:end, ]$X
    }

    dat.stage1
}

dat.output = treatment_generator(dat.stage1, T)
dat.y = dat.output[1:(N*T), ]

$eps = rnorm(N*T, 0, 1)

ksi.zc.y = -.6
psi.zb.y = .3
rho.y = .4
beta.y = -7.5

dat.y$Y = with(dat.y, Units + Times + ksi.zc.y*Z_conti + psi.zb.y*Z_bin + rho.y*ksi.zc.y*Z_conti.lag + rho.y*psi.zb.y*Z_bin.lag + rho.y^2*ksi.zc.y*Z_conti.lag2 + rho^2*psi.zb.y*Z_bin.lag2 + beta.y*X + rho.y*beta.y*X.lag + rho.y^2*beta.y*X.lag2 )

plot(dat.y$X, dat.y$Y)