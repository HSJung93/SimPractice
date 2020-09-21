# Panel Data Simulation for GSynth
### https://www.cambridge.org/core/journals/political-analysis/article/generalized-synthetic-control-method-causal-inference-with-interactive-fixed-effects-models/B63A8BD7C239DD4141C67DA10CD0E4F3

N = 50
N_tr = 5
T = 30
T_0= 20
w = .8

(Units.factor = rep(as.character(1:N) , T))
(Times.factor = rep(as.character(1:T), each=N))
(Treat.factor = c( rep("control", N*T_0) , rep(c(rep("treatment", N_tr) , rep("control", (N - N_tr))), (T-T_0))))
(FactorTime.factor = )
(FactorUnit.factor = )