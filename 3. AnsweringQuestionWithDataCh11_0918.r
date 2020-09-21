# Chapter 11. Simulating Data
### There are some problems out there where we don't have really good analytic math formulas to tell us the correct answer.
set.seed(20200918)

# An independent samples t-test. 
group_A <- rnorm(10,100,7)
group_B <- rnorm(10,105,7)
t.test(group_A, group_B, var.equal = T)

# replicate p-values with n=10
save_ps <- length(1000)
for(i in 1:1000){
    group_A <- rnorm(10,100,7)
    group_B <- rnorm(10,105,7)
    t_results <- t.test(group_A, group_B, var.equal = T)
    save_ps[i] <- t_results$p.value
}

prop_p <- length(save_ps[save_ps<.05])/1000
print(prop_p)

# replicate p-values with n=50
save_ps <- length(1000)
for(i in 1:1000){
    group_A <- rnorm(50,100,7)
    group_B <- rnorm(50,105,7)
    t_results <- t.test(group_A, group_B, var.equal = T)
    save_ps[i] <- t_results$p.value
}

prop_p <- length(save_ps[save_ps<.05])/1000
print(prop_p)

# vary the number of subjects from 10 to 50, and vary the size of the effect from 0 to 20 in steps of 4
num_sims        <-500
N               <-c(10,20,30,40,50)
mean_difference <-c(0,4,8,12,16,20)
save_ps<-length(num_sims)

all_df<-data.frame()

for(diff in mean_difference){
  for (j in N){
    for(i in 1:num_sims){
      group_A <- rnorm(j,100,7)
      group_B <- rnorm(j,100+diff, 7)
      t_results <- t.test(group_A,group_B,var.equal = TRUE)
      save_ps[i] <- t_results$p.value
    }
    sim_df <-data.frame(save_ps,
                        num_subjects=as.factor(rep(j,num_sims)),
                        mean_diff =rep(diff,num_sims))
    all_df <- rbind(all_df,sim_df)
  }
}  

library(dplyr)

plot_df <- all_df %>%
            dplyr::group_by(num_subjects,mean_diff) %>%
            dplyr::summarise(proportion_sig = length(save_ps[save_ps<0.05])/num_sims)

library(tidyverse)
ggplot(plot_df, aes(x=mean_diff, 
                    y=proportion_sig, 
                    group=num_subjects, 
                    color=num_subjects))+
  geom_point()+
  geom_line()+
  theme_classic()

### When the mean diference is 0, we should get an average of 5%, or (0.05 proportion) experiments being significant. This is what we expect by chance, and it doesn’t matter how many subjects we run. When there is no difference, we will reject the null 5% of the time (these would all be type 1 errors).

# simulation ANOVA
N <- 10
groups <- rep(c("A","B","C"), each=10)
DV <- c(rnorm(100,10,15),   # means for group A
        rnorm(100,10,15),   # means for group B
        rnorm(100,20,15)    # means for group C
        )
sim_df<-data.frame(groups,DV)

aov_results <- summary(aov(DV~groups, sim_df))

library(xtable)
knitr::kable(xtable(aov_results))

# In this next example, we simulate the same design 100 times, save the p-values, and the determine the proportion of significant simulations.

N <- 10
save_p<-length(100)

for(i in 1:100){
  groups <- rep(c("A","B","C"), each=10)
  DV <- c(rnorm(100,10,15),   # means for group A
          rnorm(100,10,15),   # means for group B
          rnorm(100,20,15)    # means for group C
          )
  sim_df<-data.frame(groups,DV)
  
  aov_results <- summary(aov(DV~groups, sim_df))
  save_p[i]<-aov_results[[1]]$`Pr(>F)`[1]
}

length(save_p[save_p<0.05])/100