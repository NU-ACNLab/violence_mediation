### This script goes through output from the high-dimensional multimodal
### mediation models, and summarizes the hyper-parameters and log likelihoods
###
### Ellyn Butler
### August 16, 2022

library('ggplot2')
library('ggpubr')

kappa1=kappa2=kappa3=kappa4<-c(150, 200, 250, 300, 1000) #all coefficients are 0, so clearly 150 is too large for the kappas
mu.prod<-c(0,0.1,0.5,1,2,Inf)


################################## Full Model ##################################

re_long <- readRDS('/projects/b1108/projects/violence_mediation/models/viol_re_mono_kappaext_extralong.rds')
#re_long <- readRDS('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/models/viol_re_mono_kappaext_extralong.rds')

full_df <- expand.grid(kappa1, mu.prod)
names(full_df) <- c('kappa1', 'muprod')

full_df$logLik_M1 <- NA
full_df$logLik_M2 <- NA
full_df$logLik_Y <- NA
full_df$logLik_sum <- NA
full_df$converge <- NA

j=1
k=1
for (i in 1:nrow(full_df)) {
  full_df[i, 'logLik_M1'] <- re_long[[j]][[k]]$logLik$M1
  full_df[i, 'logLik_M2'] <- re_long[[j]][[k]]$logLik$M2
  full_df[i, 'logLik_Y'] <- re_long[[j]][[k]]$logLik$Y
  full_df[i, 'logLik_sum'] <- re_long[[j]][[k]]$logLik$sum
  full_df[i, 'converge'] <- re_long[[j]][[k]]$converge
  j=j+1
  if (j > length(mu.prod)) { j=1 }
  k=k+1
  if (k > length(kappa1)) { k=1 }
}


write.csv(full_df, '/projects/b1108/projects/violence_mediation/models/viol_re_mono_kappaext_extralong_summary.csv', row.names=FALSE)


full_df[full_df$logLik_sum == max(full_df$logLik_sum), ]
# ^ Choose parameter combination based on log Lik here
# kappa1 = 1 implies that there is little penalty on the pathway effects

# percent zeros
sum(re_long[[1]][[6]]$IE.M1M2 == 0)
sum(re_long[[1]][[6]]$IE.M1M2 == 0)/(nrow(re_long[[1]][[6]]$IE.M1M2)*ncol(re_long[[1]][[6]]$IE.M1M2))

### When does a cell become 0? Start with smallest when mu.prod = 0
# They are all zero... whoops
# Make a line plot of this
sum(re_long[[1]][[1]]$IE.M1M2 == 0)/(nrow(re_long[[1]][[1]]$IE.M1M2)*ncol(re_long[[1]][[1]]$IE.M1M2))
sum(re_long[[1]][[2]]$IE.M1M2 == 0)/(nrow(re_long[[1]][[2]]$IE.M1M2)*ncol(re_long[[1]][[2]]$IE.M1M2))
sum(re_long[[1]][[3]]$IE.M1M2 == 0)/(nrow(re_long[[1]][[3]]$IE.M1M2)*ncol(re_long[[1]][[3]]$IE.M1M2))
sum(re_long[[1]][[4]]$IE.M1M2 == 0)/(nrow(re_long[[1]][[4]]$IE.M1M2)*ncol(re_long[[1]][[4]]$IE.M1M2))
sum(re_long[[1]][[5]]$IE.M1M2 == 0)/(nrow(re_long[[1]][[5]]$IE.M1M2)*ncol(re_long[[1]][[5]]$IE.M1M2)) # changes rapidly here
