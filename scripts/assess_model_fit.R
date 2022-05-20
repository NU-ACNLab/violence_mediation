### This script goes through output from the high-dimensional multimodal
### mediation models, and summarizes the hyper-parameters and log likelihoods
###
### Ellyn Butler
### May 20, 2022

kappa1=kappa2=kappa3=kappa4<-10^c(seq(-5,-3,length.out=3),seq(-3,0,length.out=11)[-1],seq(0,2,length.out=6)[-1])
mu.prod<-c(0,0.1,0.5,1,2,Inf)


################################## Full Model ##################################

re_long <- readRDS('/projects/b1108/projects/violence_mediation/models/viol_re_mono_long.rds')

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


write.csv(full_df, '/projects/b1108/projects/violence_mediation/models/viol_re_mono_long_summary.csv', row.names=FALSE)



################################## Mini Models ##################################


for (m in 1:4) {
  full_df <- expand.grid(kappa1, mu.prod)
  names(full_df) <- c('kappa1', 'muprod')

  full_df$logLik_M1 <- NA
  full_df$logLik_M2 <- NA
  full_df$logLik_Y <- NA
  full_df$logLik_sum <- NA
  full_df$converge <- NA

  re <- readRDS(paste0('/projects/b1108/projects/violence_mediation/models/viol_re_2_50_', m, '.rds'))

  j=1
  k=1
  for (i in 1:nrow(full_df)) {
    full_df[i, 'logLik_M1'] <- re[[j]][[k]]$logLik$M1
    full_df[i, 'logLik_M2'] <- re[[j]][[k]]$logLik$M2
    full_df[i, 'logLik_Y'] <- re[[j]][[k]]$logLik$Y
    full_df[i, 'logLik_sum'] <- re[[j]][[k]]$logLik$sum
    full_df[i, 'converge'] <- re[[j]][[k]]$converge
    j=j+1
    if (j > length(mu.prod)) { j=1 }
    k=k+1
    if (k > length(kappa1)) { k=1 }
  }
  assign(paste0('full_df', m), full_df)

  write.csv(full_df, paste0('/projects/b1108/projects/violence_mediation/models/viol_re_summary_', m, '.csv'), row.names=FALSE)
}


#### Max log likelihood

full_df[full_df$logLik_sum == max(full_df$logLik_sum), ]

full_df1[full_df1$logLik_sum == max(full_df1$logLik_sum), ]
full_df2[full_df2$logLik_sum == max(full_df2$logLik_sum), ]
full_df3[full_df3$logLik_sum == max(full_df3$logLik_sum), ]
full_df4[full_df4$logLik_sum == max(full_df4$logLik_sum), ]
