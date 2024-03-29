### This script goes through output from the high-dimensional multimodal
### mediation models, and summarizes the hyper-parameters and log likelihoods
###
### Ellyn Butler
### November 17, 2022 - January 23, 2023

kappa1=kappa2=kappa3=kappa4<-10^c(seq(-5,-3,length.out=3),seq(-3,0,length.out=11)[-1],seq(0,2,length.out=6)[-1])
mu.prod<-c(0,0.1,0.5,1,2,Inf)

final_df <- read.csv('/projects/b1108/projects/violence_mediation/data/combined_data.csv')
logn = log(nrow(final_df))

################################## Full Model ##################################

re_long <- readRDS('/projects/b1108/projects/violence_mediation/models/viol_re_scaled_long_fulldata_fullmatch.rds')

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

bic_df <- data.frame(matrix(ncol=13, nrow=length(kappa1)*length(mu.prod)))
names(bic_df) <- c('kappa', 'm', 'muprod', 'p', 'tot_ie_m1', 'tot_ie_m2',
                   'tot_ie_m1m2', 'tot', 'neg2loglik', 'carda1', 'carda2',
                   'carda3', 'bic')

q = 1
for (m in 1:length(kappa1)) {
  for (p in 1:length(mu.prod)) {
    ############################ Overall effects ###############################

    #print('Overall effects')
    tot_ie_m1 <- re_long[[p]][[m]]$beta%*%re_long[[p]][[m]]$theta
    #print(paste('M1', tot_ie_m1))
    tot_ie_m2 <- re_long[[p]][[m]]$zeta%*%re_long[[p]][[m]]$pi
    #print(paste('M2', tot_ie_m2))
    tot_ie_m1m2 <- 0

    for (j in 1:8) {
      for (k in 1:length(grep('region', names(final_df)))) {
        tot_ie_m1m2 <- tot_ie_m1m2 + re_long[[p]][[m]]$beta[j]*re_long[[p]][[m]]$Lambda[j, k]*re_long[[p]][[m]]$pi[k]
      }
    }
    #print(paste('M1M2', tot_ie_m1m2))

    # Total effect (c)... try with data too
    tot <- tot_ie_m1 + tot_ie_m2 + tot_ie_m1m2 + re_long[[p]][[m]]$delta
    #print(paste('Total', tot))

    bic_df[q, 'tot_ie_m1'] <- tot_ie_m1
    bic_df[q, 'tot_ie_m2'] <- tot_ie_m2
    bic_df[q, 'tot_ie_m1m2'] <- tot_ie_m1m2
    bic_df[q, 'tot'] <- tot

    ############################ Overall effects ###############################

    bic_df[q, 'kappa'] <- kappa1[m]
    bic_df[q, 'm'] <- m
    bic_df[q, 'muprod'] <- mu.prod[p]
    bic_df[q, 'p'] <- p
    bic_df[q, 'neg2loglik'] <- -2*re_long[[p]][[m]]$logLik$sum
    bic_df[q, 'carda1'] <- sum(re_long[[p]][[m]]$IE.M1 != 0)
    bic_df[q, 'carda2'] <- sum(re_long[[p]][[m]]$IE.M2 != 0)
    bic_df[q, 'carda3'] <- sum(re_long[[p]][[m]]$IE.M1M2 != 0)
    bic_df[q, 'bic'] <- bic_df[q, 'neg2loglik'] + logn*(bic_df[q, 'carda1'] + bic_df[q, 'carda2'] + bic_df[q, 'carda3'])

    q = q+1
  }
  #q = q+1... OOPS (January 23, 2023)
}

######################## Optimal model according to BIC? #######################

bic_df[bic_df$bic %in%  min(bic_df$bic, na.rm=TRUE),]

# What are the non-zero paths?
p <- bic_df[bic_df$bic %in%  min(bic_df$bic, na.rm=TRUE), 'p']
m <- bic_df[bic_df$bic %in%  min(bic_df$bic, na.rm=TRUE), 'm']

important_immune <- re_long[[p]][[m]]$IE.M1[re_long[[p]][[m]]$IE.M1 != 0] #IL10
important_regs <- re_long[[p]][[m]]$IE.M2[re_long[[p]][[m]]$IE.M2 != 0]

# Effect sizes
re_long[[1]][[1]]$IE.M1[names(important_immune)]
# M1.1
# 0.001756765
re_long[[1]][[1]]$IE.M2[names(important_regs)]
# M2.2        M2.12       M2.116       M2.130       M2.188       M2.235
# 0.003796597 -0.082205693 -0.052561131 -0.053436294 -0.028493162  0.139652100
# M2.248       M2.257
# 0.023960599  0.024509353

# X -> M1 -> Y
re_long[[1]][[1]]$beta[,names(important_immune)]
#0.170415
re_long[[1]][[1]]$theta[names(important_immune),]
#0.01030875

# X -> M2 -> Y
re_long[[1]][[1]]$zeta[,names(important_regs)]
# M2.2        M2.12      M2.116     M2.130     M2.188     M2.235     M2.248
# -0.1390642  0.1797810  0.2130487  0.1785623  0.1994847  0.3016011  0.2112760
# M2.257
# -0.1579374
re_long[[1]][[1]]$pi[names(important_regs),]
# M2.2        M2.12       M2.116      M2.130      M2.188       M2.235
# -0.02730104 -0.45725459 -0.24670948 -0.29925852 -0.14283380  0.46303581
# M2.248     M2.257
# 0.11340899 -0.15518400

# Region mapping
map_df <- data.frame(matnames = names(re_long[[p]][[m]]$IE.M2),
                     regnames = names(final_df)[17:ncol(final_df)])

important_df <- map_df[map_df$matnames %in% names(important_regs), ]
