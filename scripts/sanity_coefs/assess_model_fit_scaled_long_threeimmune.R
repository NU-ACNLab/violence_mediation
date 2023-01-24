### This script goes through output from the high-dimensional multimodal
### mediation models, and summarizes the hyper-parameters and log likelihoods
###
### Ellyn Butler
### January 24, 2023

kappa1=kappa2=kappa3=kappa4<-10^c(seq(-5,-3,length.out=3),seq(-3,0,length.out=11)[-1],seq(0,2,length.out=6)[-1])
mu.prod<-c(0,0.1,0.5,1,2,Inf)

final_df <- read.csv('/projects/b1108/projects/violence_mediation/data/combined_data.csv')
logn = log(nrow(final_df))

################################## Full Model ##################################

re_long <- readRDS('/projects/b1108/projects/violence_mediation/models/viol_re_scaled_long_fulldata_threeimmune.rds')

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

    # NOTE: j should iterate over the indices of all the included immune variables
    for (j in 1:2) {
      # NOTE: k should iterate over the indices of all the included amygconn variables
      for (k in 1:5) {
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
#       M1.1
#0.006553306
re_long[[1]][[1]]$IE.M2[names(important_regs)]
#        M2.2        M2.12       M2.235       M2.257       M2.274
# 0.003744796 -0.087345027  0.139565645  0.028881367 -0.048287221

# X -> M1 -> Y
re_long[[1]][[1]]$beta[,names(important_immune)]
#0.1813547
re_long[[1]][[1]]$theta[names(important_immune),]
#0.0361353

# X -> M2 -> Y
re_long[[1]][[1]]$zeta[,names(important_regs)]
#M2.2      M2.12     M2.235     M2.257     M2.274
#-0.2018542  0.1920313  0.2984655 -0.1921725 -0.2939074
re_long[[1]][[1]]$pi[names(important_regs),]
#       M2.2       M2.12      M2.235      M2.257      M2.274
#-0.01855199 -0.45484779  0.46761072 -0.15028880  0.16429398

# Region mapping
map_df <- data.frame(matnames = names(re_long[[p]][[m]]$IE.M2),
                     regnames = names(final_df)[17:ncol(final_df)])

important_df <- map_df[map_df$matnames %in% names(important_regs), ]


############################# Get model of interest ############################

# Model that includes three immune variables?
unique(bic_df$carda1)
carda1_df <- bic_df[which(bic_df$carda1 == 3), ]

if (nrow(carda1_df) == 1) {
  row <- rownames(carda1_df)
} else {
  print('investigate')
}

# What are the non-zero paths?
p <- bic_df[row, 'p']
m <- bic_df[row, 'm']

important_immune <- re_long[[p]][[m]]$IE.M1[re_long[[p]][[m]]$IE.M1 != 0] #IL10, Basophils
important_regs <- re_long[[p]][[m]]$IE.M2[re_long[[p]][[m]]$IE.M2 != 0]



#
