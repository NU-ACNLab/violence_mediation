### This script goes through output from the high-dimensional multimodal
### mediation models, and summarizes the hyper-parameters and log likelihoods
###
### Ellyn Butler
### May 20, 2022 - August 16, 2022

library('ggplot2')
library('ggpubr')

kappa1=kappa2=kappa3=kappa4<-10^c(seq(-5,-3,length.out=3),seq(-3,0,length.out=11)[-1],seq(0,2,length.out=6)[-1])
mu.prod<-c(0,0.1,0.5,1,2,Inf)

viol_df <- read.csv('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/data/violence.csv')
dep_df <- read.csv('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/data/dep_immune.csv')
mono_df <- read.csv('/Users/flutist4129/Documents/Northwestern/studies/mwmh/data/processed/immune/monocytes.csv')
amyg_df <- read.csv('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/data/amygconn_2021-12-12.csv')

final_df <- merge(viol_df, dep_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, mono_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, amyg_df, by=c('subid', 'sesid'))

final_df <- final_df[!is.na(final_df$ever) & !is.na(final_df$RCADS_sum) &
  final_df$sesid == 1 & !is.na(final_df$IL6) & !is.na(final_df$ClassicalMono) &
  !is.na(final_df$NonClassicalMono), ]
logn = log(nrow(final_df))

################################## Full Model ##################################

#re_long <- readRDS('/projects/b1108/projects/violence_mediation/models/viol_re_scaled_long.rds')
re_long <- readRDS('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/models/viol_re_scaled_long.rds')

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


#write.csv(full_df, '/projects/b1108/projects/violence_mediation/models/viol_re_scaled_long_summary.csv', row.names=FALSE)


full_df[full_df$logLik_sum == max(full_df$logLik_sum), ]
# ^ Choose parameter combination based on log Lik here
# kappa1 = 1 implies that there is little penalty on the pathway effects


bic_df <- data.frame(matrix(ncol=13, nrow=length(kappa1)*length(mu.prod)))
names(bic_df) <- c('kappa', 'm', 'muprod', 'p', 'tot_ie_m1', 'tot_ie_m2',
                   'tot_ie_m1m2', 'tot', 'neg2loglik', 'carda1', 'carda2',
                   'carda3', 'bic')

q = 1
for (m in 1:length(kappa1)) {
  #print(paste('kappa =', kappa1[m]))

  ### When does a cell become 0? Start with smallest when mu.prod = 0
  # ^ Is there a turning point where many of them become 0?
  # How quickly are the values decreasing?
  # Make a line plot of this
  #print(sum(re_long[[1]][[m]]$IE.M1M2 == 0)/(nrow(re_long[[1]][[m]]$IE.M1M2)*ncol(re_long[[1]][[m]]$IE.M1M2)))
  #print(sum(re_long[[2]][[m]]$IE.M1M2 == 0)/(nrow(re_long[[2]][[m]]$IE.M1M2)*ncol(re_long[[2]][[m]]$IE.M1M2)))
  #print(sum(re_long[[3]][[m]]$IE.M1M2 == 0)/(nrow(re_long[[3]][[m]]$IE.M1M2)*ncol(re_long[[3]][[m]]$IE.M1M2)))
  #print(sum(re_long[[4]][[m]]$IE.M1M2 == 0)/(nrow(re_long[[4]][[m]]$IE.M1M2)*ncol(re_long[[4]][[m]]$IE.M1M2)))
  #print(sum(re_long[[5]][[m]]$IE.M1M2 == 0)/(nrow(re_long[[5]][[m]]$IE.M1M2)*ncol(re_long[[5]][[m]]$IE.M1M2))) # changes rapidly here
  #print(sum(re_long[[6]][[m]]$IE.M1M2 == 0)/(nrow(re_long[[6]][[m]]$IE.M1M2)*ncol(re_long[[6]][[m]]$IE.M1M2)))

  coeff_df <- data.frame(ID=1:(8*300*length(mu.prod)), kappa1=rep(kappa1[m], 8*300*length(mu.prod)),
                        mu.prod=factor(c(rep(mu.prod[1], 8*300), rep(mu.prod[2], 8*300),
                                  rep(mu.prod[3], 8*300), rep(mu.prod[4], 8*300),
                                  rep(mu.prod[5], 8*300), rep(mu.prod[6], 8*300))),
                        Immune=rep(c(rep(1, 300), rep(2, 300), rep(3, 300), rep(4, 300),
                                   rep(5, 300), rep(6, 300), rep(7, 300), rep(8, 300)),
                                   length(mu.prod)),
                        Amygdala=rep(1:300, 8*length(mu.prod)),
                        coefficient=c(re_long[[1]][[m]]$IE.M1M2[1,], re_long[[1]][[m]]$IE.M1M2[2,],
                                      re_long[[1]][[m]]$IE.M1M2[3,], re_long[[1]][[m]]$IE.M1M2[4,],
                                      re_long[[1]][[m]]$IE.M1M2[5,], re_long[[1]][[m]]$IE.M1M2[6,],
                                      re_long[[1]][[m]]$IE.M1M2[7,], re_long[[1]][[m]]$IE.M1M2[8,],
                                      re_long[[2]][[m]]$IE.M1M2[1,], re_long[[2]][[m]]$IE.M1M2[2,],
                                      re_long[[2]][[m]]$IE.M1M2[3,], re_long[[2]][[m]]$IE.M1M2[4,],
                                      re_long[[2]][[m]]$IE.M1M2[5,], re_long[[2]][[m]]$IE.M1M2[6,],
                                      re_long[[2]][[m]]$IE.M1M2[7,], re_long[[2]][[m]]$IE.M1M2[8,],
                                      re_long[[3]][[m]]$IE.M1M2[1,], re_long[[3]][[m]]$IE.M1M2[2,],
                                      re_long[[3]][[m]]$IE.M1M2[3,], re_long[[3]][[m]]$IE.M1M2[4,],
                                      re_long[[3]][[m]]$IE.M1M2[5,], re_long[[3]][[m]]$IE.M1M2[6,],
                                      re_long[[3]][[m]]$IE.M1M2[7,], re_long[[3]][[m]]$IE.M1M2[8,],
                                      re_long[[4]][[m]]$IE.M1M2[1,], re_long[[4]][[m]]$IE.M1M2[2,],
                                      re_long[[4]][[m]]$IE.M1M2[3,], re_long[[4]][[m]]$IE.M1M2[4,],
                                      re_long[[4]][[m]]$IE.M1M2[5,], re_long[[4]][[m]]$IE.M1M2[6,],
                                      re_long[[4]][[m]]$IE.M1M2[7,], re_long[[4]][[m]]$IE.M1M2[8,],
                                      re_long[[5]][[m]]$IE.M1M2[1,], re_long[[5]][[m]]$IE.M1M2[2,],
                                      re_long[[5]][[m]]$IE.M1M2[3,], re_long[[5]][[m]]$IE.M1M2[4,],
                                      re_long[[5]][[m]]$IE.M1M2[5,], re_long[[5]][[m]]$IE.M1M2[6,],
                                      re_long[[5]][[m]]$IE.M1M2[7,], re_long[[5]][[m]]$IE.M1M2[8,],
                                      re_long[[6]][[m]]$IE.M1M2[1,], re_long[[6]][[m]]$IE.M1M2[2,],
                                      re_long[[6]][[m]]$IE.M1M2[3,], re_long[[6]][[m]]$IE.M1M2[4,],
                                      re_long[[6]][[m]]$IE.M1M2[5,], re_long[[6]][[m]]$IE.M1M2[6,],
                                      re_long[[6]][[m]]$IE.M1M2[7,], re_long[[6]][[m]]$IE.M1M2[8,]))

  coeff_df$Path <- paste(coeff_df$Immune, coeff_df$Amygdala, sep='_')

  coeff_plot <- ggplot(coeff_df, aes(x = mu.prod, y = coefficient, group = Path)) +
    geom_point(alpha=0.5) + theme_linedraw() + geom_line(alpha=0.3)


  pdf(paste0('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/plots/kappa', m, '_coefficients.pdf'), width=6, height=6)
  #pdf(paste0('/projects/b1108/projects/violence_mediation/plots/kappa', m, '_coefficients.pdf'), width=6, height=6)
  coeff_plot
  dev.off()

  ################################# To Zero Plots ################################

  coeff_df$zero_pt1pt512inf <- 0
  coeff_df$zero_pt512inf <- 0
  coeff_df$zero_12inf <- 0
  coeff_df$zero_2inf <- 0
  for (i in 1:nrow(coeff_df)) {
    path <- coeff_df[i, 'Path']
    pt1_coeff <- coeff_df[coeff_df$Path == path & coeff_df$mu.prod == '0.1', 'coefficient']
    pt5_coeff <- coeff_df[coeff_df$Path == path & coeff_df$mu.prod == '0.5', 'coefficient']
    one_coeff <- coeff_df[coeff_df$Path == path & coeff_df$mu.prod == '1', 'coefficient']
    two_coeff <- coeff_df[coeff_df$Path == path & coeff_df$mu.prod == '2', 'coefficient']
    inf_coeff <- coeff_df[coeff_df$Path == path & coeff_df$mu.prod == 'Inf', 'coefficient']
    if (pt1_coeff + pt5_coeff + one_coeff + two_coeff + inf_coeff == 0) {
      coeff_df[i, 'zero_pt1pt512inf'] <- 1
    } else if (pt5_coeff + one_coeff + two_coeff + inf_coeff == 0) {
      coeff_df[i, 'zero_pt512inf'] <- 1
    } else if (one_coeff + two_coeff + inf_coeff == 0) {
      coeff_df[i, 'zero_12inf'] <- 1
    } else if (two_coeff + inf_coeff == 0) {
      coeff_df[i, 'zero_2inf'] <- 1
    }
  }

  # paths that are 0 for both 2 and infinity
  zero_2inf_df <- coeff_df[coeff_df$zero_2inf == 1, ]

  coeff_plot0_2inf <- ggplot(zero_2inf_df, aes(x = mu.prod, y = coefficient, group = Path)) +
    geom_point(alpha=0.5) + theme_linedraw() + geom_line(alpha=0.3) +
    ggtitle('Coefficients 0 starting at mu.prod = 2')

  ## 1+2+Inf
  zero_12inf_df <- coeff_df[coeff_df$zero_12inf == 1, ]

  coeff_plot0_12inf <- ggplot(zero_12inf_df, aes(x = mu.prod, y = coefficient, group = Path)) +
    geom_point(alpha=0.5) + theme_linedraw() + geom_line(alpha=0.3) +
    ggtitle('Coefficients 0 starting at mu.prod = 1')

  ## 0.5+1+2+Inf
  zero_pt512inf_df <- coeff_df[coeff_df$zero_pt512inf == 1, ]

  coeff_plot0_pt512inf <- ggplot(zero_pt512inf_df, aes(x = mu.prod, y = coefficient, group = Path)) +
    geom_point(alpha=0.5) + theme_linedraw() + geom_line(alpha=0.3) +
    ggtitle('Coefficients 0 starting at mu.prod = .5')

  ## 0.1+0.5+1+2+Inf
  zero_pt1pt512inf_df <- coeff_df[coeff_df$zero_pt1pt512inf == 1, ]

  coeff_plot0_pt1pt512inf <- ggplot(zero_pt1pt512inf_df, aes(x = mu.prod, y = coefficient, group = Path)) +
    geom_point(alpha=0.5) + theme_linedraw() + geom_line(alpha=0.3) +
    ggtitle('Coefficients 0 starting at mu.prod = .1')

  #pdf(paste0('/projects/b1108/projects/violence_mediation/plots/kappa', m, '_coefficients_zero.pdf'), width=10, height=10)
  pdf(paste0('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/plots/kappa', m, '_coefficients_zero.pdf'), width=10, height=10)
  ggarrange(coeff_plot0_2inf, coeff_plot0_12inf,
            coeff_plot0_pt512inf, coeff_plot0_pt1pt512inf, nrow=2, ncol=2)
  dev.off()

  for (p in 1:length(mu.prod)) {
    ############################ Overall effects ###############################

    #print('Overall effects')
    tot_ie_m1 <- re_long[[p]][[m]]$beta%*%re_long[[p]][[m]]$theta
    #print(paste('M1', tot_ie_m1))
    tot_ie_m2 <- re_long[[p]][[m]]$zeta%*%re_long[[p]][[m]]$pi
    #print(paste('M2', tot_ie_m2))
    tot_ie_m1m2 <- 0

    for (j in 1:8) {
      for (k in 1:300) {
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

  }
  q = q+1
}

######################## Optimal model according to BIC? #######################

bic_df[bic_df$bic %in%  min(bic_df$bic, na.rm=TRUE),] # eek

# What are the non-zero paths?

p <- bic_df[bic_df$bic %in%  min(bic_df$bic, na.rm=TRUE), 'p']
m <- bic_df[bic_df$bic %in%  min(bic_df$bic, na.rm=TRUE), 'm']

re_long[[p]][[m]]$IE.M2[re_long[[p]][[m]]$IE.M2 != 0] #M2.1, M2.32, M2.35, M2.294 





#
