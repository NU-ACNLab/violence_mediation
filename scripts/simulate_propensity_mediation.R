### Simulate propensity score weighting using mediation models
###
### Ellyn Butler
### November 11, 2022 - November 18, 2022

library('MASS')
library('MatchIt')

set.seed(2022)

sigma <- matrix(c(1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1), nrow=4, ncol=4)
mu <- rep(0, 4)
sim_df <- mvrnorm(100, mu, sigma)
sim_df <- data.frame(sim_df)

names(sim_df) <- c('C', 'X', 'M', 'Y')
sim_df$X <- ifelse(sim_df$X < -.4, 1, 0)


################# W/o matching

xonm_mod <- lm(M ~ X, sim_df)
xmony_mod <- lm(Y ~ X + M, sim_df)
a <- xonm_mod$coefficients[2]
b <- xmony_mod$coefficients[3]
a*b #-0.3913207


################# W/ matching

######## w/ weights options

full_match <- matchit(X ~ C, data=sim_df, method='full', interactive=FALSE)

pdf('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/plots/matching/simulation_full_match.pdf')
plot(summary(full_match))
plot(full_match, type='jitter', interactive=FALSE)
plot(full_match, type='qq', interactive=FALSE)
dev.off()

xonm_mod_matched <- lm(M ~ X, sim_df, weights=full_match$weights)
xmony_mod_matched <- lm(Y ~ X + M, sim_df, weights=full_match$weights)
a_matched <- xonm_mod_matched$coefficients[2]
b_matched <- xmony_mod_matched$coefficients[3]
a_matched*b_matched #-0.330242


######## w/ manual weights

sim_df$X_weighted <- sim_df$X*sqrt(full_match$weights)
sim_df$M_weighted <- sim_df$M*sqrt(full_match$weights)
sim_df$Y_weighted <- sim_df$Y*sqrt(full_match$weights)
sim_df$sqrt_weights <- sqrt(full_match$weights)

xonm_mod_matched_manual <- lm(M_weighted ~ X_weighted - 1 + sqrt_weights, sim_df)
xmony_mod_matched_manual <- lm(Y_weighted ~ X_weighted + M_weighted - 1 + sqrt_weights, sim_df)
a_matched_manual <- xonm_mod_matched_manual$coefficients[1]
b_matched_manual <- xmony_mod_matched_manual$coefficients[2]
a_matched_manual*b_matched_manual #-0.330242 
