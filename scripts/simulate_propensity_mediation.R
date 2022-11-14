### Simulate propensity score weighting using mediation models
###
### Ellyn Butler
### November 11, 2022

library('MASS')
library('MatchIt')

sigma <- matrix(c(1, .5, .5, .5, 1, .5, .5, .5, 1), nrow=3, ncol=3)
sim_df <- mvrnorm(100, )
