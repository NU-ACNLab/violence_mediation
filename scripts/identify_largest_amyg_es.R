### This script identifies the largest amygdala effect sizes for the IE2 model
###
### Ellyn Butler
### May 20, 2022

re <- readRDS('/projects/b1108/projects/violence_mediation/models/viol_re_mono_long.rds')


re_long[[1]][[17]]$IE.M2[re_long[[1]][[17]]$IE.M2 > .04]
# 1: -56.16_-44.76_-24.23
# 32: 10.09_-17.10_74.14
# 47: 36.04_-9.44_13.95
# 214: 51.52_-32.52_7.55
# 294: -33.00_-51.00_-50.00
