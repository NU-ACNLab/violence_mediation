### This script identifies the largest amygdala effect sizes for the IE2 model
###
### Ellyn Butler
### May 20, 2022 - May 27, 2022

re <- readRDS('/projects/b1108/projects/violence_mediation/models/viol_re_scaled_long.rds')

######## X -> M2 -> Y

re[[1]][[1]]$IE.M2[re[[1]][[1]]$IE.M2 > .04]
# 14: 0.05_-14.53_46.74
# 29: 3.45_-17.44_58.45
# 32: 10.09_-17.10_74.14
# 214: 51.52_-32.52_7.55
# 296: 7.50_-72.00_-39.00


# OLD
# 1: -56.16_-44.76_-24.23
# 32: 10.09_-17.10_74.14
# 47: 36.04_-9.44_13.95
# 214: 51.52_-32.52_7.55
# 294: -33.00_-51.00_-50.00


######## X -> M1 -> M2 -> Y

re[[1]][[1]]$IE.M1M2[re[[1]][[1]]$IE.M1M2 > .01]

round(re[[1]][[1]]$IE.M1M2, digits=4) == 0.0152 # M1.8, M2.14
round(re[[1]][[1]]$IE.M1M2, digits=4) == 0.0133 # M1.8, M2.63
round(re[[1]][[1]]$IE.M1M2, digits=4) == 0.0109 # M1.8, M2.188

# 14: 0.05_-14.53_46.74
# 63: 42.05_-0.39_47.10
# 188: 31.83_14.37_55.98
