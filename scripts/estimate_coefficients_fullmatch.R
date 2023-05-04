### This script gets the unpenalized coefficient estimates for the variables
### selected using the high-dimensional multi-modal mediation model.
###
### Ellyn Butler
### February 6, 2023 - March 22, 2023

library('lavaan')

set.seed(2023)

df <- read.csv('/projects/b1108/projects/violence_mediation/data/combined_data.csv')

full_match <- readRDS(file='/projects/b1108/projects/violence_mediation/models/matching/fullmatch.rds')

regions <- paste0('region', c(2, 14, 118, 132, 190, 237, 252, 261))

df2 <- df[, c('subid', 'ever', 'RCADS_sum', 'IL10', regions)]
df2[, c('RCADS_sum', 'IL10', regions)] <- scale(df2[, c('RCADS_sum', 'IL10', regions)])

df2$ever <- df2$ever*sqrt(full_match$weights)
df2[, regions] <- df2[, regions]*sqrt(full_match$weights)
df2$RCADS_sum <- df2$RCADS_sum*sqrt(full_match$weights)


###### IL10
il10_model <- "
  IL10 ~ a1 * ever
  region2 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region2 + b3 * region14 + b4 * region118 \
    + b5 * region132 + b6 * region190 + b7 * region237 + b8 * region252 + b9 * region261
  ind_eff := a1 * b1
"

il10_fit <- lavaan::sem(model = il10_model, data = df2, se = "boot", bootstrap = 5000)
#sampling.weights = full_match$weights,

il10_est <- lavaan::parameterEstimates(il10_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#53   ind_eff :=     a1*b1 ind_eff  0.021 0.024  0.897  0.370   -0.004    0.100

save(il10_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/il10_fit_fullmatch.RData')
save(il10_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/il10_est_fullmatch.RData')

###### region2
region2_model <- "
  IL10 ~ a1 * ever
  region2 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region2 + b3 * region14 + b4 * region118 \
    + b5 * region132 + b6 * region190 + b7 * region237 + b8 * region252 + b9 * region261
  ind_eff := a2 * b2
"

region2_fit <- lavaan::sem(model = region2_model, data = df2, se = "boot", bootstrap = 5000)

region2_est <- lavaan::parameterEstimates(region2_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#53   ind_eff :=     a2*b2 ind_eff -0.017 0.023 -0.767  0.443   -0.083    0.013

save(region2_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region2_fit_fullmatch.RData')
save(region2_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region2_est_fullmatch.RData')

###### region14
region14_model <- "
  IL10 ~ a1 * ever
  region14 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region14 + b3 * region2 + b4 * region118 \
    + b5 * region132 + b6 * region190 + b7 * region237 + b8 * region252 + b9 * region261
  ind_eff := a2 * b2
"

region14_fit <- lavaan::sem(model = region14_model, data = df2, se = "boot", bootstrap = 5000)

region14_est <- lavaan::parameterEstimates(region14_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#53   ind_eff :=     a2*b2 ind_eff -0.023 0.025 -0.896  0.370   -0.095    0.007

save(region14_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region14_fit_fullmatch.RData')
save(region14_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region14_est_fullmatch.RData')

###### region118 -
region118_model <- "
  IL10 ~ a1 * ever
  region118 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region118 + b3 * region2 + b4 * region14 \
    + b5 * region132 + b6 * region190 + b7 * region237 + b8 * region252 + b9 * region261
  ind_eff := a2 * b2
"

region118_fit <- lavaan::sem(model = region118_model, data = df2, se = "boot", bootstrap = 5000)

region118_est <- lavaan::parameterEstimates(region118_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#53   ind_eff :=     a2*b2 ind_eff -0.050 0.039 -1.285  0.199   -0.164   -0.002!!!!!!!!!!!!!!!!

save(region118_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region118_fit_fullmatch.RData')
save(region118_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region118_est_fullmatch.RData')

###### region132
region132_model <- "
  IL10 ~ a1 * ever
  region132 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region132 + b3 * region2 + b4 * region14 \
    + b5 * region118 + b6 * region190 + b7 * region237 + b8 * region252 + b9 * region261
  ind_eff := a2 * b2
"

region132_fit <- lavaan::sem(model = region132_model, data = df2, se = "boot", bootstrap = 5000)

region132_est <- lavaan::parameterEstimates(region132_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#53   ind_eff :=     a2*b2 ind_eff -0.018 0.031 -0.576  0.564   -0.111    0.013

save(region132_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region132_fit_fullmatch.RData')
save(region132_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region132_est_fullmatch.RData')

###### region190
region190_model <- "
  IL10 ~ a1 * ever
  region190 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region190 + b3 * region2 + b4 * region14 \
    + b5 * region118 + b6 * region132 + b7 * region237 + b8 * region252 + b9 * region261
  ind_eff := a2 * b2
"

region190_fit <- lavaan::sem(model = region190_model, data = df2, se = "boot", bootstrap = 5000)

region190_est <- lavaan::parameterEstimates(region190_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#53   ind_eff :=     a2*b2 ind_eff  0.046 0.037  1.239  0.215   -0.006    0.153

save(region190_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region190_fit_fullmatch.RData')
save(region190_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region190_est_fullmatch.RData')

###### region237 - hippocampus
region237_model <- "
  IL10 ~ a1 * ever
  region237 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region237 + b3 * region2 + b4 * region14 \
    + b5 * region118 + b6 * region132 + b7 * region190 + b8 * region252 + b9 * region261
  ind_eff := a2 * b2
"

region237_fit <- lavaan::sem(model = region237_model, data = df2, se = "boot", bootstrap = 5000)

region237_est <- lavaan::parameterEstimates(region237_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#53   ind_eff :=     a2*b2 ind_eff  0.068 0.041  1.649  0.099    0.008    0.170!!!!!!!!!!!!!!

save(region237_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region237_fit_fullmatch.RData')
save(region237_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region237_est_fullmatch.RData')

###### region252
region252_model <- "
  IL10 ~ a1 * ever
  region252 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region252 + b3 * region2 + b4 * region14 \
    + b5 * region118 + b6 * region132 + b7 * region190 + b8 * region237 + b9 * region261
  ind_eff := a2 * b2
"

region252_fit <- lavaan::sem(model = region252_model, data = df2, se = "boot", bootstrap = 5000)

region252_est <- lavaan::parameterEstimates(region252_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#53   ind_eff :=     a2*b2 ind_eff  0.006 0.020  0.295  0.768   -0.016    0.071

save(region252_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region252_fit_fullmatch.RData')
save(region252_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region252_est_fullmatch.RData')

###### region261
region261_model <- "
  IL10 ~ a1 * ever
  region261 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region261 + b3 * region2 + b4 * region14 \
    + b5 * region118 + b6 * region132 + b7 * region190 + b8 * region237 + b9 * region252
  ind_eff := a2 * b2
"

region261_fit <- lavaan::sem(model = region261_model, data = df2, se = "boot", bootstrap = 5000)

region261_est <- lavaan::parameterEstimates(region261_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#53   ind_eff :=     a2*b2 ind_eff  0.029 0.030  0.973  0.331   -0.012    0.108

save(region261_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region261_fit_fullmatch.RData')
save(region261_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region261_est_fullmatch.RData')

#
