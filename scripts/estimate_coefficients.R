### This script gets the unpenalized coefficient estimates for the variables
### selected using the high-dimensional multi-modal mediation model.
###
### Ellyn Butler
### December 6, 2022 - March 22, 2023

set.seed(2023)

df <- read.csv('/projects/b1108/projects/violence_mediation/data/combined_data.csv')

df2 <- df[, c('subid', 'ever', 'RCADS_sum', 'IL10', paste0('region', c(2, 14, 237, 261, 281)))]
df2[, c('RCADS_sum', 'IL10', paste0('region', c(2, 14, 237, 261, 281)))] <- scale(df2[, c('RCADS_sum', 'IL10', paste0('region', c(2, 14, 237, 261, 281)))])
#df2$ever <- df2$ever - mean(df2$ever) #unnecessary

m1_x_mod <- lm(IL10 ~ ever, data=df2)

m2.2_xm1_mod <- lm(region2 ~ ever + IL10, data=df2)
m2.14_xm1_mod <- lm(region14 ~ ever + IL10, data=df2)
m2.237_xm1_mod <- lm(region237 ~ ever + IL10, data=df2)
m2.261_xm1_mod <- lm(region261 ~ ever + IL10, data=df2)
m2.281_xm1_mod <- lm(region281 ~ ever + IL10, data=df2)

y_xm1m2_mod <- lm(RCADS_sum ~ ever + IL10 + region2 + region14 + region237 + region261 + region281, data=df2)

# January 31, 2023: MINIMALLY PENALIZED MODEL DID NOT CONVERGE

##### Just through IL10
betatheta <- m1_x_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['IL10']]
# Maximally penalized:  0.0440, 0.0049
# Minimally penalized:  0.1814, 0.0361
# Unpenalized:          0.1814, 0.0892
# Significance: -, -
# lavaan (correct?):    0.181,  0.089

##### Just through region2 (primary visual cortex)
zetapi_2 <- m2.2_xm1_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['region2']]
# Maximally penalized:  -0.1116, 0.0274
# Minimally penalized:  -0.2019, -0.0186
# Unpenalized:          -0.2501, 0.1288
# Significance: -/+, +
# lavaan (correct?):    -0.250,  0.129

##### Just through region14 (inferior temporal gyrus)
zetapi_14 <- m2.14_xm1_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['region14']]
# Maximally penalized:  0.0196, -0.0413
# Minimally penalized:  0.1920, -0.4548
# Unpenalized:          0.1711, -0.1572
# Significance: -, +
# lavaan (correct?):    0.171,  -0.157

##### Just through region237 (hippocampus)
zetapi_237 <- m2.237_xm1_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['region237']]
# Maximally penalized:  0.1606, 0.0443
# Minimally penalized:  0.2985, 0.4676
# Unpenalized:          0.2935, 0.2009
# Significance: +, +
# lavaan (correct?):    0.293,  0.201

##### Just through region261 (pallidum)
zetapi_261 <- m2.261_xm1_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['region261']]
# Maximally penalized:  -0.0459, -0.1506
# Minimally penalized:  -0.1922, -0.1503
# Unpenalized:          -0.1876, -0.2267
# Significance: -, +
# lavaan (correct?):    -0.188,  -0.227

##### Just through region281 (medial inferior cerebellum)
zetapi_281 <- m2.281_xm1_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['region281']]
# Maximally penalized:  -0.1281, 0.0217
# Minimally penalized:  -0.2939, 0.1643
# Unpenalized:          -0.2897, 0.1007
# Significance: +, -
# lavaan (correct?):    -0.290,  0.101



######################### Using the `mediation` package ########################


########################### Using the `lavaan` package #########################
library('lavaan')


###### IL10
il10_model <- "
  IL10 ~ a1 * ever
  region2 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region2 + b3 * region14 + b4 * region237 + b5 * region261 + b6 * region281
  ind_eff := a1 * b1
"

il10_fit <- lavaan::sem(model = il10_model, data = df2, se = "boot", bootstrap = 5000)
#lavaan WARNING: 9 bootstrap runs failed or did not converge.

il10_est <- lavaan::parameterEstimates(il10_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#29   ind_eff :=     a1*b1 ind_eff  0.016 0.022  0.723  0.470   -0.006    0.091

save(il10_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/il10_fit.RData')
save(il10_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/il10_est.RData')

###### region2
region2_model <- "
  IL10 ~ a1 * ever
  region2 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region2 + b3 * region14 + b4 * region237 + b5 * region261 + b6 * region281
  ind_eff := a2 * b2
"

region2_fit <- lavaan::sem(model = region2_model, data = df2, se = "boot", bootstrap = 5000)

region2_est <- lavaan::parameterEstimates(region2_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#29   ind_eff :=     a2*b2 ind_eff -0.032 0.023 -1.414  0.157   -0.092    0.00029

save(region2_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region2_fit.RData')
save(region2_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region2_est.RData')

###### region14
region14_model <- "
  IL10 ~ a1 * ever
  region14 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region14 + b3 * region2 + b4 * region237 + b5 * region261 + b6 * region281
  ind_eff := a2 * b2
"

region14_fit <- lavaan::sem(model = region14_model, data = df2, se = "boot", bootstrap = 5000)

region14_est <- lavaan::parameterEstimates(region14_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#29   ind_eff :=     a2*b2 ind_eff -0.027 0.025 -1.079  0.280   -0.093    0.008

save(region14_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region14_fit.RData')
save(region14_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region14_est.RData')

###### region237 - hippocampus
region237_model <- "
  IL10 ~ a1 * ever
  region237 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region237 + b3 * region14 + b4 * region2 + b5 * region261 + b6 * region281
  ind_eff := a2 * b2
"

region237_fit <- lavaan::sem(model = region237_model, data = df2, se = "boot", bootstrap = 5000)

region237_est <- lavaan::parameterEstimates(region237_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#29   ind_eff :=     a2*b2 ind_eff  0.059 0.032  1.867  0.062    0.009    0.134!!!!!!!!!!!!!!!!!

save(region237_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region237_fit.RData')
save(region237_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region237_est.RData')

###### region261 - pallidum
region261_model <- "
  IL10 ~ a1 * ever
  region261 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region261 + b3 * region14 + b4 * region237 + b5 * region2 + b6 * region281
  ind_eff := a2 * b2
"

region261_fit <- lavaan::sem(model = region261_model, data = df2, se = "boot", bootstrap = 5000)

region261_est <- lavaan::parameterEstimates(region261_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#29   ind_eff :=     a2*b2 ind_eff  0.043 0.034  1.238  0.216   -0.009    0.127

save(region261_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region261_fit.RData')
save(region261_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region261_est.RData')

###### region281
region281_model <- "
  IL10 ~ a1 * ever
  region281 ~ a2 * ever + d21 * IL10
  RCADS_sum ~  cp * ever + b1 * IL10 + b2 * region281 + b3 * region14 + b4 * region237 + b5 * region261 + b6 * region2
  ind_eff := a2 * b2
"

region281_fit <- lavaan::sem(model = region281_model, data = df2, se = "boot", bootstrap = 5000)

region281_est <- lavaan::parameterEstimates(region281_fit, boot.ci.type = "bca.simple")
#         lhs op       rhs   label    est    se      z pvalue ci.lower ci.upper
#29   ind_eff :=     a2*b2 ind_eff -0.029 0.025 -1.188  0.235   -0.099    0.002

save(region281_fit, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region281_fit.RData')
save(region281_est, file = '/projects/b1108/projects/violence_mediation/models/lavaan_output/region281_est.RData')


#
