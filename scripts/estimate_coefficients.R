### This script gets the unpenalized coefficient estimates for the variables
### selected using the high-dimensional multi-modal mediation model.
###
### Ellyn Butler
### December 6, 2022

df <- read.csv('/projects/b1108/projects/violence_mediation/data/combined_data.csv')

df <- df[, c('subid', 'ever', 'RCADS_sum', 'IL10', paste0('region', c(2, 14, 237, 261, 281)))]
df[, c('RCADS_sum', 'IL10', paste0('region', c(2, 14, 237, 261, 281)))] <- scale(df[, c('RCADS_sum', 'IL10', paste0('region', c(2, 14, 237, 261, 281)))])

m1_x_mod <- lm(IL10 ~ ever, data=df)

m2.2_xm1_mod <- lm(region2 ~ ever + IL10, data=df)
m2.14_xm1_mod <- lm(region14 ~ ever + IL10, data=df)
m2.237_xm1_mod <- lm(region237 ~ ever + IL10, data=df)
m2.261_xm1_mod <- lm(region261 ~ ever + IL10, data=df)
m2.281_xm1_mod <- lm(region281 ~ ever + IL10, data=df)

y_xm1m2_mod <- lm(RCADS_sum ~ ever + IL10 + region2 + region14 + region237 + region261 + region281, data=df)

# Just through IL10
betatheta <- m1_x_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['IL10']]
# Minimally penalized:  0.1814, 0.0361
# Unpenalized:          0.1814, 0.0892

# Just through region2 (primary visual cortex)
zetapi_2 <- m2.2_xm1_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['region2']]
# Minimally penalized:  -0.2019, -0.0186
# Unpenalized:          -0.2501, 0.1288
# ^ sign flipped... why?
# A: Possibly because we aren't controlling for all the other M2 variables?

# Just through region14 (inferior temporal gyrus)
zetapi_14 <- m2.14_xm1_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['region14']]
# Minimally penalized:  0.1920, -0.4548
# Unpenalized:          0.1711, -0.1572
# ^ Why are the unpenalized coefficients smaller in magnitude?
# A: Possibly because we aren't controlling for all the other M1 variables?

# Just through region237 (hippocampus)
zetapi_237 <- m2.237_xm1_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['region237']]
# Minimally penalized:  0.2985, 0.4676
# Unpenalized:          0.2935, 0.2009
# ^ Why are the unpenalized coefficients smaller in magnitude?

# Just through region261 (pallidum)
zetapi_261 <- m2.261_xm1_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['region261']]
# Minimally penalized:  -0.1922, -0.1503
# Unpenalized:          -0.1876, -0.2267

# Just through region281 (medial inferior cerebellum)
zetapi_281 <- m2.281_xm1_mod$coefficients[['ever']]*y_xm1m2_mod$coefficients[['region281']]
# Minimally penalized:  -0.2939, 0.1643
# Unpenalized:          -0.2897, 0.1007
