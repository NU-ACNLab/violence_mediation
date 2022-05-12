### This script runs a much simpler mediation model just to get a basic
### understanding of appropriate sensitivity analyses
### Based on David MacKinnon's "Tutorial in Modern Mediation Analysis"
###
### Ellyn Butler
### April 27, 2022


# Load data
viol_df <- read.csv('~/Documents/Northwestern/projects/violence_mediation/data/violence.csv')
dep_df <- read.csv('~/Documents/Northwestern/projects/violence_mediation/data/dep_immune.csv')
#immune_df <- read.csv('')
amyg_df <- read.csv('~/Documents/Northwestern/projects/violence_mediation/data/amygconn_2021-12-12.csv')

final_df <- merge(viol_df, dep_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, amyg_df, by=c('subid', 'sesid'))

mini_df <- final_df[final_df$sesid == 1 & !is.na(final_df$ever), c('subid', 'ever', 'IL6', 'region200', 'RCADS_sum')]

names(mini_df) <- c('subid', 'violence', 'IL6', 'amygconn', 'anxiety')
row.names(mini_df) <- 1:nrow(mini_df)

data1 <- mini_df

# Scale continuous variables
mini_df$IL6 <- scale(mini_df$IL6)
mini_df$amygconn <- scale(mini_df$amygconn)
mini_df$anxiety <- scale(mini_df$anxiety)

############################# One Mediator Model #############################
xy_mod <- lm(anxiety ~ violence, data=mini_df)
xm1_mod <- lm(IL6 ~ violence, data=mini_df)
xm1y_mod <- lm(anxiety ~ IL6 + violence, data=mini_df)

# Mediated effect
# a * b
med_prod <- xm1_mod$coefficients[['violence']]*xm1y_mod$coefficients[['IL6']]
# c - c'
med_diff <- xy_mod$coefficients[['violence']] - xm1y_mod$coefficients[['violence']]
# QUESTION: Why aren't these ^ equal?

dir_effect <- xm1y_mod$coefficients[['violence']]

tot_effect <- med_prod + xm1y_mod$coefficients[['violence']]
tot_effect2 <- xy_mod$coefficients[['violence']]
# QUESTION: Why aren't these ^ equal?

a <- xm1_mod$coefficients[['violence']]
b <- xm1y_mod$coefficients[['IL6']]
se_a <- summary(xm1_mod)$coefficients[2, 2]
se_b <- summary(xm1y_mod)$coefficients[2, 2]
se_ab <- sqrt(a^2*se_b^2 + b^2*se_a^2)

ci_lower <- med_prod - 1.96*se_ab
ci_higher <- med_prod + 1.96*se_ab

### Bootstrap CI


########## Assumptions

### 1.) No unmeasured X to Y confounders given covariates.

### 2.) No unmeasured M to Y confounders given covariates.

### 3.) No unmeasured X to M confounders given covariates

### 4.) There is no effect of X that confounders the M to Y relation.

### Note! Randomized X satisfies Assumptions 1 and 3 but not 2 and 4


# Questions
# 1.) p66?


########################### Parallel Mediators Model ###########################






xm2_mod <- lm(amygconn ~ violence, data=mini_df)
