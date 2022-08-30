
# load data
viol_df <- read.csv('/projects/b1108/projects/violence_mediation/data/violence.csv')
dep_df <- read.csv('/projects/b1108/projects/violence_mediation/data/dep_immune.csv')
mono_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/immune/monocytes.csv')
amyg_df <- read.csv('/projects/b1108/projects/violence_mediation/data/amygconn_2021-12-12.csv')
age_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/demographic/age_visits_2022-07-26.csv')
demo_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/demographic/demographics_2022-08-22.csv')
#ses_df <- read.csv()
# variables I need in here: age, sex, race, puberty, BMI, SES
# ideally also: Confusion, Hubbub, and Order Scale, the Brody Parenting Scale,
# the Harter Social Support Scale, and the Physical Activity and Exercise Scale

final_df <- merge(viol_df, dep_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, mono_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, amyg_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, age_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, demo_df, by=c('subid', 'sesid'))

final_df <- final_df[!is.na(final_df$ever) & !is.na(final_df$RCADS_sum) &
  final_df$sesid == 1 & !is.na(final_df$IL6) & !is.na(final_df$ClassicalMono) &
  !is.na(final_df$NonClassicalMono), ]

X <- final_df$ever
Y <- scale(final_df$RCADS_sum)
M1 <- scale(as.matrix(final_df[, c('IL10', 'IL6', 'IL8', 'TNFa', 'CRP', 'uPAR',
                              'ClassicalMono', 'NonClassicalMono')]))
M2 <- scale(as.matrix(final_df[, paste0('region', 1:300)])) # change to c(1:243, 246:300)
Cov1 <- scale(as.matrix(final_df[, c('age_mri', 'BMIperc', 'PubCat')]))
Cov2 <- as.matrix(final_df[, c('black', 'white')])
Cov <- cbind(Cov1, Cov2)

# X: violence, 1=Yes, 0=No - vector
# Y: depression score - vector
# M1: Immune variables - matrix where each row is a person, and each column is a region
# M2: Amygdala connectivity - matrix where each row is a person, and each column is a region

Y.1 <- Y
M1.1 <- M1
M2.1 <- M2

Y.2 <- Y
M1.2 <- M1
M2.2 <- M2

##################################

# Regress out
Y.1 <- lm(Y ~ Cov[, 'age_mri'] + Cov[, 'BMIperc'] + Cov[, 'PubCat'] + Cov[, 'black'] + Cov[, 'white'])$residuals

for (m1 in 1:ncol(M1)) {
  M1.1[, m1] <- lm(M1[, m1] ~ Cov[, 'age_mri'] + Cov[, 'BMIperc'] + Cov[, 'PubCat'] + Cov[, 'black'] + Cov[, 'white'])$residuals
}

for (m2 in 1:ncol(M2)) {
  M2.1[, m2] <- lm(M2[, m2] ~ Cov[, 'age_mri'] + Cov[, 'BMIperc'] + Cov[, 'PubCat'] + Cov[, 'black'] + Cov[, 'white'])$residuals
}



##################################

# Regress out
mod <- lm(Y ~ Cov[, 'age_mri'] + Cov[, 'BMIperc'] + Cov[, 'PubCat'] + Cov[, 'black'] + Cov[, 'white'] + X)
Y.2 <- mod$residuals + (mod$coefficients['X'])*X

for (m1 in 1:ncol(M1)) {
  mod <- lm(M1[, m1] ~ Cov[, 'age_mri'] + Cov[, 'BMIperc'] + Cov[, 'PubCat'] + Cov[, 'black'] + Cov[, 'white'] + X)
  M1.2[, m1] <- mod$residuals + (mod$coefficients['X'])*X
}

for (m2 in 1:ncol(M2)) {
  mod <- lm(M2[, m2] ~ Cov[, 'age_mri'] + Cov[, 'BMIperc'] + Cov[, 'PubCat'] + Cov[, 'black'] + Cov[, 'white'] + X)
  M2.2[, m2] <- mod$residuals + (mod$coefficients['X'])*X
}




boo <- cbind(Y.1, Y.2)
cor(boo[,'Y.1'], boo[,'Y.2'])








#
