### This script tries out different matching procedures to deal with confounds
### prior to fitting the high-dimensional multimodal mediation model
### https://kosukeimai.github.io/MatchIt/articles/matching-methods.html
### https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf
### https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html (follow this)
###
### R Version: 4.1.0 (2021-05-18)
###
### Ellyn Butler
### October 6, 2022 - October 20, 2022


# "Covariates should be those that cause variation in the outcome and selection
# into treatment group; these are known as confounding variables."

# "Values of standardized mean differences and eCDF statistics close to zero and
# values of variance ratios close to one indicate good balance, and here many of
# them are far from their ideal values."

# "some matching options, such as setting restrictions for common support or
# calipers, can further decrease the number of remaining units"

################################# Load packages ################################

library('MatchIt') # v 4.4.0
library('cobalt')


################################### Load data ##################################

basedir <- '/projects/b1108/studies/mwmh/data/processed/'

final_df <- read.csv('/projects/b1108/projects/violence_mediation/data/combined_data.csv')
demo_df <- read.csv(paste0(basedir, 'demographic/demographics_2022-10-04.csv'))
final_df <- merge(final_df, demo_df)
nrow(final_df) #248


##### Filter out rows with missing violence or immune data... all present
final_df <- final_df[which(!is.na(final_df$black) & !is.na(final_df$white) &
                      !is.na(final_df$otherrace) & !is.na(final_df$black) &
                      !is.na(final_df$black) & !is.na(final_df$age_mri) &
                      !is.na(final_df$female) & !is.na(final_df$IPR)), ]
nrow(final_df) #248

##### Select relevant columns for matching (violence + covariates)
final_df <- final_df[, c('subid', 'ever', 'black', 'white', 'otherrace', 'age_mri',
                         'female', 'PubCat', 'IPR')]


#################################### Matching ##################################


table(final_df$ever)

##### Nearest
nearest_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='nearest', interactive=FALSE, link = "probit")
pdf('/projects/b1108/projects/violence_mediation/plots/matching/nearest_match.pdf')
plot(summary(nearest_match))
plot(nearest_match, type='qq', interactive=FALSE)
plot(nearest_match, type='qq', interactive=FALSE, which.xs=c('black', 'white', 'otherrace'))
plot(nearest_match, type='qq', interactive=FALSE, which.xs=c('age_mri', 'female'))
plot(nearest_match, type='qq', interactive=FALSE, which.xs=c('PubCat', 'IPR'))
dev.off()

nearest_match_caliper <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='nearest', interactive=FALSE, caliper=0.1,
                         link = "probit")
pdf('/projects/b1108/projects/violence_mediation/plots/matching/nearest_match_caliper.pdf')
plot(summary(nearest_match_caliper))
plot(nearest_match_caliper, type='qq', interactive=FALSE)
plot(nearest_match_caliper, type='qq', interactive=FALSE, which.xs=c('black', 'white', 'otherrace'))
plot(nearest_match_caliper, type='qq', interactive=FALSE, which.xs=c('age_mri', 'female'))
plot(nearest_match_caliper, type='qq', interactive=FALSE, which.xs=c('PubCat', 'IPR'))
dev.off()

##### Optimal
optimal_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='optimal', interactive=FALSE)
pdf('/projects/b1108/projects/violence_mediation/plots/matching/optimal_match.pdf')
plot(summary(optimal_match))
plot(optimal_match, type='jitter', interactive=FALSE)
plot(optimal_match, type='qq', interactive=FALSE, which.xs=c('black', 'white', 'otherrace'))
plot(optimal_match, type='qq', interactive=FALSE, which.xs=c('age_mri', 'female'))
plot(optimal_match, type='qq', interactive=FALSE, which.xs=c('PubCat', 'IPR'))
dev.off()

##### Full
full_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='full', interactive=FALSE)
pdf('/projects/b1108/projects/violence_mediation/plots/matching/full_match.pdf')
plot(summary(full_match))
plot(full_match, type='jitter', interactive=FALSE)
plot(full_match, type='qq', interactive=FALSE, which.xs=c('black', 'white', 'otherrace'))
plot(full_match, type='qq', interactive=FALSE, which.xs=c('age_mri', 'female'))
plot(full_match, type='qq', interactive=FALSE, which.xs=c('PubCat', 'IPR'))
dev.off()

# love plot

v <- data.frame(old = c('IPR', 'female', 'age_mri', 'PubCat', 'otherrace',
                        'white', 'black'),
                new = c('IPR', 'female', 'age', 'puberty', 'other race',
                        'white', 'black'))

full_love <- love.plot(bal.tab(full_match), stat = 'mean.diffs', threshold = .1,
                    var.order = 'unadjusted', var.names = v) +
                    theme(legend.position = 'bottom') +
                    scale_color_manual(values=c('cadetblue2', 'darkslateblue'))

pdf('/projects/b1108/projects/violence_mediation/plots/matching/full_love.pdf', width=4, height=4)
full_love
dev.off()

full_match_caliper <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='full', interactive=FALSE, caliper=0.1)
pdf('/projects/b1108/projects/violence_mediation/plots/matching/full_match_caliper.pdf')
plot(summary(full_match_caliper))
plot(full_match_caliper, type='jitter', interactive=FALSE)
plot(full_match_caliper, type='qq', interactive=FALSE, which.xs=c('black', 'white', 'otherrace'))
plot(full_match_caliper, type='qq', interactive=FALSE, which.xs=c('age_mri', 'female'))
plot(full_match_caliper, type='qq', interactive=FALSE, which.xs=c('PubCat', 'IPR'))
dev.off()

##### Genetic
genetic_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='genetic', interactive=FALSE)
pdf('/projects/b1108/projects/violence_mediation/plots/matching/genetic_match.pdf')
plot(summary(genetic_match))
plot(genetic_match, type='jitter', interactive=FALSE)
plot(genetic_match, type='qq', interactive=FALSE, which.xs=c('black', 'white', 'otherrace'))
plot(genetic_match, type='qq', interactive=FALSE, which.xs=c('age_mri', 'female'))
plot(genetic_match, type='qq', interactive=FALSE, which.xs=c('PubCat', 'IPR'))
dev.off()

##### Exact
#exact_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
#                         data=final_df, method='exact', interactive=FALSE)

##### Coarsened
coarsened_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='cem', interactive=FALSE)
pdf('/projects/b1108/projects/violence_mediation/plots/matching/coarsened_match.pdf')
plot(summary(coarsened_match))
plot(coarsened_match, interactive=FALSE)
plot(coarsened_match, type='qq', interactive=FALSE, which.xs=c('black', 'white', 'otherrace'))
plot(coarsened_match, type='qq', interactive=FALSE, which.xs=c('age_mri', 'female'))
plot(coarsened_match, type='qq', interactive=FALSE, which.xs=c('PubCat', 'IPR'))
dev.off()

##### Subclass
subclass_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='subclass', interactive=FALSE)
pdf('/projects/b1108/projects/violence_mediation/plots/matching/subclass_match.pdf')
plot(summary(subclass_match))
plot(subclass_match, type='jitter', interactive=FALSE)
plot(subclass_match, type='qq', interactive=FALSE, which.xs=c('black', 'white', 'otherrace'))
plot(subclass_match, type='qq', interactive=FALSE, which.xs=c('age_mri', 'female'))
plot(subclass_match, type='qq', interactive=FALSE, which.xs=c('PubCat', 'IPR'))
dev.off()

##### Cardinality #ERROR: compilation failed for package ‘Rglpk’
#cardinal_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
#                         data=final_df, method='cardinality', interactive=FALSE)
#pdf('/projects/b1108/projects/violence_mediation/plots/matching/cardinal_match.pdf')
#plot(cardinal_match, type='jitter', interactive=FALSE)
#dev.off()







#
