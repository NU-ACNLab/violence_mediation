### This script tries out different matching procedures to deal with confounds
### prior to fitting the high-dimensional multimodal mediation model
### https://kosukeimai.github.io/MatchIt/articles/matching-methods.html
### https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf
### R Version: 4.1.0 (2021-05-18)
###
### Ellyn Butler
### October 6, 2022

################################# Load packages ################################

library('MatchIt') # v 4.4.0


################################### Load data ##################################

img_df <- read.csv('/projects/b1108/studies/mwmh/data/raw/neuroimaging/meta/bids_10-06-2022.csv')

##### Get the sessions that have at least one type of fmri scan
img_df <- img_df[which(img_df$rest == 1 | img_df$avoid == 1 | img_df$faces == 1), ]

demo_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/demographic/demographics_2022-10-04.csv')
depanx_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/clinical/depanx_2022-10-04.csv')
immune_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/immune/immune_2022-10-06.csv')
viol_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/violence/violence_2022-10-06.csv')

##### Merge
df_list <- list(img_df, demo_df, depanx_df, immune_df, viol_df)
final_df <- Reduce(function(x, y) merge(x, y), df_list)
nrow(final_df) #488

##### Filter for the first time point
final_df <- final_df[which(final_df$sesid == 1), ]
nrow(final_df) #260

##### Filter out rows with missing violence or immune data
final_df <- final_df[which(!is.na(final_df$black) & !is.na(final_df$white) &
                      !is.na(final_df$otherrace) & !is.na(final_df$black) &
                      !is.na(final_df$black) & !is.na(final_df$age_mri) &
                      !is.na(final_df$female) & !is.na(final_df$IPR) &
                      !is.na(final_df$RCADS_sum) & !is.na(final_df$IL10) &
                      !is.na(final_df$IL6) & !is.na(final_df$IL8) &
                      !is.na(final_df$TNFa) & !is.na(final_df$CRP) &
                      !is.na(final_df$uPAR) & !is.na(final_df$ClassicalMono) &
                      !is.na(final_df$NonClassicalMono) & !is.na(final_df$Neutrophils) &
                      !is.na(final_df$Lymphocytes) & !is.na(final_df$Eosinophils) &
                      !is.na(final_df$Basophils) & !is.na(final_df$ever)), ]
nrow(final_df) #256

##### Select relevant columns for matching (violence + covariates)
final_df <- final_df[, c('subid', 'ever', 'black', 'white', 'otherrace', 'age_mri',
                         'female', 'PubCat', 'IPR')]


#################################### Matching ##################################


table(final_df$ever)

##### Nearest
nearest_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='nearest')
pdf('/projects/b1108/projects/violence_mediation/plots/matching/nearest_match.pdf')
plot(nearest_match, type='jitter', interactive=FALSE)
dev.off()

##### Optimal
optimal_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='optimal')
pdf('/projects/b1108/projects/violence_mediation/plots/matching/optimal_match.pdf')
plot(optimal_match, type='jitter', interactive=FALSE)
dev.off()

##### Full
full_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='full')
pdf('/projects/b1108/projects/violence_mediation/plots/matching/full_match.pdf')
plot(full_match, type='jitter', interactive=FALSE)
dev.off()

##### Genetic
genetic_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='genetic')
pdf('/projects/b1108/projects/violence_mediation/plots/matching/genetic_match.pdf')
plot(genetic_match, type='jitter', interactive=FALSE)
dev.off()

##### Exact
#exact_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
#                         data=final_df, method='exact')

##### Coarsened
coarsened_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='cem')
pdf('/projects/b1108/projects/violence_mediation/plots/matching/coarsened_match.pdf')
plot(coarsened_match, interactive=FALSE)
dev.off()

##### Subclass
subclass_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='subclass')
pdf('/projects/b1108/projects/violence_mediation/plots/matching/subclass_match.pdf')
plot(subclass_match, type='jitter', interactive=FALSE)
dev.off()

##### Cardinality #ERROR: compilation failed for package ‘Rglpk’
#cardinal_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
#                         data=final_df, method='cardinality')
#pdf('/projects/b1108/projects/violence_mediation/plots/matching/cardinal_match.pdf')
#plot(cardinal_match, type='jitter', interactive=FALSE)
#dev.off()







#
