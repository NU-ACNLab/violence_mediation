### This script creates a correlation plot between violence, internalizing
### symptoms, important mediators, and the potential confounders
###
### Ellyn Butler
### November 17, 2022

library(ggplot2)
library(ggcorrplot) #Not playing nice with Quest

# Load data
basedir <- '/Users/flutist4129/Documents/Northwestern/studies/mwmh/data/processed/'

final_df <- read.csv('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/data/combined_data.csv')
demo_df <- read.csv(paste0(basedir, 'demographic/demographics_2022-11-07.csv'))
final_df <- merge(final_df, demo_df)

corr_df <- final_df[, c('ever', 'RCADS_sum', 'IL10', 'region2', 'region14',
                        'region237', 'region261', 'region281', 'black', 'white',
                        'otherrace', 'PubCat', 'age_mri', 'female', 'IPR')]

names(corr_df) <- c('violence', 'internalizing', 'IL10', 'visual', 'temporal',
                    'hippocampus', 'pallidum', 'cerebellum', 'black', 'white',
                    'other race', 'puberty', 'age', 'female', 'IPR')

corr_all <- round(cor(corr_df), 3)
ggcorr_plot <- ggcorrplot(corr_all)

pdf('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/plots/allvars_corrplot.pdf', width=7, height=7)
ggcorr_plot
dev.off()
