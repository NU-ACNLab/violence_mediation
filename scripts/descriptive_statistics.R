### This script gets descriptive statistics for the sample
###
### Ellyn Butler
###


############################### Before exclusions ###############################




############################### After exclusions ###############################

basedir <- '/projects/b1108/studies/mwmh/data/processed/'

final_df <- read.csv('/projects/b1108/projects/violence_mediation/data/combined_data.csv')
demo_df <- read.csv(paste0(basedir, 'demographic/demographics_2022-11-07.csv'))
final_df <- merge(final_df, demo_df)

# Number of participants
length(final_df$subid) # 233

table(final_df$female) # 149 (84 males)

summary(final_df$age_mri) # min=11.93, median=14.03, mean=14.02, max=15.37
