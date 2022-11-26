### This script describes co-occurrences of violence exposure types
###
### Ellyn Butler
### November 25, 2022

###### Load the data
full_df <- read.csv('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/data/combined_data.csv')
viol_df <- read.csv('/Users/flutist4129/Documents/Northwestern/studies/mwmh/data/processed/violence/violence_2022-10-06.csv')
viol_df <- merge(full_df, viol_df)

# Among those who have been shot at, what percent have been shoved/kicked/punched?
(sum(viol_df[viol_df$etv7_ever == 1, 'etv5_ever'])/nrow(viol_df[viol_df$etv7_ever == 1, ]))*100

# Among those who have been attacked with a knife, what percent have been shoved/kicked/punched?
(sum(viol_df[viol_df$etv6_ever == 1, 'etv5_ever'])/nrow(viol_df[viol_df$etv6_ever == 1, ]))*100
