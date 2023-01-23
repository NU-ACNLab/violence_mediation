### This script creates Table 1
###
### Ellyn Butler
### January 18, 2023


library(table1)
library(dplyr)

df <- read.csv('~/Documents/Northwestern/projects/violence_mediation/data/combined_data.csv')
demo_df <- read.csv('~/Documents/Northwestern/studies/mwmh/data/processed/demographic/demographics_2022-11-07.csv')
demo_df <- demo_df[demo_df$sesid == 1, ]
df <- merge(df, demo_df)

df$Age <- df$age_mri

df$Sex <- recode(df$female, `1`='Female', `0`='Male')
df$Sex <- factor(df$Sex)

df$Black <- recode(df$black, `1`='Yes', `0`='No')
df$Black <- factor(df$Black)

df$White <- recode(df$white, `1`='Yes', `0`='No')
df$White <- factor(df$White)

df$BMI_Percentile <- df$BMIperc
df$Puberty_Category <- df$PubCat

df$Violence <- recode(df$ever, `1`='Violence Exposed', `0`='Not Violence Exposed')

table1(~ Age + Sex + Black + White + BMI_Percentile + Puberty_Category | Violence, data=df)
