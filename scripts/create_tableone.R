### This script creates Table 1
###
### Ellyn Butler
### January 18, 2023 - March 20, 2023


library(table1)
library(dplyr)

df <- read.csv('~/Documents/Northwestern/projects/violence_mediation/data/combined_data.csv')
df <- df[df$sesid == 1, ]

demo_df <- read.csv('~/Documents/Northwestern/studies/mwmh/data/raw/demographic/MWMH_EB_July2022.csv')
demo_df2 <- read.csv('~/Documents/Northwestern/studies/mwmh/data/processed/demographic/demographics_2022-11-07.csv')
demo_df$subid <- paste0('MWMH', demo_df$ID)


df <- merge(df, demo_df)
df <- merge(df, demo_df2)

df$Age <- df$age_mri

df$Sex <- recode(df$female, `1`='Female', `0`='Male')
df$Sex <- factor(df$Sex)

df$Black <- recode(df$black, `1`='Yes', `0`='No')
df$Black <- factor(df$Black)

df$White <- recode(df$white, `1`='Yes', `0`='No')
df$White <- factor(df$White)

df$Hispanic <- recode(df$v1.c.ahispan, `1`='Yes', `0`='No')
df$Hispanic <- factor(df$Hispanic)

df$BMI_Percentile <- df$BMIperc
df$Puberty_Category <- df$PubCat

df$Violence <- recode(df$ever, `1`='Violence Exposed', `0`='Not Violence Exposed')

table1(~ Age + Sex + Black + White + Hispanic + BMI_Percentile + Puberty_Category | Violence, data=df)
