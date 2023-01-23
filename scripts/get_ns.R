### This script gets the Ns and sample characteristics before and after exclusions
###
### Ellyn Butler
### January 18, 2022

#library(tableone)
#library(flextable)
library(table1) #https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

# load data
basedir <- '/projects/b1108/studies/mwmh/data/processed/'
viol_df <- read.csv(paste0(basedir, 'violence/violence_2022-10-06.csv'))
viol_df <- viol_df[viol_df$sesid == 1, ]
immune_df <- read.csv(paste0(basedir, 'immune/immune_2022-10-06.csv'))
immune_df <- immune_df[immune_df$sesid == 1, ]
dep_df <- read.csv(paste0(basedir, 'clinical/depanx_2022-10-04.csv'))
dep_df <- dep_df[dep_df$sesid == 1, ]
amyg_df2 <- read.csv(paste0(basedir, 'neuroimaging/tabulated/amygconn_2022-11-03.csv'))
amyg_df2 <- amyg_df2[amyg_df2$sesid == 1, ]

demo_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/demographic/demographics_2022-10-04.csv')


# Filter for first time point
first_df <- demo_df[which(demo_df$sesid == 1), ]

# Remove if they don't have an age for lab or MRI (like they were never in the study)
first_df <- first_df[!is.na(first_df$age_lab) & !is.na(first_df$age_mri),] #277!

################################ Pre-exclusions ################################

sum(first_df$female)/nrow(first_df)

mean(first_df$age_mri)
sd(first_df$age_mri)
#mean(first_df$age_lab)
#sd(first_df$age_lab)


################################ Post-exclusions ###############################

########### Old threshold - sanity check
final_df1 <- merge(first_df, viol_df)
final_df1 <- final_df1[!is.na(final_df1$ever), ] # one missing violence data (276)
final_df2 <- merge(first_df, immune_df)
final_df2 <- final_df2[!is.na(final_df2$IL6) & !is.na(final_df2$ClassicalMono) &
  !is.na(final_df2$NonClassicalMono) & !is.na(final_df2$Neutrophils) &
  !is.na(final_df2$Lymphocytes) & !is.na(final_df2$Eosinophils) &
  !is.na(final_df2$Basophils), ] # four missing immune data (273)
final_df3 <- merge(first_df, dep_df)
final_df3 <- final_df3[!is.na(final_df3$RCADS_sum), ] # none missing internalizing data (277)
final_df4 <- merge(first_df, amyg_df2) # 22 missing imaging data (255)

final_dfs <- list(first_df, viol_df, immune_df, amyg_df2, dep_df)
final_df <- Reduce(function(...) merge(...), final_dfs)

########### Final
final_df3 <- merge(viol_df, immune_df, by=c('subid', 'sesid'))
final_df3 <- merge(final_df3, dep_df, by=c('subid', 'sesid'))
final_df3 <- merge(final_df3, amyg_df2, by=c('subid', 'sesid'))

final_df3 <- final_df3[!is.na(final_df3$ever) & final_df3$sesid == 1, ]
nrow(final_df3)

final_df3 <- final_df3[!is.na(final_df3$RCADS_sum), ]
nrow(final_df3)

final_df3 <- final_df3[!is.na(final_df3$IL6) & !is.na(final_df3$ClassicalMono) &
  !is.na(final_df3$NonClassicalMono) & !is.na(final_df3$Neutrophils) &
  !is.na(final_df3$Lymphocytes) & !is.na(final_df3$Eosinophils) &
  !is.na(final_df3$Basophils), ]
nrow(final_df3)

# Identify amygconn variables with NAs (because didn't make it into mask)
regs_df3 <- data.frame(reg=paste0('region', c(1:243, 246:300)),
                      num_nas=NA)
for (reg in regs_df3$reg) {
  regs_df3[regs_df3$reg == reg, 'num_nas'] <- sum(is.na(final_df3[, reg]))
}

# Remove amygconn variables that you removed the first time - decide on a threshold
largena_vars3 <- regs_df3[regs_df3$num_nas > 9, 'reg']
final_df3 <- final_df3[, !(names(final_df3) %in% largena_vars3)]

# Remove subjects that still have NAs in amygconn
immune <- c('IL10', 'IL6', 'IL8', 'TNFa', 'CRP', 'uPAR', 'ClassicalMono',
            'NonClassicalMono', 'Neutrophils', 'Lymphocytes', 'Eosinophils',
            'Basophils')
remaining_regs <- names(final_df3)[names(final_df3) %in% regs_df3$reg]
final_df3 <- final_df3[, c('subid', 'sesid', 'ever', 'RCADS_sum', immune, remaining_regs)]
final_df3 <- na.omit(final_df3)
dim(final_df3)

final_df3 <- merge(final_df3, first_df)

sum(final_df3$female)/nrow(final_df3)

mean(final_df3$age_mri)
sd(final_df3$age_mri)

mean(final_df3$days_mri_minus_lab)
sd(final_df3$days_mri_minus_lab)

final_df3$female <- as.factor(final_df3$female)
final_df3$black <- as.factor(final_df3$black)
final_df3$white <- as.factor(final_df3$white)

table1(~ age_mri + female + black + white + BMIperc + PubCat | ever, data=final_df3)
