### Merge all data types for final sample
###
### Ellyn Butler
### November 3, 2022


# load data
basedir <- '/projects/b1108/studies/mwmh/data/processed/'
viol_df <- read.csv(paste0(basedir, 'violence/violence_2022-10-06.csv'))
immune_df <- read.csv(paste0(basedir, 'immune/immune_2022-10-06.csv'))
dep_df <- read.csv(paste0(basedir, 'clinical/depanx_2022-10-04.csv'))
amyg_df1 <- read.csv(paste0(basedir, 'neuroimaging/tabulated/amygconn_2022-10-31.csv'))
amyg_df2 <- read.csv(paste0(basedir, 'neuroimaging/tabulated/amygconn_2022-11-03.csv'))


########################### naning out with any nans ###########################

final_df1 <- merge(viol_df, immune_df, by=c('subid', 'sesid'))
final_df1 <- merge(final_df1, dep_df, by=c('subid', 'sesid'))
final_df1 <- merge(final_df1, amyg_df1, by=c('subid', 'sesid'))

final_df1 <- final_df1[!is.na(final_df1$ever) & !is.na(final_df1$RCADS_sum) &
  final_df1$sesid == 1 & !is.na(final_df1$IL6) & !is.na(final_df1$ClassicalMono) &
  !is.na(final_df1$NonClassicalMono) & !is.na(final_df1$Neutrophils) &
  !is.na(final_df1$Lymphocytes) & !is.na(final_df1$Eosinophils) &
  !is.na(final_df1$Basophils), ]

# Identify amygconn variables with NAs (because didn't make it into mask)
regs_df1 <- data.frame(reg=paste0('region', c(1:243, 246:300)),
                      num_nas=NA)
for (reg in regs_df1$reg) {
  regs_df1[regs_df1$reg == reg, 'num_nas'] <- sum(is.na(final_df1[, reg]))
}

# Remove amygconn variables that have more than 16 subjects with NAs
largena_vars <- regs_df1[regs_df1$num_nas > 16, 'reg']
final_df1 <- final_df1[, !(names(final_df1) %in% largena_vars1)]

# Remove subjects that still have NAs in amygconn
immune <- c('IL10', 'IL6', 'IL8', 'TNFa', 'CRP', 'uPAR', 'ClassicalMono',
            'NonClassicalMono', 'Neutrophils', 'Lymphocytes', 'Eosinophils',
            'Basophils')
remaining_regs <- names(final_df1)[names(final_df1) %in% regs_df1$reg]
final_df1 <- final_df1[, c('subid', 'sesid', 'ever', 'RCADS_sum', immune, remaining_regs)]
final_df1 <- na.omit(final_df1)
dim(final_df1)


############################# naning out only nans #############################

########### Old threshold - sanity check
final_df2 <- merge(viol_df, immune_df, by=c('subid', 'sesid'))
final_df2 <- merge(final_df2, dep_df, by=c('subid', 'sesid'))
final_df2 <- merge(final_df2, amyg_df2, by=c('subid', 'sesid'))

final_df2 <- final_df2[!is.na(final_df2$ever) & !is.na(final_df2$RCADS_sum) &
  final_df2$sesid == 1 & !is.na(final_df2$IL6) & !is.na(final_df2$ClassicalMono) &
  !is.na(final_df2$NonClassicalMono) & !is.na(final_df2$Neutrophils) &
  !is.na(final_df2$Lymphocytes) & !is.na(final_df2$Eosinophils) &
  !is.na(final_df2$Basophils), ]

# Identify amygconn variables with NAs (because didn't make it into mask)
regs_df2 <- data.frame(reg=paste0('region', c(1:243, 246:300)),
                      num_nas=NA)
for (reg in regs_df2$reg) {
  regs_df2[regs_df2$reg == reg, 'num_nas'] <- sum(is.na(final_df2[, reg]))
}

# Remove amygconn variables that you removed the first time - decide on a threshold
final_df2 <- final_df2[, !(names(final_df2) %in% largena_vars)]

# Remove subjects that still have NAs in amygconn
immune <- c('IL10', 'IL6', 'IL8', 'TNFa', 'CRP', 'uPAR', 'ClassicalMono',
            'NonClassicalMono', 'Neutrophils', 'Lymphocytes', 'Eosinophils',
            'Basophils')
remaining_regs <- names(final_df2)[names(final_df2) %in% regs_df2$reg]
final_df2 <- final_df2[, c('subid', 'sesid', 'ever', 'RCADS_sum', immune, remaining_regs)]
final_df2 <- na.omit(final_df2)
dim(final_df2)

# Compare
names(regs_df1) <- c('reg', 'num_nas1')
names(regs_df2) <- c('reg', 'num_nas2')
regs_df <- merge(regs_df1, regs_df2)


########### Final
final_df3 <- merge(viol_df, immune_df, by=c('subid', 'sesid'))
final_df3 <- merge(final_df3, dep_df, by=c('subid', 'sesid'))
final_df3 <- merge(final_df3, amyg_df2, by=c('subid', 'sesid'))

final_df3 <- final_df3[!is.na(final_df3$ever) & final_df3$sesid == 1, ]
nrow(final_df3)

final_df3 <- final_df3[!is.na(final_df3$RCADS_sum) , ]
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
largena_vars3 <- regs_df3[regs_df3$num_nas > 10, 'reg']
final_df3 <- final_df3[, !(names(final_df3) %in% largena_vars)]

# Remove subjects that still have NAs in amygconn
immune <- c('IL10', 'IL6', 'IL8', 'TNFa', 'CRP', 'uPAR', 'ClassicalMono',
            'NonClassicalMono', 'Neutrophils', 'Lymphocytes', 'Eosinophils',
            'Basophils')
remaining_regs <- names(final_df3)[names(final_df3) %in% regs_df3$reg]
final_df3 <- final_df3[, c('subid', 'sesid', 'ever', 'RCADS_sum', immune, remaining_regs)]
final_df3 <- na.omit(final_df3)
dim(final_df3)

write.csv(final_df3, '/projects/b1108/projects/violence_mediation/data/combined_data.csv', row.names=FALSE)






#
