### This script creates some basic summary statistics and plots of the violence
### data
###
### Ellyn Butler
### December 9, 2021

library('ggplot2')
library('dplyr')
library('ggcorrplot')

viol_df <- read.csv('~/Documents/Northwestern/studies/mwmh/data/violence/violence_11-22-2021.csv')

viol_df$v2.etv1 <- recode(viol_df$ETVv2_1, `1`=1, `2`=0)
viol_df$v2.etv2 <- recode(viol_df$ETVv2_2, `1`=1, `2`=0)
viol_df$v2.etv3 <- recode(viol_df$ETVv2_3, `1`=1, `2`=0)
viol_df$v2.etv4 <- recode(viol_df$ETVv2_4, `1`=1, `2`=0)
viol_df$v2.etv5 <- recode(viol_df$ETVv2_5, `1`=1, `2`=0)
viol_df$v2.etv6 <- recode(viol_df$ETVv2_6, `1`=1, `2`=0)
viol_df$v2.etv7 <- recode(viol_df$ETVv2_7, `1`=0, `2`=1)


ever_df <- data.frame(Variable=rep(paste0('ETV', 1:7), 2),
                      Violence=rep(c('Family Hurt or Killed', 'Friends Hurt or Killed',
                        'Saw Attacked Knife', 'Saw Shot', 'Shoved/Kicked/Punched',
                        'Attacked Knife', 'Shot At'), 2),
                      Visit=c(rep('First', 7), rep('Second', 7)),
                      N=c(nrow(viol_df[!is.na(viol_df$v1.etv1), ]),
                        nrow(viol_df[!is.na(viol_df$v1.etv2), ]),
                        nrow(viol_df[!is.na(viol_df$v1.etv3), ]),
                        nrow(viol_df[!is.na(viol_df$v1.etv4), ]),
                        nrow(viol_df[!is.na(viol_df$v1.etv5), ]),
                        nrow(viol_df[!is.na(viol_df$v1.etv6), ]),
                        nrow(viol_df[!is.na(viol_df$v1.etv7), ]),
                        nrow(viol_df[!is.na(viol_df$v2.etv1), ]),
                        nrow(viol_df[!is.na(viol_df$v2.etv2), ]),
                        nrow(viol_df[!is.na(viol_df$v2.etv3), ]),
                        nrow(viol_df[!is.na(viol_df$v2.etv4), ]),
                        nrow(viol_df[!is.na(viol_df$v2.etv5), ]),
                        nrow(viol_df[!is.na(viol_df$v2.etv6), ]),
                        nrow(viol_df[!is.na(viol_df$v2.etv7), ])),
                      Proportion=c(sum(viol_df$v1.etv1, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v1.etv1), ]),
                        sum(viol_df$v1.etv2, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v1.etv2), ]),
                        sum(viol_df$v1.etv3, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v1.etv3), ]),
                        sum(viol_df$v1.etv4, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v1.etv4), ]),
                        sum(viol_df$v1.etv5, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v1.etv5), ]),
                        sum(viol_df$v1.etv6, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v1.etv6), ]),
                        sum(viol_df$v1.etv7, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v1.etv7), ]),
                        sum(viol_df$v2.etv1, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v2.etv1), ]),
                        sum(viol_df$v2.etv2, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v2.etv2), ]),
                        sum(viol_df$v2.etv3, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v2.etv3), ]),
                        sum(viol_df$v2.etv4, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v2.etv4), ]),
                        sum(viol_df$v2.etv5, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v2.etv5), ]),
                        sum(viol_df$v2.etv6, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v2.etv6), ]),
                        sum(viol_df$v2.etv7, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$v2.etv7), ])
                      ))

prop_ever_plot <- ggplot(ever_df, aes(x=Violence, y=Proportion, fill=Violence)) +
  theme_linedraw() + facet_grid(. ~ Visit) +
  geom_bar(stat='identity', position='dodge') +
  theme(legend.position='none', axis.title.x=element_blank(),
		panel.spacing=unit(.1, 'lines'),
    axis.text.x = element_text(angle=45, hjust=1, size=10)) +
  scale_y_continuous(limits=c(0, 1), breaks=round(seq(0, 1, .1), digits=1)) +
  ggtitle('Ever Experienced Violent Acts?')


pdf('~/Documents/Northwestern/projects/violence_mediation/plots/prop_ever_violence.pdf')
prop_ever_plot
dev.off()

# Correlation plots - dangerously low
corr <- cor(viol_df[!is.na(viol_df$v1.etv1) & !is.na(viol_df$v2.etv2),
  c(paste0('v1.etv', 1:7), paste0('v2.etv', 1:7))])
corr_ever_plot <- ggcorrplot(corr)
pdf('~/Documents/Northwestern/projects/violence_mediation/plots/corr_ever_violence.pdf')
corr_ever_plot
dev.off()



# To Do: Clean past year data




# Filter dataframe for cleaned variables
viol_df2 <- viol_df[, c('ID', paste0('v1.etv', 1:7), paste0('v2.etv', 1:7))]
names(viol_df2)[names(viol_df2) == 'ID'] <- 'subid'

first_df <- viol_df2[, c('subid', paste0('v1.etv', 1:7))]
first_df$sesid <- 1
first_df <- rename(first_df, etv1_ever=v1.etv1, etv2_ever=v1.etv2,
  etv3_ever=v1.etv3, etv4_ever=v1.etv4, etv5_ever=v1.etv5, etv6_ever=v1.etv6,
  etv7_ever=v1.etv7)

second_df <- viol_df2[, c('subid', paste0('v2.etv', 1:7))]
second_df$sesid <- 2
second_df <- rename(second_df, etv1_ever=v2.etv1, etv2_ever=v2.etv2,
  etv3_ever=v2.etv3, etv4_ever=v2.etv4, etv5_ever=v2.etv5, etv6_ever=v2.etv6,
  etv7_ever=v2.etv7)

viol_df3 <- rbind(first_df, second_df)
viol_df3 <- viol_df3[, c('subid', 'sesid', paste0('etv', 1:7, '_ever'))]
viol_df3$subid <- paste0('MWMH', viol_df3$subid)

viol_df3$ever <- pmax(viol_df3$etv1_ever, viol_df3$etv2_ever, viol_df3$etv3_ever,
  viol_df3$etv4_ever, viol_df3$etv5_ever, viol_df3$etv6_ever, viol_df3$etv7_ever,
  na.rm=TRUE)

viol_df3$ever_wo5 <- pmax(viol_df3$etv1_ever, viol_df3$etv2_ever, viol_df3$etv3_ever,
  viol_df3$etv4_ever, viol_df3$etv6_ever, viol_df3$etv7_ever, na.rm=TRUE)

write.csv(viol_df3, '~/Documents/Northwestern/projects/violence_mediation/data/violence.csv')

################################ Sanity Checks ################################

# There shouldn't be any of these cases if they were in fact asked about "ever"
bad_etv1 <- viol_df[viol_df$v1.etv1 == 1 & viol_df$v2.etv1 == 0 & !is.na(viol_df$v1.etv1) & !is.na(viol_df$v2.etv1),
  c('v1.etv1', 'v2.etv1')]
nrow(bad_etv1)

bad_etv2 <- viol_df[viol_df$v1.etv2 == 1 & viol_df$v2.etv2 == 0 & !is.na(viol_df$v1.etv2) & !is.na(viol_df$v2.etv2),
  c('v1.etv2', 'v2.etv2')]
nrow(bad_etv2)

bad_etv3 <- viol_df[viol_df$v1.etv3 == 1 & viol_df$v2.etv3 == 0 & !is.na(viol_df$v1.etv3) & !is.na(viol_df$v2.etv3),
  c('v1.etv3', 'v2.etv3')]
new_etv3 <- viol_df[viol_df$v1.etv3 == 0 & viol_df$v2.etv3 == 1 & !is.na(viol_df$v1.etv3) & !is.na(viol_df$v2.etv3),
  c('v1.etv3', 'v2.etv3')]
nrow(bad_etv3)

bad_etv6 <- viol_df[viol_df$v1.etv6 == 1 & viol_df$v2.etv6 == 0 & !is.na(viol_df$v1.etv6) & !is.na(viol_df$v2.etv6),
  c('v1.etv6', 'v2.etv6')]

# "ever" frequencies
table(viol_df3[viol_df3$sesid == 1, 'ever'])
table(viol_df3[viol_df3$sesid == 2, 'ever'])
