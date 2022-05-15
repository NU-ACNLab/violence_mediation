### This script creates some basic summary statistics and plots of the violence
### data
###
### Ellyn Butler
### December 15, 2021 - May 15, 2022

library('ggplot2')
library('dplyr')

viol_df <- read.csv('/projects/b1108/projects/violence_mediation/data/violence.csv')
dep_df <- read.csv('/projects/b1108/projects/violence_mediation/data/dep_immune.csv')
mono_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/immune/monocytes.csv')
amyg_df <- read.csv('/projects/b1108/projects/violence_mediation/data/amygconn_2021-12-12.csv')

final_df <- merge(viol_df, dep_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, mono_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, amyg_df, by=c('subid', 'sesid'))

final_df <- final_df[!is.na(final_df$ever) & !is.na(final_df$RCADS_sum) &
  final_df$sesid == 1 & !is.na(final_df$IL6) & !is.na(final_df$ClassicalMono) &
  !is.na(final_df$NonClassicalMono), ]

viol_df <- final_df

ever_df <- data.frame(Variable=paste0('ETV', 1:7),
                      Violence=c('Family Hurt or Killed', 'Friends Hurt or Killed',
                        'Saw Attacked Knife', 'Saw Shot', 'Shoved Kicked Punched',
                        'Attacked Knife', 'Shot At'),
                      N=c(nrow(viol_df[!is.na(viol_df$etv1_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv2_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv3_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv4_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv5_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv6_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv7_ever), ])),
                      Proportion=c(sum(viol_df$etv1_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv1_ever), ]),
                        sum(viol_df$etv2_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv2_ever), ]),
                        sum(viol_df$etv3_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv3_ever), ]),
                        sum(viol_df$etv4_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv4_ever), ]),
                        sum(viol_df$etv5_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv5_ever), ]),
                        sum(viol_df$etv6_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv6_ever), ]),
                        sum(viol_df$etv7_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv7_ever), ]))
                      )

prop_ever_plot <- ggplot(ever_df, aes(x=Violence, y=Proportion, fill=Violence)) +
  theme_linedraw() + geom_bar(stat='identity', position='dodge') +
  theme(legend.position='none', axis.title.x=element_blank(),
		panel.spacing=unit(.1, 'lines'),
    axis.text.x = element_text(angle=45, hjust=1, size=10)) +
  scale_y_continuous(limits=c(0, .5), breaks=round(seq(0, .5, .1), digits=1))

# stats
t.test(RCADS_sum ~ ever, data=viol_df)

sum(viol_df$ever)/nrow(viol_df)

# export
jpeg('/projects/b1108/projects/violence_mediation/plots/OHBM_prop_ever_violence.jpg', res=300, units='mm', width=100, height=100)
prop_ever_plot
dev.off()
