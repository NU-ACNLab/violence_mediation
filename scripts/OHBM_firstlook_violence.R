### This script creates some basic summary statistics and plots of the violence
### data
###
### Ellyn Butler
### December 15, 2021

library('ggplot2')
library('dplyr')
library('ggcorrplot')

viol_df <- read.csv('~/Documents/Northwestern/projects/violence_mediation/data/violence.csv')
dep_df <- read.csv('~/Documents/Northwestern/projects/violence_mediation/data/dep_immune.csv')
amyg_df <- read.csv('~/Documents/Northwestern/projects/violence_mediation/data/amygconn_2021-12-12.csv')

final_df <- merge(viol_df, dep_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, amyg_df, by=c('subid', 'sesid'))

final_df <- final_df[!is.na(final_df$ever_wo5) & !is.na(final_df$RCADS_sum) &
  final_df$sesid == 1 & !is.na(final_df$IL6), ]

viol_df <- final_df

ever_df <- data.frame(Variable=paste0('ETV', c(1:4, 6, 7)),
                      Violence=c('Family Hurt or Killed', 'Friends Hurt or Killed',
                        'Saw Attacked Knife', 'Saw Shot',
                        'Attacked Knife', 'Shot At'),
                      N=c(nrow(viol_df[!is.na(viol_df$etv1_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv2_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv3_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv4_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv6_ever), ]),
                        nrow(viol_df[!is.na(viol_df$etv7_ever), ])),
                      Proportion=c(sum(viol_df$etv1_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv1_ever), ]),
                        sum(viol_df$etv2_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv2_ever), ]),
                        sum(viol_df$etv3_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv3_ever), ]),
                        sum(viol_df$etv4_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv4_ever), ]),
                        sum(viol_df$etv6_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv6_ever), ]),
                        sum(viol_df$etv7_ever, na.rm=TRUE)/nrow(viol_df[!is.na(viol_df$etv7_ever), ]))
                      )

prop_ever_plot <- ggplot(ever_df, aes(x=Violence, y=Proportion, fill=Violence)) +
  theme_linedraw() + geom_bar(stat='identity', position='dodge') +
  theme(legend.position='none', axis.title.x=element_blank(),
		panel.spacing=unit(.1, 'lines'),
    axis.text.x = element_text(angle=45, hjust=1, size=10)) +
  scale_y_continuous(limits=c(0, .4), breaks=round(seq(0, .4, .1), digits=1)) 


jpeg('~/Documents/Northwestern/projects/violence_mediation/plots/OHBM_prop_ever_violence.jpg', res=300, units='mm', width=100, height=100)
prop_ever_plot
dev.off()
