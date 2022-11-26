### This script creates a plot of the proportions of the sample that experienced
### each type of violence exposure
###
### Ellyn Butler
### November 25, 2022

library(ggplot2)

###### Load the data
full_df <- read.csv('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/data/combined_data.csv')
viol_df <- read.csv('/Users/flutist4129/Documents/Northwestern/studies/mwmh/data/processed/violence/violence_2022-10-06.csv')
viol_df <- merge(full_df, viol_df)

##### Violence plot
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


ever_df$Violence <- ordered(ever_df$Violence, c('Family Hurt or Killed', 'Friends Hurt or Killed',
  'Saw Attacked Knife', 'Saw Shot',
  'Attacked Knife', 'Shot At', 'Shoved Kicked Punched'))

prop_ever_plot <- ggplot(ever_df, aes(x=Violence, y=Proportion, fill=Violence)) +
  theme_linedraw() + geom_bar(stat='identity', position='dodge') +
  theme(legend.position='none', axis.title.x=element_blank(), axis.title.y=element_text(size=7),
		panel.spacing=unit(.1, 'lines'), axis.text.y=element_text(size=6),
    axis.text.x = element_text(angle=45, hjust=1, size=6)) +
  scale_y_continuous(limits=c(0, .5), breaks=round(seq(0, .5, .1), digits=1)) +
  scale_fill_manual(values=c('deepskyblue3', 'steelblue1', 'springgreen3', 'palegreen1', 'pink1', 'violetred1', 'firebrick2'))

# stats
sum(viol_df$ever)/nrow(viol_df)

# export
jpeg('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/plots/ever_violence_ses-1.jpg', res=300, units='mm', width=80, height=80)
prop_ever_plot
dev.off()
