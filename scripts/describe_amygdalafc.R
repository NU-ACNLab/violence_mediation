### This script gets descriptive statistics on the amygdala fc variables and
### plots their distributions as boxplots partitioned by network
###
### Ellyn Butler
### July 18, 2022 - July 19, 2022

library('psych')
library('ggplot2')
library('ggpubr')

#amyg_df <- read.csv('/projects/b1108/projects/violence_mediation/data/amygconn_2021-12-12.csv')
amyg_df <- read.csv('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/data/amygconn_2021-12-12.csv')

# Remove variables that include amygdala connectivity with itself
# (there should be two)
amyg_df <- amyg_df[, !(names(amyg_df) %in% c('region244', 'region245'))]

max(amyg_df[, 3:ncol(amyg_df)])
min(amyg_df[, 3:ncol(amyg_df)])

desc_df <- describe(amyg_df[, 3:ncol(amyg_df)])
mmm_df <- data.frame(Type=c(rep('max', nrow(desc_df)), rep('med', nrow(desc_df)),
                     rep('min', nrow(desc_df))),
                     Value=c(desc_df$max, desc_df$med, desc_df$min))

mmm_dist <- ggplot(mmm_df, aes(x = Value, fill = Type)) + theme_linedraw() +
            geom_histogram()

#pdf('/projects/b1108/projects/violence_mediation/plots/amygconn_summarystats_dists.pdf', width=5, height=3)
pdf('/Users/flutist4129/Documents/Northwestern/projects/violence_mediation/plots/amygconn_summarystats_dists.pdf', width=5, height=3)
mmm_dist
dev.off()

################################ Network plots #################################
unassigned <- paste0('region', 1:12)

SomatomotorDorsal <- paste0('region', c(13:44, 252, 253, 272, 273, 295, 296, 299, 300))

SomatomotorLateral <- paste0('region', c(45:49, 254, 255, 270, 271, 297, 298))

CinguloOpercular <- paste0('region', c(50:68, 258:261, 266:269, 274, 293, 294))

Auditory <- paste0('region', 69:80)

DefaultMode <- paste0('region', c(81:135, 238, 239, 262, 263, 277:282))

ParietoMedial <- paste0('region', 136:140)

Visual <- paste0('region', c(141:174, 264, 265, 283))

FrontoParietal <- paste0('region', c(175:201, 250, 251, 284:290))

Salience <- paste0('region', c(202:210, 248, 249, 291, 292))

VentralAttention <- paste0('region', c(211:219, 256, 257))

DorsalAttention <- paste0('region', c(220:233, 275, 276))

MedialTemporalLobe <- paste0('region', 234:237)

Reward <- paste0('region', c(240:243, 246, 247)) #intentionally excluded amygdalae (244, 245)
