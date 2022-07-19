### This script gets descriptive statistics on the amygdala fc variables and
### plots their distributions as boxplots partitioned by network
###
### Ellyn Butler
### July 18, 2022

library('psych')

amyg_df <- read.csv('/projects/b1108/projects/violence_mediation/data/amygconn_2021-12-12.csv')

# Remove variables that include amygdala connectivity with itself
# (there should be two)
amyg_df <- amyg_df[, !(names(amyg_df) %in% c('region244', 'region245'))]

max(amyg_df[, 3:ncol(amyg_df)])
min(amyg_df[, 3:ncol(amyg_df)])

# Network plots
unassigned <-

SomatomotorDorsal <-

SomatomotorLateral <-

CinguloOpercular <-

Auditory <-

DefaultMode <-

ParietoMedial <-

Visual <-

FrontoParietal <-

Salience <-

VentralAttention <-

DorsalAttention <-

MedialTemporalLobe <-

Reward <-

Cerebellum <-
