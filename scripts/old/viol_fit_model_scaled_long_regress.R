### This script fits the basic mediation model
###
### Ellyn Butler
### August 22, 2022


rm(list=ls())

# load functions
source("/projects/b1108/projects/multimodal_integration/PathLasso.R")

# load data
viol_df <- read.csv('/projects/b1108/projects/violence_mediation/data/violence.csv')
dep_df <- read.csv('/projects/b1108/projects/violence_mediation/data/dep_immune.csv')
mono_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/immune/monocytes.csv')
amyg_df <- read.csv('/projects/b1108/projects/violence_mediation/data/amygconn_2021-12-12.csv')
age_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/demographic/age_visits_2022-07-26.csv')
demo_df <- read.csv('/projects/b1108/studies/mwmh/data/processed/demographic/demographics_2022-08-22.csv')
#ses_df <- read.csv()
# variables I need in here: age, sex, race, puberty, BMI, SES
# ideally also: Confusion, Hubbub, and Order Scale, the Brody Parenting Scale,
# the Harter Social Support Scale, and the Physical Activity and Exercise Scale

final_df <- merge(viol_df, dep_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, mono_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, amyg_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, age_df, by=c('subid', 'sesid'))
final_df <- merge(final_df, demo_df, by=c('subid', 'sesid'))

final_df <- final_df[!is.na(final_df$ever) & !is.na(final_df$RCADS_sum) &
  final_df$sesid == 1 & !is.na(final_df$IL6) & !is.na(final_df$ClassicalMono) &
  !is.na(final_df$NonClassicalMono), ]

X <- final_df$ever
Y <- scale(final_df$RCADS_sum)
M1 <- scale(as.matrix(final_df[, c('IL10', 'IL6', 'IL8', 'TNFa', 'CRP', 'uPAR',
                              'ClassicalMono', 'NonClassicalMono')]))
M2 <- scale(as.matrix(final_df[, paste0('region', 1:300)])) # change to c(1:243, 246:300)
Cov1 <- scale(as.matrix(final_df[, c('age_mri', 'BMIperc', 'PubCat')]))
Cov2 <- as.matrix(final_df[, c('black', 'white')])
Cov <- cbind(Cov1, Cov2)

# X: violence, 1=Yes, 0=No - vector
# Y: depression score - vector
# M1: Immune variables - matrix where each row is a person, and each column is a region
# M2: Amygdala connectivity - matrix where each row is a person, and each column is a region

##################################

# Regress out
Y <- lm(Y ~ Cov[, 'age_mri'] + Cov[, 'BMIperc'] + Cov[, 'PubCat'] + Cov[, 'black'] + Cov[, 'white'])$residuals

for (m1 in 1:ncol(M1)) {
  M1[, m1] <- lm(M1[, m1] ~ Cov[, 'age_mri'] + Cov[, 'BMIperc'] + Cov[, 'PubCat'] + Cov[, 'black'] + Cov[, 'white'])$residuals
}

for (m2 in 1:ncol(M2)) {
  M2[, m2] <- lm(M2[, m2] ~ Cov[, 'age_mri'] + Cov[, 'BMIperc'] + Cov[, 'PubCat'] + Cov[, 'black'] + Cov[, 'white'])$residuals
}

##################################
# method parameters

# if standardize data
standardize<-TRUE

max.itr<-5000
tol<-1e-6
trace<-FALSE

rho<-1
rho.increase<-FALSE

nu1=nu2<-2
kappa1=kappa2=kappa3=kappa4<-10^c(seq(-5,-3,length.out=3),seq(-3,0,length.out=11)[-1],seq(0,2,length.out=6)[-1])
mu.prod<-c(0,0.1,0.5,1,2,Inf)
##################################


##################################
# run

re<-vector("list",length=length(mu.prod))

for(ss in 1:length(mu.prod))
{
  re[[ss]]<-vector("list",length=length(kappa1))

  if(!is.infinite(mu.prod[ss]))
  {
    mu1=mu2<-mu.prod[ss]*kappa1

    for(i in length(kappa1):1)
    {
      if(i==length(kappa1))
      {
        try(re[[ss]][[i]]<-pathlasso.2b(X,M1,M2,Y,kappa1=kappa1[i],kappa2=kappa2[i],kappa3[i],kappa4[i],nu1=nu1,nu2=nu2,mu1=mu1[i],mu2=mu2[i],rho=rho,standardize=standardize,
                                        max.itr=max.itr,tol=tol,rho.increase=rho.increase,trace=FALSE,beta0=NULL,theta0=NULL,zeta0=NULL,pi0=NULL,Lambda0=NULL))
      }else
      {
        if(is.null(re[[ss]][[i+1]])==FALSE)
        {
          try(re[[ss]][[i]]<-pathlasso.2b(X,M1,M2,Y,kappa1=kappa1[i],kappa2=kappa2[i],kappa3[i],kappa4[i],nu1=nu1,nu2=nu2,mu1=mu1[i],mu2=mu2[i],rho=rho,standardize=standardize,
                                          max.itr=max.itr,tol=tol,rho.increase=rho.increase,trace=FALSE,
                                          beta0=re[[ss]][[i+1]]$out.scaled$beta,theta0=re[[ss]][[i+1]]$out.scaled$theta,zeta0=re[[ss]][[i+1]]$out.scaled$zeta,
                                          pi0=re[[ss]][[i+1]]$out.scaled$pi,Lambda0=re[[ss]][[i+1]]$out.scaled$Lambda))
        }else
        {
          try(re[[ss]][[i]]<-pathlasso.2b(X,M1,M2,Y,kappa1=kappa1[i],kappa2=kappa2[i],kappa3[i],kappa4[i],nu1=nu1,nu2=nu2,mu1=mu1[i],mu2=mu2[i],rho=rho,standardize=standardize,
                                          max.itr=max.itr,tol=tol,rho.increase=rho.increase,trace=FALSE,beta0=NULL,theta0=NULL,zeta0=NULL,pi0=NULL,Lambda0=NULL))
        }
      }

      print(paste0("mu product value ",mu.prod[ss]," (index ",ss,"): tuning parameter index ",i," (",format((length(kappa1)+1-i)/length(kappa1)*100,digits=1,nsmall=1),"% complete)"))
    }
  }else
  {
    mu1=mu2<-kappa1

    for(i in length(kappa1):1)
    {
      if(i==length(kappa1))
      {
        try(re[[ss]][[i]]<-pathlasso.2b(X,M1,M2,Y,kappa1=0,kappa2=0,kappa3[i],kappa4[i],nu1=nu1,nu2=nu2,mu1=mu1[i],mu2=mu2[i],rho=rho,standardize=standardize,
                                        max.itr=max.itr,tol=tol,rho.increase=rho.increase,trace=FALSE,beta0=NULL,theta0=NULL,zeta0=NULL,pi0=NULL,Lambda0=NULL))
      }else
      {
        if(is.null(re[[ss]][[i+1]])==FALSE)
        {
          try(re[[ss]][[i]]<-pathlasso.2b(X,M1,M2,Y,kappa1=0,kappa2=0,kappa3[i],kappa4[i],nu1=nu1,nu2=nu2,mu1=mu1[i],mu2=mu2[i],rho=rho,standardize=standardize,
                                          max.itr=max.itr,tol=tol,rho.increase=rho.increase,trace=FALSE,
                                          beta0=re[[ss]][[i+1]]$out.scaled$beta,theta0=re[[ss]][[i+1]]$out.scaled$theta,zeta0=re[[ss]][[i+1]]$out.scaled$zeta,
                                          pi0=re[[ss]][[i+1]]$out.scaled$pi,Lambda0=re[[ss]][[i+1]]$out.scaled$Lambda))
        }else
        {
          try(re[[ss]][[i]]<-pathlasso.2b(X,M1,M2,Y,kappa1=0,kappa2=0,kappa3[i],kappa4[i],nu1=nu1,nu2=nu2,mu1=mu1[i],mu2=mu2[i],rho=rho,standardize=standardize,
                                          max.itr=max.itr,tol=tol,rho.increase=rho.increase,trace=FALSE,beta0=NULL,theta0=NULL,zeta0=NULL,pi0=NULL,Lambda0=NULL))
        }
      }

      print(paste0("mu product value ",mu.prod[ss]," (index ",ss,"): tuning parameter index ",i," (",format((length(kappa1)+1-i)/length(kappa1)*100,digits=1,nsmall=1),"% complete)"))
    }
  }
}
##################################

warnings()

saveRDS(re, '/projects/b1108/projects/violence_mediation/models/viol_re_scaled_long_regress.rds')
