### This script fits the matched mediation model
###
### Ellyn Butler
### October 31, 2022 - November 6, 2022


rm(list=ls())

# load functions
source("/projects/b1108/projects/multimodal_integration/PathLasso.R")
library('MatchIt')

# Load data
basedir <- '/projects/b1108/studies/mwmh/data/processed/'

final_df <- read.csv('/projects/b1108/projects/violence_mediation/data/combined_data.csv')
demo_df <- read.csv(paste0(basedir, 'demographic/demographics_2022-11-07.csv'))
final_df <- merge(final_df, demo_df)

full_match <- matchit(ever ~ black + white + otherrace + age_mri + female + PubCat + IPR,
                         data=final_df, method='full', interactive=FALSE)

# write out weights
saveRDS(full_match, file='/projects/b1108/projects/violence_mediation/models/matching/fullmatch.rds')

# get data types
immune <- c('IL10', 'IL6', 'IL8', 'TNFa', 'CRP', 'uPAR', 'ClassicalMono',
            'NonClassicalMono', 'Neutrophils', 'Lymphocytes', 'Eosinophils',
            'Basophils')
regs <- grep('region', names(final_df), value=TRUE)

# Get the final matrices
X <- final_df$ever*sqrt(full_match$weights)
Y <- scale(final_df$RCADS_sum)*sqrt(full_match$weights)
M1 <- scale(as.matrix(final_df[, immune]))*sqrt(full_match$weights)
M2 <- scale(as.matrix(final_df[, regs]))*sqrt(full_match$weights)

# X: violence, 1=Yes, 0=No - vector
# Y: depression score - vector
# M1: Immune variables - matrix where each row is a person, and each column is a region
# M2: Amygdala connectivity - matrix where each row is a person, and each column is a region
##################################

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

saveRDS(re, '/projects/b1108/projects/violence_mediation/models/viol_re_scaled_long_fulldata_fullmatch.rds')

write.csv(re[[1]][[1]]$IE.M1M2, '/projects/b1108/projects/violence_mediation/models/IE_M1M2_scaled_long_fulldata_fullmatch.csv')
write.csv(re[[1]][[1]]$IE.M1, '/projects/b1108/projects/violence_mediation/models/IE_M1_scaled_long_fulldata_fullmatch.csv')
write.csv(re[[1]][[1]]$IE.M2, '/projects/b1108/projects/violence_mediation/models/IE_M2_scaled_long_fulldata_fullmatch.csv')
