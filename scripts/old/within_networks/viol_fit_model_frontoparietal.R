### This script fits the basic mediation model
###
### Ellyn Butler
### August 15, 2022


rm(list=ls())

# load functions
source("/projects/b1108/projects/multimodal_integration/PathLasso.R")

# load data
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

X <- final_df$ever
Y <- final_df$RCADS_sum
M1 <- as.matrix(final_df[, c('IL10', 'IL6', 'IL8', 'TNFa', 'CRP', 'uPAR',
                              'ClassicalMono', 'NonClassicalMono')])
M2 <- as.matrix(final_df[, paste0('region', c(175:201, 250, 251, 284:290))])

# X: violence, 1=Yes, 0=No - vector
# Y: depression score - vector
# M1: Immune variables - matrix where each row is a person, and each column is a region
# M2: Amygdala connectivity - matrix where each row is a person, and each column is a region
##################################

##################################
# method parameters

# if standardize data
standardize<-TRUE

max.itr<-10000
tol<-1e-6
trace<-FALSE

rho<-1
rho.increase<-FALSE

nu1=nu2<-2
kappa1=kappa2=kappa3=kappa4<-c(10^c(seq(-5,-3,length.out=5),seq(-3,0,length.out=15)[-1],seq(0,2,length.out=10)[-1]), c(150, 200, 250, 300, 1000))
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

saveRDS(re, '/projects/b1108/projects/violence_mediation/models/within_networks/viol_re_frontoparietal.rds')

write.csv(re[[1]][[1]]$IE.M1M2, '/projects/b1108/projects/violence_mediation/models/within_networks/IE_M1M2_frontoparietal.csv')
write.csv(re[[1]][[1]]$IE.M1, '/projects/b1108/projects/violence_mediation/models/within_networks/IE_M1_frontoparietal.csv')
write.csv(re[[1]][[1]]$IE.M2, '/projects/b1108/projects/violence_mediation/models/within_networks/IE_M2_frontoparietal.csv')
