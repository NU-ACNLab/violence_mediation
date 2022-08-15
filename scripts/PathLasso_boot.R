### This script bootstraps the pathway coefficients
###
### Ellyn Butler
### July 19, 2022

# load functions
source("/projects/b1108/projects/multimodal_integration/PathLasso.R")
library('parallel')

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
Y <- scale(final_df$RCADS_sum)
M1 <- scale(as.matrix(final_df[, c('IL10', 'IL6', 'IL8', 'TNFa', 'CRP', 'uPAR',
                              'ClassicalMono', 'NonClassicalMono')]))
M2 <- scale(as.matrix(final_df[, paste0('region', c(1:243, 246:300))]))

dir.out3=dir.out2 <- '/projects/b1108/projects/violence_mediation/models'

# method parameters
standardize<-TRUE

max.itr<-5000
tol<-1e-6
trace<-FALSE

rho<-1
rho.increase<-FALSE

kappa1=kappa2=kappa3=kappa4<-10^c(seq(-2,1,length.out=11),seq(1,2,length.out=6)[-1])

mu.prod<-0.1
mu1<-mu.prod*kappa1
mu2<-mu.prod*kappa1

n.tune<-length(kappa1)

# bootstrap parameters
sims<-500
conf.level<-0.95
##################################

##################################
method0<-c("P2P3","P1P3","P1P2P3-1","P1P2P3-2")

runonce<-function(b)
{
  if(file.exists(paste0(dir.out,"/boot_run",b,".RData"))==FALSE)
  {
    set.seed(as.numeric(paste0("20",run.date))+b*100)
    itmp<-sample(1:n,size=n,replace=TRUE)

    new.M1<-X%*%re[[kappa.idx[1]]]$beta+res.M1[itmp,]
    new.M2<-X%*%re[[kappa.idx[1]]]$zeta+new.M1%*%re[[kappa.idx[1]]]$Lambda+res.M2[itmp,]
    new.Y<-X*re[[kappa.idx[1]]]$delta+new.M1%*%re[[kappa.idx[1]]]$theta+new.M2%*%re[[kappa.idx[1]]]$pi+res.Y[itmp]

    out.tmp<-NULL
    if(method=="P2P3")
    {
      # kappa1 = kappa2 = 0
      try(out.tmp<-pathlasso.2b(X,new.M1,new.M2,new.Y,kappa1=0,kappa2=0,kappa3[kappa.idx[1]],kappa4[kappa.idx[1]],nu1=nu1,nu2=nu2,mu1=kappa1[kappa.idx[1]],mu2=kappa2[kappa.idx[1]],
                                rho=rho,standardize=standardize,max.itr=max.itr,tol=tol,rho.increase=rho.increase,trace=FALSE,beta0=NULL,theta0=NULL,zeta0=NULL,pi0=NULL,Lambda0=NULL))
    }
    if(method=="P1P3")
    {
      # mu1 = mu2 = 0
      try(out.tmp<-pathlasso.2b(X,new.M1,new.M2,new.Y,kappa1=kappa1[kappa.idx[1]],kappa2=kappa2[kappa.idx[1]],kappa3[kappa.idx[1]],kappa4[kappa.idx[1]],nu1=nu1,nu2=nu2,mu1=0,mu2=0,
                                rho=rho,standardize=standardize,max.itr=max.itr,tol=tol,rho.increase=rho.increase,trace=FALSE,beta0=NULL,theta0=NULL,zeta0=NULL,pi0=NULL,Lambda0=NULL))
    }
    if(method=="P1P2P3-1")
    {
      # kappa1 = kappa2 = kappa3 = kappa4 = mu1 = mu2
      try(out.tmp<-pathlasso.2b(X,new.M1,new.M2,new.Y,kappa1=kappa1[kappa.idx[1]],kappa2=kappa2[kappa.idx[1]],kappa3[kappa.idx[1]],kappa4[kappa.idx[1]],
                                nu1=nu1,nu2=nu2,mu1=kappa1[kappa.idx[1]],mu2=kappa2[kappa.idx[1]],
                                rho=rho,standardize=standardize,max.itr=max.itr,tol=tol,rho.increase=rho.increase,trace=FALSE,beta0=NULL,theta0=NULL,zeta0=NULL,pi0=NULL,Lambda0=NULL))
    }
    if(method=="P1P2P3-2")
    {
      # kappa1 = kappa2 = kappa3 = kappa4
      # mu1 = mu2 = mu.prod * kappa1
      try(out.tmp<-pathlasso.2b(X,new.M1,new.M2,new.Y,kappa1=kappa1[kappa.idx[1]],kappa2=kappa2[kappa.idx[1]],kappa3[kappa.idx[1]],kappa4[kappa.idx[1]],
                                nu1=nu1,nu2=nu2,mu1=mu1[kappa.idx[1]],mu2=mu2[kappa.idx[1]],
                                rho=rho,standardize=standardize,max.itr=max.itr,tol=tol,rho.increase=rho.increase,trace=FALSE,beta0=NULL,theta0=NULL,zeta0=NULL,pi0=NULL,Lambda0=NULL))
    }

    save(list=c("itmp","out.tmp"),file=paste0(dir.out,"/boot_run",b,".RData"))
  }
}

ii<-length(method0)
for(ii in length(method0):1)
{
  method<-method0[ii]

  dir.out4<-paste0(dir.out3,"/",method)
  if(file.exists(dir.out4)==FALSE)
  {
    dir.create(dir.out4)
  }

  re.file<-paste0(dir.out2,"/PathLasso_",method,".RData") #Needs to already exist
  run.file<-paste0(dir.out2,"/",method,"/summary.RData") #Needs to already exist
  out.file<-paste0(dir.out2,"/",method,"/bootRes.RData")

  if(file.exists(out.file)==FALSE)
  {
    if(file.exists(run.file)==TRUE&file.exists(re.file)==TRUE) #### Q: Need these from previous runs?
    {
      load(re.file)

      env.tmp<-new.env()
      load(run.file,env.tmp)

      # Tuning parameter selection using all sample: based on BIC
      # tuning parameter estiamte
      kappa.idx<-which.min(otmp$BIC[5,])
      print(paste0("kappa estimate: ",round(kappa1[kappa.idx],digit=4)," (index: ",kappa.idx,")"))

      dir.out<-paste0(dir.out4,"/kappa_idx",kappa.idx)
      if(file.exists(dir.out)==FALSE)
      {
        dir.create(dir.out)
      }

      #==============================================================
      # calculate residuals
      res.M1<-scale(M1-X%*%re[[kappa.idx[1]]]$beta,center=TRUE,scale=FALSE)
      res.M2<-scale(M2-X%*%re[[kappa.idx[1]]]$zeta-M1%*%re[[kappa.idx[1]]]$Lambda,center=TRUE,scale=FALSE)
      res.Y<-scale(Y-X*re[[kappa.idx[1]]]$delta-M1%*%re[[kappa.idx[1]]]$theta-M2%*%re[[kappa.idx[1]]]$pi,center=TRUE,scale=FALSE)

      require(parallel)
      mclapply(1:sims,runonce,mc.cores=25)
      #==============================================================
    }
  }
}
##################################
