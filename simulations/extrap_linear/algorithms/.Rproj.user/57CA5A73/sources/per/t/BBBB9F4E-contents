library(glmnet)
library(MASS)


jointrls<-function(y,x,alp=0,lam=0,gam=0,tau=1,
                   eig=NULL,obj=TRUE,eps=1e-16,
                   cv.seed=10,scale.x=TRUE,...){
  p=dim(x)[2]
  n=dim(x)[1]
  
  U=which(is.na(y))
  L=setdiff(1:n,U)
  l=length(L)
 					   
  xL=x[L,]
  xU=x[U,]
  mls=apply(xL,2,mean)
  ses<-sqrt(apply(xL,2,var)*(l-1)/l)
  if(!scale.x)ses=rep(1,p)
  
  if(sum(ses<1e-15)>0)ses[ses<1e-15]=1		

  xL=sweep(xL,2,mls)
  xL=sweep(xL,2,1/ses,FUN="*")
  xU=sweep(xU,2,mls)
  xU=sweep(xU,2,1/ses,FUN="*")
  s.const=sqrt(n/l)
  
  mu=mean(y[L])
  sey=sqrt(var(y[L])*(l-1)/l)
  yL=(y[L]-mu)/(sey)
  
  normvec=list(sey=sey,muy=mu,mlx=mls,ses=ses,s.const=s.const)
  
  ind<-is.na(xU)
  if(any(ind))
    xU[ind]<-0
  
  if(gam<eps)gam=eps
  if(is.finite(gam)){
    if(is.null(eig)){
      eig<- eigen(xU%*%t(xU))
      eig$val[-c(1:p)]=0
      eig$val[eig$val<0]=0
      eig$vec=t(eig$vec)%*%xU
    }
    zU=sqrt(tau*gam)*diag(1/sqrt((eig$val+gam)))%*%eig$vec
  }else{
    zU=sqrt(tau)*xU
  }
  
  x1=x
  y1=rep(0,n)
  y1[L]=s.const*yL
  
  x1[c(L,U),]=s.const*rbind(xL,zU)
  
  err=NULL
  if(is.null(lam)|length(lam)>1){
    set.seed(cv.seed)
    g<-cv.glmnet(x1,y1,alpha=alp,lambda=lam,family="gaussian",standardize=FALSE,intercept=FALSE,...)
    j=which.min(g$cvm)
    lam=g$lambda[j]
    err=g$cvm[j]
  }
  
  bsemi=NA
  alpha=NA
  if(obj){
    obj<-glmnet(x1,y1,standardize=FALSE,lambda=lam,alpha=alp,intercept=FALSE,...)
    bsemi<-as.vector(coef(obj))
    bsemi[-1]=bsemi[-1]*sey/ses
    bsemi[1]=mu-sum(mls*bsemi[-1])
    alpha=as.vector(t(cbind(1,zU))%*%cbind(1,zU)%*%bsemi)/gam
  }
  
  list(obj=obj,bsemi=bsemi,alpha=alpha,zU=zU,err=err,lam=lam,alp=alp,
       eta=tau,gam=gam,L=L,U=U,normvec=normvec)
}

cv.regress.init<-function(x,L,U,fold=3,scale.x=TRUE,...){
  cv.folds<-function (n, folds = fold){
    split(sample(1:n), rep(1:folds, length = n))
  }
  all.folds <- cv.folds(length(L),fold)##-1)
  K=length(all.folds)
  p=dim(x)[2]
  zUs=list()
  
  val=rep(0,K)
  for(i in 1:K){
    omit=all.folds[[i]]
    L1=L[-omit]
    U1=c(U,L[omit])
	  
    mls<-apply(x[L1,],2,mean)
    ses<-sqrt(apply(x[L1,],2,var)*(length(L1)-1)/length(L1))
    if(!scale.x)ses=rep(1,p)
    if(sum(ses<1e-15)>0)ses[ses<1e-15]=1		
    xU=sweep(x[U1,],2,mls)
    xU=sweep(xU,2,1/ses,FUN="*")
    
    ind<-is.na(xU)
    if(any(ind))
      xU[ind]<-0
    eig<- eigen(xU%*%t(xU))
    eig$val[-c(1:p)]=0
    eig$val[eig$val<0]=0
    eig$vec=t(eig$vec)%*%xU
    zUs[[i]]<-eig
  }
  mls<-apply(x[L,],2,mean)
  ses<-sqrt(apply(x[L,],2,var)*(length(L)-1)/length(L))
  if(!scale.x)ses=rep(1,p)
  if(sum(ses<1e-15)>0)ses[ses<1e-15]=1		
	
  xU=sweep(x[U,],2,mls)
  xU=sweep(xU,2,1/ses,FUN="*")
  
  ind<-is.na(xU)
  if(any(ind))
    xU[ind]<-0
  eig<- eigen(xU%*%t(xU))
  eig$val[-c(1:p)]=0
  eig$val[eig$val<0]=0
  eig$vec=t(eig$vec)%*%xU
  zUs[[i+1]]<-eig
  
  return(list(eigs=zUs,all.folds=all.folds,K=K))
}

cv.regress.internal<-function(y,x,lam,alp,gam,tau,init=NULL,scale.x=TRUE,fold=fold,...){
  if(is.null(init)){
    init=cv.regress.init(y,x,fold=3)
  }
  
  eig=init$eigs
  all.folds=init$all.folds
  K=init$K
  val=rep(0,K)
  lvec=rep(0,K)
  
  for(i in 1:K){
    omit=all.folds[[i]]
    yval=y
    yval[L[omit]]=NA
    g1<-jointrls(yval,x,lam=lam,alp=alp,tau=tau,gam=gam,
                 eig=eig[[i]],scale.x=scale.x)
	normvec=g1$normvec
    x2=sweep(x[L[omit],],2,normvec$mlx)
    x2=sweep(x2,2,1/normvec$ses,FUN="*")
    bsemi=g1$bsemi  
    bsemi[1]=bsemi[1]-normvec$muy+sum(normvec$mlx*bsemi[-1])
    bsemi[-1]=bsemi[-1]*normvec$ses/normvec$sey
    bsemi=bsemi[-1]
    
    f=as.vector(x2%*%bsemi)
    val[i]=sum( (f-(y[L[omit]]-normvec$muy)/normvec$sey)^2)
    lvec[i]=g1$lam
  }
  cvec=sum(val)/length(L)
  olam=which.min(val)
  c(cvec,lvec[olam])
}

cv.jointrls<-function(y,x,fold=3,lambda,alpha,gam1=NULL,gam2=NULL,
                     scale.x=TRUE,init=NULL,verbose=TRUE,
					 cv.seed=10,eps=1e-9,...){
  t1<-proc.time()
  L=which(!is.na(y))
  U=which(is.na(y))
    
  if(is.null(init)){
    set.seed(cv.seed)
    if(verbose){cat("Init Stage ...\n")}
    init=cv.regress.init(x,L,U,scale.x=scale.x,fold=fold)
    if(verbose){cat("Init Stage Complete"," time= ",(proc.time()-t1)/60,"\n")}
  }
  nm=c("lam","alp","gam", "eta", "cverr")
    if(is.null(gam1))gam1=1/sort(c(Inf,1e5,1e4,1e3,1e2,1,0.5,0.1))
    if(is.null(gam2))gam2=sort(c(Inf,1e5,1e4,1e3,1e2,1,0.5,0.1))
    lgam=length(gam2)
    leta=length(gam1)
    
    if(missing(alpha))alpha=c(0,0.25,0.5,0.75,1)
    if(missing(lambda))lambda=NULL
    lalp=length(alpha)
    bsemis=matrix(0,dim(x)[2]+1,lgam*leta*lalp)
    mat=matrix(0,lgam*leta*lalp,length(nm))
    mat=as.data.frame(mat)
    names(mat)=nm
    
    r1=1
    m1=1
    for(k in 1:lalp){
      for(j in 1:lgam){
        for(i in 1:leta){
          a1=cv.regress.internal(y,x,lam=lambda,alp=alpha[k],gam=gam2[i],tau=gam1[j],
                                 init=init,fold=fold,scale.x=scale.x,...)
          cv.err=a1[1]
          lam=a1[2]
          g1<-jointrls(y,x,lam=lam,alp=alpha[k],gam=gam2[i],tau=gam1[j],
                       eig=init$eigs[[init$K+1]],scale.x=scale.x,...)
          bsemis[,r1]=g1$bsemi

          mat[r1,]=c(lam,alpha[k],gam2[i],gam1[j],cv.err)
          r1=r1+1
        }
        if(verbose){
          cat("CV Iteration: i=",m1," of ",lgam*lalp," time=",(proc.time()-t1)/60,"\n")
        }
        m1=m1+1
        
      }
    }
  list(mat=mat,bsemis=bsemis,init=init)
}
