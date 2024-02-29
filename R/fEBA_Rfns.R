#function to simulate functional white noise data
fws.sim <- function(nb=15,gsz=20,Ts,seed){

  set.seed(seed)

  #create b-spline basis
  bspl1.1 <- create.bspline.basis(norder=nb)
  #plot(bspl1.1)
  eval.bspl1.1 <- eval.basis(seq(0, 1, length.out=gsz), bspl1.1)

  # draw coefficients for fourier basis
  covmat <- diag(exp(((1:nb)-1)/20)); # covariance matrix for coefficients
  fcf <- sqrt(covmat)%*%matrix(rnorm(nb*Ts),ncol=Ts);

  # draw one time point realization for process
  fwn <- t(fcf)%*%t(eval.bspl1.1);

  #standardize variance across components
  fwn <- apply(fwn,2,function(x) 1*(x/sd(x)))

  return(fwn);
}

#function to simulate nonstationary 3 band linear data
f3bL.sim <- function(nb,gsz,Ts,seed){

  set.seed(seed)
  seed2<-sample(1:600,3);
  
  #low frequencies
  X <- fws.sim(nb=nb,gsz=gsz,T=Ts,seed=seed2[1]);
  Ts <- nrow(X);
  Fs <- floor(Ts/2)+1;
  f <- seq(from=0,by=1/Ts,length.out=floor(Ts/2)+1);
  f <- c(f,rev(f[c(-1,-which(f==0.5))]));
  dft <- mvfft(X)/Ts;
  dft[which(f>0.15),] <- 0;
  fwn1.lf <- Re(mvfft(dft,inverse=TRUE));

  fwn1.lf <- apply(fwn1.lf,2,function(x) x/sd(x));
  # fwn1.lf <- fwn1.lf/sd(fwn1.lf);

  #middle frequencies
  X <- fws.sim(nb=nb,gsz=gsz,T=Ts,seed=seed2[2]);
  Ts <- nrow(X);
  Fs <- floor(Ts/2)+1;
  f <- seq(from=0,by=1/Ts,length.out=floor(Ts/2)+1);
  f <- c(f,rev(f[c(-1,-which(f==0.5))]));
  dft <- mvfft(X)/Ts;
  dft[which(f<=0.15 | f>0.35),] <- 0;
  fwn1.mf <- Re(mvfft(dft,inverse=TRUE));

  fwn1.mf <- apply(fwn1.mf,2,function(x) x/sd(x));
  # fwn1.mf <- fwn1.mf/sd(fwn1.mf);

  #high frequencies
  X <- fws.sim(nb=nb,gsz=gsz,T=Ts,seed=seed2[3]);
  Ts <- nrow(X);
  Fs <- floor(Ts/2)+1;
  f <- seq(from=0,by=1/Ts,length.out=floor(Ts/2)+1);
  f <- c(f,rev(f[c(-1,-which(f==0.5))]));
  dft <- mvfft(X)/Ts;
  dft[which(f<=0.35),] <- 0;
  fwn1.hf <- Re(mvfft(dft,inverse=TRUE));

  fwn1.hf <- apply(fwn1.hf,2,function(x) x/sd(x));
  # fwn1.hf <- fwn1.hf/sd(fwn1.hf);

  #nonstationary 3 segments linear
  #combine
  coef1 <- seq(from=10,to=1,length.out=Ts);
  coef2 <- seq(from=5,to=5,length.out=Ts);
  coef3 <- seq(from=1,to=10,length.out=Ts);
  X.3bL <- coef1*fwn1.lf*sqrt(.3) + coef2*fwn1.mf*sqrt(.4) + coef3*fwn1.hf*sqrt(.3);

  return(X.3bL)
}

#function to simulate nonstationary 3 band sinusoidal data
f3bS.sim <- function(nb,gsz,Ts,seed){
  
  set.seed(seed)
  seed2<-sample(1:600,4);
  
  #low frequencies
  X <- fws.sim(nb=nb,gsz=gsz,T=Ts,seed=seed2[1]);
  Ts <- nrow(X);
  Fs <- floor(Ts/2)+1;
  f <- seq(from=0,by=1/Ts,length.out=floor(Ts/2)+1);
  f <- c(f,rev(f[c(-1,-which(f==0.5))]));
  dft <- mvfft(X)/Ts;
  dft[which(f>0.15),] <- 0;
  fwn1.lf <- Re(mvfft(dft,inverse=TRUE));
  
  fwn1.lf <- apply(fwn1.lf,2,function(x) x/sd(x));
  # fwn1.lf <- fwn1.lf/sd(fwn1.lf);
  
  #middle frequencies
  X <- fws.sim(nb=nb,gsz=gsz,T=Ts,seed=seed2[2]);
  Ts <- nrow(X);
  Fs <- floor(Ts/2)+1;
  f <- seq(from=0,by=1/Ts,length.out=floor(Ts/2)+1);
  f <- c(f,rev(f[c(-1,-which(f==0.5))]));
  dft <- mvfft(X)/Ts;
  dft[which(f<=0.15 | f>0.35),] <- 0;
  fwn1.mf <- Re(mvfft(dft,inverse=TRUE));
  
  fwn1.mf <- apply(fwn1.mf,2,function(x) x/sd(x));
  # fwn1.mf <- fwn1.mf/sd(fwn1.mf);
  
  #high frequencies
  X <- fws.sim(nb=nb,gsz=gsz,T=Ts,seed=seed2[3]);
  Ts <- nrow(X);
  Fs <- floor(Ts/2)+1;
  f <- seq(from=0,by=1/Ts,length.out=floor(Ts/2)+1);
  f <- c(f,rev(f[c(-1,-which(f==0.5))]));
  dft <- mvfft(X)/Ts;
  dft[which(f<=0.35),] <- 0;
  fwn1.hf <- Re(mvfft(dft,inverse=TRUE));
  
  fwn1.hf <- apply(fwn1.hf,2,function(x) x/sd(x));
  # fwn1.hf <- fwn1.hf/sd(fwn1.hf);
  
  #nonstationary 3 segments linear
  #combine
  coef1 <- sqrt(9)*sin(2*pi*seq(0,1,length=Ts));
  coef2 <- sqrt(9)*cos(2*pi*seq(0,1,length=Ts));
  coef3 <- sqrt(9)*cos(4*pi*seq(0,1,length=Ts)); 
  X.3bS <- coef1*fwn1.lf*sqrt(.3) + coef2*fwn1.mf*sqrt(.4) + coef3*fwn1.hf*sqrt(.3)+fws.sim(nb=nb,gsz=gsz,T=Ts,seed=seed2[4]);
  
  return(X.3bS)
}

#function to run iterative eba algorithm
fEBA.wrapper <- function(X,Rsel,K,N,ndraw,alpha,std,blockdiag,dcap=10^10){

  freq <- seq(from=0,by=1/N,length.out=floor(N/2)+1);
  R<-min(ncol(X),Rsel);

  #multitaper estimator of power spectrum
  pse <- fhat(X,N,K,Rsel,std);
  B <- dim(pse)[3];
  dimnames(pse) <- list(freq,apply(expand.grid(1:R,1:R),1,paste,collapse = "-"),1:B);

  #demeaned multitaper estimator of power spectrum
  gpse <- ghat(pse);
  dimnames(pse) <- list(freq,apply(expand.grid(1:R,1:R),1,paste,collapse = "-"),1:B);

  #initialization
  f.part=c(1,floor(N/2)+1); #starting partition
  stp <- 0; #stopping condition
  idx <- 1; #partition index
  logfile <- list(); #initialize log file
  sumfile <- matrix(NA,nrow=0,ncol=4); #initalize summary file
  partlst <- list(freq[f.part]); #initialize list of partitions
  bw <- floor((K+1)*(N/(N+1)))+1; #bandwidth in terms of # of frequencies + 1
  passctr <- 0;
  dctr <- 0;

  #recursive loop to generate new partition points
  while(stp==0){
    if(min(f.part[idx+1]-bw,f.part[idx]+bw+(dctr+1)*dcap)>(f.part[idx]+bw+dctr*dcap)){ #endf>startf

      #display pass counter
      passctr <- passctr + 1;
      message(paste("Pass ",passctr,sep=""));

      #identify changepoint candidates (Cpp functions)
      startf <- f.part[idx]+bw+dctr*dcap;
      endf <- min(f.part[idx+1]-bw,startf+dcap);
      tmp <- fEBA(fhat=pse[startf:endf,,,drop=FALSE],ghat=gpse[startf:endf,,,drop=FALSE],
                  K,ndraw,alpha,blockdiag)

      #reformat and add labeling
      names(tmp) <- c('Qts','Qint','Qpv','Qhb');
      rownames(tmp$Qts) <- freq[(startf+1):endf];
      rownames(tmp$Qint) <- freq[(startf+1):endf];
      rownames(tmp$Qpv) <- freq[(startf+1):endf];
      rownames(tmp$Qhb) <- c('freq','pval','threshold','sig');
      colnames(tmp$Qts) <- apply(expand.grid(1:R,1:R),1,paste,collapse = "-");
      colnames(tmp$Qint) <- "Qint";
      colnames(tmp$Qpv) <- c(apply(expand.grid(1:R,1:R),1,paste,collapse = "-"),"Qint");
      colnames(tmp$Qhb) <- c(apply(expand.grid(1:R,1:R),1,paste,collapse = "-"),"Qint");
      tmp$Qhb[1,] <- freq[startf+1+tmp$Qhb[1,]];

      #append run to log file
      logfile <- c(logfile,list(tmp));

      #append to summary
      sumfile <- rbind(sumfile,tmp$Qhb[,'Qint']);

      #print results from pass
      print(tmp$Qhb[,'Qint']);

      #update partition, list, and starting points
      f.part.curr <- f.part;
      len.curr <- length(f.part.curr);
      if(as.logical(tmp$Qhb[4,"Qint"]) && tmp$Qhb['freq','Qint']!=freq[f.part.curr[idx+1]]){
        f.part <- as.vector(sort(c(f.part,which(freq==tmp$Qhb['freq','Qint']))));
      }

      if(length(f.part)>len.curr){
        #if updates
        #append new partition to partition list
        partlst <- c(partlst,list(freq[f.part]));
        #increment idx
        idx <- idx+1;
        #reset dctr
        dctr <- 0;
      } else {
        #if no updates
        #increment dctr
        dctr <- dctr + 1;
      }
    }else{
      idx <- idx+1; #move to next segment if too small
    }

    #stop once reached last partition point
    stp <- as.numeric(length(f.part)==idx);
  }

  #compile results
  rownames(sumfile) <- 1:nrow(sumfile);
  results <- list(part.final=freq[f.part],part.list=partlst,
                  summary=sumfile,fhat=pse,ghat=gpse,log=logfile);
  return(results)
}