
#' Simulate Functional White Noise Data
#' @description
#' The function fws.sim will simulate a functional time series of white noise data
#'
#' @param nb Number of basis functions to use when simulating the data. Default value is 15
#' @param gsz Number of points along the functional domain where the basis functions will be evaluated. Default value is 20
#' @param Ts Numeric/integer with total length of intended time series
#' @param seed The seed you wish to set, to ensure reproducibility
#' @return A Ts x gsz dimension matrix, containing the simulated values
#' @export
#' @importFrom fda create.bspline.basis
#' @importFrom fda eval.basis
#' @importFrom stats rnorm
#' @examples
#' fws.sim(Ts = 2000, seed = 234)
#' @details
#' If no values for nb or gsz are selected, the function will utilize the aforementioned default values of 15 and 20, respectively. \cr \cr
#' Ts and seed need to be provided every time the function is run. \cr \cr
#' Every input must be a numeric, positive number. \cr \cr
#' For more information on how this data is simulated, consult the corresponding paper at https://arxiv.org/abs/2102.01784

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

#' Simulate Functional Data with Linear Time-Varying Dynamics
#' @description
#' The function f3bL.sim will simulate a functional time series consisting of 3 bands, that follows a linear trend.
#'
#' @param nb Number of basis functions to use when simulating the data. Default value is 15
#' @param gsz Number of points along the functional domain where the basis functions will be evaluated. Default value is 20
#' @param Ts Numeric/integer with total length of intended time series
#' @param seed The seed you wish to set, to ensure reproducibility
#' @return A Ts x gsz dimension matrix, containing the simulated values
#' @export
#' @importFrom stats fft
#' @importFrom stats rnorm
#' @examples
#' f3bL.sim(Ts = 2000, seed = 234)
#' @details
#' If no values for nb or gsz are selected, the function will utilize the aforementioned default values of 15 and 20, respectively. \cr \cr
#' Ts and seed need to be provided every time the function is run. \cr \cr
#' Every input must be a numeric, positive number. \cr \cr
#' For more information on how this data is simulated, consult the corresponding paper at https://arxiv.org/abs/2102.01784

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


#' Simulate Functional Data with Sinusoidal Time-Varying Dynamics
#' @description
#' The function f3bS.sim will simulate a functional time series consisting of 3 bands, that follows a sinusoidal trend.
#'
#' @param nb Number of basis functions to use when simulating the data. Default value is 15
#' @param gsz Number of points along the functional domain where the basis functions will be evaluated. Default value is 20
#' @param Ts Numeric/integer with total length of intended time series
#' @param seed The seed you wish to set, to ensure reproducibility
#' @return A Ts x gsz dimension matrix, containing the simulated values
#' @export
#' @importFrom stats fft
#' @importFrom stats rnorm
#' @examples
#' f3bS.sim(Ts = 2000, seed = 234)
#' @details
#' If no values for nb or gsz are selected, the function will utilize the aforementioned default values of 15 and 20, respectively. \cr \cr
#' Ts and seed need to be provided every time the function is run. \cr \cr
#' Every input must be a numeric, positive number. \cr \cr
#' For more information on how this data is simulated, consult the corresponding paper at https://arxiv.org/abs/2102.01784

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

#' Implementing Iterative Empirical Band Analysis For Nonstationary Functional Time Series Data
#' @description
#' Functional Data (either simulated or uploaded) is inputted into this algorithm, along with values for different parameters, in order to
#' return the frequencies at which significant partitions occur.
#'
#' @param X A matrix with T rows and R columns, that is a realization of the Functional Time Series that will be analyzed. T is the length of the time series, and R is the number of points in the functional domain where the function is observed. The data must be entirely numeric.
#' @param Rsel The number of points within the functional domain that will be used for computing test statistics. Rsel must satisfy: 1 <= Rsel <= R
#' @param K The number of tapers to utilize in estimating the local power spectrum. K must satisfy: 1 <= K < floor(N/4 - 1)
#' @param N The number of observations that will be contained in each roughly stationary block. N must satisfy: 30 <= N <= T
#' @param ndraw The number of random draws from a Gaussian procedure that are needed to approximate the p-values.
#' @param alpha The numeric significance level for testing each frequency partition.
#' @param std A binary indicator to show whether the variance in each stationary block should be standardized.
#' @param blockdiag A binary indicator to show whether the covariance matrix for the gaussian process should be approximated with a block diagonal structure.
#' @param dcap The number of frequencies to test in a single pass
#' @return A list containing 6 different objects: \cr \cr
#' 1- A numeric vector, with the endpoints of the functional domain, along with any frequencies that were deemed to be significant partition points. \cr \cr
#' 2- A list, where the first entry is the endpoints of the functional domain, and any additional entries consist of those endpoints, and any frequencies deemed significant. \cr \cr
#' 3- A matrix that serves to summarize the results of each pass of the algorithm. \cr \cr
#' 4- An array of the multitaper estimator of the power spectrum. \cr \cr
#' 5- An array of the demeaned multitaper estimator of the power spectrum. \cr \cr
#' 6- A list, containing the test statistics, critical values, p-values, and summary tables, for each frequency that was tested in each pass, in every component of the data.
#' @export
#' @importFrom stats fft
#' @importFrom stats rnorm
#' @examples
#' nb=15; #number of basis functions used to generate white noise
#' R=5; #number of points in functional domain
#' Ts=2000; #length of time series
#' seed=234; #seed for reproducibility
#' X=fws.sim(nb=nb,gsz=R,Ts=Ts,seed=seed);
#' B=5; #number of time blocks
#' N=Ts/B; #number of observations per time block
#' bw=0.04; #bandwidth for multitaper spectral estimator
#' K=max(1,floor(bw*(N+1)-1)); #number of tapers for multitaper spectral estimator
#' std=FALSE; #standardize variance for points in functional domain (TRUE) or not (FALSE)
#' freq=seq(from=0,by=1/N,length.out=floor(N/2)+1); #Fourier frequencies
#' Rsel=4; #number of points in functional domain used for test statistics
#' pse=fhat(X,N,K,Rsel,std);

#' cmpnt="1-1"; #select component to view
#' dimnames(pse) <- list(freq,apply(expand.grid(1:Rsel,1:Rsel),1,paste,collapse = "-"),1:B);
#' image.plot(x=(1:B)*(Ts/B),y=as.numeric(rownames(pse)),z=t(Re(pse[,cmpnt,])),
#'            axes = TRUE, col = inferno(256),
#'            main = 'Multitaper Autospectrum',xlab='Time',ylab='Hz',xaxs="i");

##run fEBA algorithm

#input parameters
#' set.seed(47)
#' ndraw=100000; #number of draws from Gaussian process for approximating p-values
#' blockdiag=TRUE; #use block diagonal covariance matrix approximation
#' dcap=40; #max number of frequencies tested in a given pass
#' alpha=0.05/ceiling((1-2*bw/0.5)*(floor(N/2)+1)/dcap); #alpha with Bonferroni correction

#' run.wn <- fEBA.wrapper(X,Rsel,K,N,ndraw,alpha,std,blockdiag,dcap);
#' print(run.wn$summary);

#' @details
#' This code has only been tested for the following dimensions: T <= 50000, K <= 49, N <= 1000, Rsel <= 10 \cr \cr
#' Every input must be either a Boolean or Numeric, as mentioned above \cr \cr
#' For more information on how this algorithm is run, and how to understand its results, consult the corresponding paper at https://arxiv.org/abs/2102.01784

#function to run iterative eba algorithm
fEBA.wrapper <- function(X,Rsel,K,N,ndraw,alpha,std,blockdiag,dcap=10^10){

  freq <- seq(from=0,by=1/N,length.out=floor(N/2)+1);
  R<-min(ncol(X),Rsel);

  #multitaper estimator of power spectrum
  pse <- fhat_pmt(X,N,K,Rsel,std);
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
      tmp <- fEBA(fhat_pmt=pse[startf:endf,,,drop=FALSE],ghat=gpse[startf:endf,,,drop=FALSE],
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
                  summary=sumfile,fhat_pmt=pse,ghat=gpse,log=logfile);
  return(results)
}
