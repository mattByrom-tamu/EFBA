#############################################################################
#############################################################################
#  R functions for "Empirical Frequency Band Analysis of Nonstationary Time Series"
#  by Bruce, Tang, Hall, and Krafty (2019)
#
#  Author: Scott A. Bruce
#
#  Date: June 16, 2019
#
#  Description:
#  Contains all functions needed to implement the search algorithm with
#  the FRESH statistic.  This is the file that should be chosen in the demo
#  file in order to load all functions needed simulate the data and run the
#  search algorithm.
#############################################################################
#############################################################################

###function to linearly detrend a data series
#' Linearly Detrend a Data Series
#'
#' @param vec univariate time series
#' @param std TRUE/FALSE Boolean: Do you want to standardize the total variance of the time series to 1?
#'
#' @return detrended and standardized time series
#' @noRd
#' @importFrom stats sd
#'
#' @examples
#' detrend(vec = eba.simdata(T= 50000)$wn,std = FALSE)
detrend <- function(vec,std){
  #remove linear trend for each interval
  xmat <- cbind(matrix(1,length(vec),1),seq(1,length(vec),length=length(vec)));
  linfit <- solve(t(xmat)%*%xmat)%*%t(xmat)%*%vec;
  vec <- vec-xmat%*%linfit;

  #standardized variance
  if (std){
    vec <- vec/sd(vec);
  }

  #return
  return(vec);
}

###function to create symmetric matrix from vector
#' creates symmetric matrix from vector
#'
#' @param x vector of
#' @param n
#'
#' @return symmetric matrix from x and n
#' @noRd
#'
#' @examples
#' Used to calculate covf in gcov function
symmat <- function(x,n){
  tmp <- matrix(0,n,n);
  tmp[lower.tri(tmp)] <- rep(x,n:1-1);
  return(tmp+t(tmp)+diag(x));
}

###function to compute multitaper spectogram using sine tapers
#' Title
#' @description
#' A short description...
#'
#' @param X univariate time series with a length > 0 and not missing or non-finite values
#' @param T sqrt(N)
#' @param N number of of observations per approximately stationary block
#' @param B calculated from X and N
#' @param K number of tapers to use in multitaper spectral estimator
#'
#' @return multitaper spectrogram
#' @noRd
#' @importFrom stats mvfft
#'
#' @examples
#' used in eba.search to compute multitaper spectrogram using sine tapers
mtspc <- function(X,T,N,B,K){
  tapers <- outer(1:N,1:K,FUN=function(n,k) sqrt(2/(N+1))*sin(pi*k*n/(N+1))); #sine tapers
  mtspec <- apply(X=X,MARGIN=2,FUN=function(x) rowMeans(Mod(mvfft(tapers*x))^2)); #multitaper estimates
  f <- seq(from=0,by=1/N,length.out=floor(N/2)+1);
  t <- seq(0,1-1/B,by=1/B)+1/(2*B); #time points
  return(list(mtspec=mtspec[1:length(f),],tapers=tapers,f=f,t=t));
}

###function to find covariance matrix of demeaned spectral estimates
#' Title
#'
#' @param X
#' @param B
#' @param N
#' @param K
#' @param tapers
#'
#' @return
#' @noRd
#' @importFrom stats mvfft
#' @importFrom stats toeplitz
#' @examples compute covariance of de-meaned spectral estimates
gcov <- function(X,B,N,K,tapers){
  #covariance component depending on smaller frequency (omega)
  covf.omega <- (X/K)^2;

  #covariance component depending on frequency lag (nu)
  N.nu <- floor(min((K+1)/(N+1),0.5)*N)+1; #number of positive frequency lags with non-zero correlation
  N.omega <- dim(X)[1];
  ift <- matrix(apply(X=tapers,MARGIN=2,FUN=function(x) {(abs(mvfft(x*tapers,inverse=TRUE))^2)[1:N.nu,]}),N.nu,K^2);
  covf.nu <- c(apply(X=ift,MARGIN=1,FUN=sum),rep(0,max(0,N.omega-N.nu)));

  #elementwise product of two components to get covf
  covf <- array(apply(X=covf.omega,MARGIN=2,FUN=function(x) symmat(x,N.omega)*toeplitz(covf.nu)),c(N.omega,N.omega,B));

  #estimate covariance matrix for de-meaned multitaper estimates
  covg <- aperm(apply(X=covf,MARGIN=c(1,2),FUN=function(x) (1-(2/B))*x + (1/B^2)*sum(x)),c(2,3,1));

  return(covg);
}

###function to partition series into segments of size N
#' Title
#'
#' @param X
#' @param N
#'
#' @return
#' @noRd
#'
#' @examples used to partition series into segments of size N in eba.search
partN <- function(X,N){
  T <- length(X); #length of series
  B <- floor(T/N); #number of partition segments
  if(T%%N!=0){
    tmp <- floor(T%%N/2+0.5);
    if(T%%N%%2!=0){
      Xmat <- matrix(X[tmp:(T-tmp)],N,B);
      warning(paste("Length of X is not a multiple of N.  First ",tmp-1," and last ",tmp," observations have been discarded.",sep=""));
    }else if(T%%N%%2==0){
      Xmat <- matrix(X[(tmp+1):(T-tmp)],N,B);
      warning(paste("Length of X is not a multiple of N.  First ",tmp," and last ",tmp," observations have been discarded.",sep=""));
    }
    T <- N*B; #update T
  } else{
    Xmat <- matrix(X,N,B);
  }
  return(Xmat);
}

###function to find frequency partition point using Hochberg step up rule
#' Title
#'
#' @param X.dm
#' @param f
#' @param startf
#' @param endf
#' @param covg
#' @param alpha
#'
#' @return
#' @noRd
#'
#' @examples used to identify changepoint candidates in eba.search
eba.b <- function(X.dm,f,startf,endf,covg,alpha) {

  #initialize data container for results
  out <- vector(mode='numeric',length=6);
  names(out) <- c('bhat.idx','bhat','bhat.Q','bhat.pval','bhat.pval.th','bhat.sig');

  #Obtain vector of Q values for different endpoints
  N.srch.b <- endf-startf; #number of frequencies searching over to find b
  X.dm.cumrowsum <- apply(X=X.dm[startf:(endf-1),,drop=FALSE],MARGIN=2,FUN=cumsum);
  Qb <- apply(X=(X.dm[(startf+1):endf,,drop=FALSE]-(1:N.srch.b)^-1*X.dm.cumrowsum)^2,MARGIN=1,FUN=sum);
  #x11();plot(f[(startf+1):endf],Qb,type="l",xlab="Hz",ylab="Q",mgp=c(3,1,0));

  #Obtain corresponding vector of p-values for different endpoints
  bidx <- (1:length(Qb))+1;
  #clusterExport(c1, 'covg', envir=environment())
  sigma2 <- pmax(sapply(X=bidx,FUN=function(idx) covg[idx,idx,]
                        + (-2/(idx-1))*colSums(matrix(covg[idx,1:(idx-1),],nrow=idx-1))
                        + (1/(idx-1)^2)*colSums(matrix(covg[1:(idx-1),1:(idx-1),],nrow=(idx-1)^2))),1e-16);
  #clusterExport(c1, c('sw','sigma2','Qb'), envir=environment())
  pval.b <- sapply(X=1:length(Qb),FUN=function(x) 1-sw(sigma2[,x],Qb[x]));
  #x11();plot(f[(startf+1):endf],pval.b,type="l",xlab="Hz",ylab="p-value",mgp=c(3,1,0));
  names(pval.b) <- f[(startf + 1):endf]
  #Hochberg step up procedure for determining significance
  pval.ord <- sort(pval.b);
  pval.th <- alpha/((endf-startf)-(1:(endf-startf))+1);
  pval.rej <- as.numeric(pval.ord<pval.th);

  stp <- 0;
  i<-length(pval.rej);
  while (stp == 0 & i>=1){
    if (pval.rej[i]==1){
      pval.rej[1:i] <- 1;
      stp <- 1;
    } else{
      i <- i-1;
    }
  }

  pval.idx <- which(pval.b %in% pval.ord[as.logical(pval.rej)])[1]; #smallest significant frequency

  if(is.na(pval.idx)){#if no rejections, set pval index to frequency with smallest p-value
    pval.idx <- which(pval.b==min(pval.b));
    b.sig.ind <- FALSE;
  } else {
    b.sig.ind <- TRUE;
  }

  bhat <- startf+pval.idx;

  #output results for bhat
  out[c('bhat.idx','bhat','bhat.Q','bhat.pval','bhat.pval.th','bhat.sig')] <- c(bhat,f[bhat],Qb[pval.idx],
                                                                                pval.b[pval.idx],pval.th[which(pval.ord==pval.b[pval.idx])[1]],as.logical(b.sig.ind));
  list.out <- list("out" = out, "pvals" = pval.b)
  return(list.out);

}

###function to test for flat spectrum through time (i.e. frequency component contribution same across time)
#' Title
#'
#' @param f
#' @param partfinal
#' @param ghat
#' @param covg
#'
#' @return
#' @noRd
#'
#' @examples used in eba.search to get tests for if spectra have any time varying behavior. Low p value indicates low time varying behavior.
eba.flat <- function(f,partfinal,ghat,covg){

  #f - Fourier frequencies
  #partfinal - final partition using EBA algorithm
  #mtspec - multitaper spectral estimates (N x B)
  #ghat - demeaned multitaper spectral estimates (N x B)
  #covg - estimated covariance of demeaned multitaper spectral estimates for b=1,...B (N x N x B)

  #calculate test statistic for each band
  partidx <- which(f %in% partfinal[-1]); #indices for frequency partition
  nf <- diff(c(0,partidx)); #number of frequencies in each band
  cmsum <- apply(X=ghat,MARGIN=2,FUN=cumsum); #cumulative sum of ghat across frequencies

  #average of ghat within bands for each time point
  if (length(partidx)==1){
    bandmean <- cmsum[partidx[1],,drop=FALSE]/nf;
  } else {
    bandmean <- rbind(cmsum[partidx[1],],diff(cmsum[partidx,]))/nf;
  }

  bandss <- apply(X=bandmean,MARGIN=1,function(x) sum(x^2)); #sum of squared values over time

  #test for significance for each band
  bandmean.var <- matrix(,nrow=nrow(bandmean),ncol=ncol(bandmean));
  for (i in 1:length(partidx)){
    if (i==1){
      bandmean.var[i,] <- apply(X=covg[1:partidx[i],1:partidx[i],],MARGIN=3,FUN=mean);
    } else {
      bandmean.var[i,] <- apply(X=covg[partidx[i-1]:partidx[i],partidx[i-1]:partidx[i],],MARGIN=3,FUN=mean);
    }
  }
  pval <- c();
  for (i in 1:length(partidx)){
    pval[i] <- 1-sw(bandmean.var[i,],bandss[i]);
  }

  #output results
  out <- cbind(partfinal[-1],pval);
  colnames(out) <- c("partfinal","pval.flat");

  return(out);

}


###wrapper function
#' Searches for frequency partitions for univariate time series
#'
#' @param X Vector: univariate time series with a length > 0 and not missing or non-finite values
#' @param N Double: number of of observations per approximately stationary block
#' @param K Double: number of tapers to use in multitaper spectral estimator
#' @param std TRUE/FALSE Boolean: should the variance of each stationary block be standardized to one across all blocks?
#' @param alpha Double: significance level to use for testing partition points using FRESH statistic
#'
#' @return Results on partitions of frequency space  \cr \cr
#' flat: tests to see if the spectra have any time varying behavior. Low p value indicates low time varying behavior. \cr \cr
#' pvals: tests each partition as a possible frequency \cr \cr
#' final: final estimated frequency partition points \cr \cr
#' list: gives you a list of identified frequency bands each pass of the algorithm \cr \cr
#' log: for each pass of he algorithm, gives the indetified frequency, test statistic, thrshiold, pval, and significance (boolean) \cr \cr
#' @export
#' @importFrom momentchi2 sw
#' @examples
#' eba.search(X = eba.simdata(T= 50000)$wn, N = 500, K = 15, std = FALSE, alpha = .05)
#' @details
#' How to run the EBA with the FRESH statistic \cr  \cr
#' 'X' is a vector containing a realization of the time series process you wish to analyze \cr  \cr
#' 'N' is a number representing how many observations should be contained in each approximately stationary block for the estimation procedure of the local approximately stationary power spectrum. For example, N=1000 means that the time series is broken up into approximately stationary segments each containing 1000 observations. Note that N must be significantly smaller than the total length of the time series. \cr \cr
#' 'K' is a number representing how many tapers to use in estimating the approximately stationary local power spectrum using multitaper spectral estimation. Note here that K must be significantly smaller than N in order to achieve reasonable frequency resolution. For example, if you wish to distinguish behavior for frequencies separated by 0.01 or larger (this is the bandwidth) and you had N=1000 observations in each approximately stationary block, then you could have at most K=9 tapers. More generally, using the sine tapers, bw=(K+1)/(N+1). \cr \cr
#' 'std' is a binary indicator to determine if the variance in each stationary block should be standardized to unit variance (std=TRUE) or not (std=FALSE).  \cr  \cr
#' 'alpha' is the significance level for testing each frequency partition. For example, alpha=0.05 corresponds to the 5% significance level or 95% confidence level. \cr \cr
#' Once you have created the inputs pass them into the 'eba.search' function. You can refer to the EBA vignette for an example using demo data. \cr \cr

eba.search <- function(X,N,K,std,alpha){

  #check function arguments and specify defaults
  if(is.numeric(X) !='TRUE'){
    stop("X must be a numeric vector.")
  }else if(length(X) == 0){
    stop("X must have length greater than zero.")
  }else if(any(!is.finite(X))){
    stop("X cannot have missing or non-finite values.")
  }

  if(missing(N)){
    N <- floor(sqrt(T)); #sqrt(T)
    message(paste("Note: N set to ",N,".",sep=""));
  }else if(any(!is.finite(N))){
    stop("N cannot be non-finite or missing.")
  }else if(!(is.numeric(N) == 'TRUE' && length(N)==1 && N%%1==0 && N>0)){
    stop("N must be an integer greater than zero.")
  }else if(N > length(X)){
    stop("N must be smaller than length of X.")
  }else if(N < 30){
    stop("N must be larger for good approximation using FFT within each partition segment.")
  }

  if(missing(K)){
    K <- floor(8*(T+(T/N))/T-1); #K such that number of Fourier frequencies x bandwidth = 4
    message(paste("Note: K set to ",K,".",sep=""));
  }else if(any(!is.finite(K))){
    stop("K cannot be non-finite or missing.")
  }else if(!(is.numeric(K) == 'TRUE' && length(K)==1 && K%%1==0 && K>0)){
    stop("K must be an integer greater than zero.");
  }else if(K > floor(2*N*.15-1)){
    stop("K should be smaller for good frequency resolution.  K=floor(2*N*bandwidth-1) is recommended.")
  }

  #partition series into segments of size N
  Xmat <- partN(X=X,N=N);
  B <- dim(Xmat)[2];

  #linearly detrend data in each partition segment
  Xmat.dt <- apply(X=Xmat,MARGIN=2,function(x) detrend(x,std=std));

  #compute multitaper spectrogram using sine tapers
  X.mtspc <- mtspc(X=Xmat.dt,T=T,N=N,B=B,K=K);

  #de-mean spectral estimates in time
  mu.f <- rowMeans(X.mtspc$mtspec);
  X.ghat <- sweep(X.mtspc$mtspec,1,mu.f);

  #compute covariance of de-meaned spectral estimates
  covg <- gcov(X=X.mtspc$mtspec,B=B,N=N,K=K,tapers=X.mtspc$tapers);

  #recursive loop to find frequency domain partition using max rule
  startf <- 1;
  endf <- length(X.mtspc$f);

  f.part=c(startf,endf); #starting partition
  stp <- 0; #stopping condition
  idx <- 1; #partition index
  out <- c(); #initialize log file
  partlst <- list(X.mtspc$f[f.part]); #initialize list of partitions
  bw <- floor((K+1)*(N/(N+1)))+1; #bandwidth in terms of # of frequencies + 1

  #recursive loop to generate new partition points
  while(stp==0){
    if (f.part[idx+1]-f.part[idx] > 2*bw) {

      #identify changepoint candidates
      tmp <- eba.b(X.dm=X.ghat,f=X.mtspc$f,startf=f.part[idx]+bw,endf=f.part[idx+1]-bw,
                   covg=covg[(f.part[idx]+bw):(f.part[idx+1]-bw),(f.part[idx]+bw):(f.part[idx+1]-bw),],
                   alpha=alpha);
      # Saves the p-values at each frequency for each iteration
      if(idx == 1){
        pvals <- tmp[[2]]
      } else {
        pvals <- c(pvals, tmp[[2]])
      }

      tmp <- tmp[[1]]
      #append run to log file
      out <- rbind(out,tmp);

      #update partition
      f.part.curr <- f.part;
      len.curr <- length(f.part.curr);
      if(tmp['bhat.sig']==1 && tmp['bhat.idx']!=f.part.curr[idx+1]){
        f.part <- as.vector(sort(c(f.part,tmp['bhat.idx'])));
      }

      #update partition list
      if(length(f.part)>len.curr){
        #if updates, append new partition to partition list
        partlst <- c(partlst,list(X.mtspc$f[f.part]));
      }
      #move to next partition point
      idx <- idx+1;

    } else{
      idx <- idx+1; #move to next segment if too small
    }

    #stop once reached last partition point
    stp <- as.numeric(length(f.part)==idx);

  }

  test.flat <- eba.flat(f=X.mtspc$f,partfinal=X.mtspc$f[f.part],ghat=X.ghat,covg=covg);

  # Saves only the last p-value seen at each frequency across iterations, and then formats the resuls

  ### ex.) Iterations 1,2 and 3 had a p-value for the frequency of 0.3 -> Only the p-value from the 3rd iteration is saved
  ### ex.) Iterations 1 and 2 had a p-value for the frequency of 0.15 -> Only the p-value from the 2rd iteration is saved
  ### ex.) Only iteration 1 had a p-value for the frequency of 0.075 -> Only the p-value from the 1st iteration is saved

  r <- rev(pvals); # Ensures we look at the last completed iteration first
  mm <- data.frame("Freq" = names(r), "Pvals" = unname(r));
  mm <- mm[!duplicated(mm[,1]), ] # Ensures no duplicate frequencies are in the below dataframe
  final_vals <- data.frame("Frequency" = as.numeric(rev(mm[,1])), "P-Values" = as.numeric(rev(mm[,2])))

  #compile results
  rownames(out) <- 1:nrow(out);
  results <- list(part.final=X.mtspc$f[f.part],part.list=partlst,log=out,mtspec=X.mtspc,flat=test.flat, pvals = final_vals);
  return(results)
}




###function to simulate data for 3 different settings
#' Simulate time series data
#' @description
#' The function eba.simdata will simulate a time series of the specified length 'T'.
#'
#' @param T numeric/integer with total length of intended time series
#' @return List of 3 frequency series
#' @export
#' @importFrom stats fft
#' @importFrom stats rnorm
#' @examples
#' eba.simdata(T=50000)
#' @details
#' Increasing T will increase the length of the time series.Bellow are brief explanations of each of the returned time series. \cr \cr
#' wn: white noise, random normal distribution with mean of 0 and var = 1 \cr \cr
#' bl: linear example used in paper \cr \cr
#' bs: sinusoidal example used in paper \cr \cr
#'
eba.simdata <- function(T){
  #associated frequencies for DFT
  f <- seq(from=0,by=1/T,length.out=floor(T/2)+1);
  f <- c(f,rev(f[c(-1,-which(f==0.5))]));

  #white noise
  X.wn <- rnorm(T,0,1);

  #nonstationary 3 segments linear
  #low frequency series
  wn1 <- rnorm(T,0,1);
  dft <- fft(wn1)/T; #discrete Fourier transform (DFT)
  dft[which(f>0.15)] <- 0; #0 out frequencies above f=0.15
  wn1.new <- Re(fft(dft,inverse=TRUE));
  wn1.new <- wn1.new/sd(wn1.new);

  #middle band frequency series
  wn2 <- rnorm(T,0,1);
  dft <- fft(wn2)/T; #discrete Fourier transform (DFT)
  dft[which(f<=0.15 | f>0.35)] <- 0; #0 out frequencies not between 0.15 < f <= 0.35
  wn2.new <- Re(fft(dft,inverse=TRUE));
  wn2.new <- wn2.new/sd(wn2.new);

  #high frequency series
  wn3 <- rnorm(T,0,1);
  dft <- fft(wn3)/T; #discrete Fourier transform (DFT)
  dft[which(f<=0.35)] <- 0; #0 out frequencies <= 0.35
  wn3.new <- Re(fft(dft,inverse=TRUE));
  wn3.new <- wn3.new/sd(wn3.new);

  #combine
  coef1 <- seq(from=sqrt(5),to=1,length.out=T);
  coef2 <- seq(from=1,to=1,length.out=T);
  coef3 <- seq(from=1,to=sqrt(5),length.out=T);
  X.3bL <- coef1*wn1.new*sqrt(.3) + coef2*wn2.new*sqrt(.4) + coef3*wn3.new*sqrt(.3);

  #nonstationary 3 segments sinusoidal
  #low frequency series
  wn1 <- rnorm(T,0,1);
  dft <- fft(wn1)/T; #discrete Fourier transform (DFT)
  dft[which(f>0.15)] <- 0; #0 out frequencies above f=0.15
  wn1.new <- Re(fft(dft,inverse=TRUE));
  wn1.new <- wn1.new/sd(wn1.new);

  #middle band frequency series
  wn2 <- rnorm(T,0,1);
  dft <- fft(wn2)/T; #discrete Fourier transform (DFT)
  dft[which(f<=0.15 | f>0.35)] <- 0; #0 out frequencies not between 0.15 < f <= 0.35
  wn2.new <- Re(fft(dft,inverse=TRUE));
  wn2.new <- wn2.new/sd(wn2.new);

  #high frequency series
  wn3 <- rnorm(T,0,1);
  dft <- fft(wn3)/T; #discrete Fourier transform (DFT)
  dft[which(f<=0.35)] <- 0; #0 out frequencies <= 0.35
  wn3.new <- Re(fft(dft,inverse=TRUE));
  wn3.new <- wn3.new/sd(wn3.new);

  #combine
  coef1 <- sqrt(4)*sin(5*pi*seq(0,1,length=T));
  coef2 <- sqrt(4)*cos(5*pi*seq(0,1,length=T));
  coef3 <- sqrt(32)*cos(5*pi*seq(0,1,length=T));
  X.3bS <- coef1*wn1.new*sqrt(.3) + coef2*wn2.new*sqrt(.4) + coef3*wn3.new*sqrt(.3)+rnorm(T,0,1);

  return(list(wn=X.wn,bL=X.3bL,bS=X.3bS));

}


