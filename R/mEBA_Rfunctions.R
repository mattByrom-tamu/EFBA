#function to linearly detrend and standardize time series
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

#ggplot settings
#' @importFrom ggplot2 theme_gray
#' @export
hw <- function(){
  theme_gray()+ theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  plot.caption=element_text(hjust=-.5),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), linewidth=.2),
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.10,"cm"),
  panel.spacing.y = unit(0.05,"cm"),
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black")
  )
}

#function to detrend, standardize, and filter out low frequencies (below 1 Hz)
#' @export
#' @importFrom signal butter
#' @importFrom signal filter
preprocess <- function(sgnl,dsfrq,channels){

  out <- sgnl[seq(1,nrow(sgnl),by=512/dsfrq),] # downsample to 64Hz

  Ts <- nrow(out) #number of time points
  N <- 2*floor(Ts^0.7)-floor(Ts^0.7/2)*2; #neighborhood for local periodogram
  ff <- seq(0,floor(N/2),by=1)/N #Fourier frequencies given N
  idx1Hz <- which(ff*dsfrq > 1)[1] #index for Fourier frequency at 1 Hz
  bf <- butter(4, idx1Hz/N, type="high") # high pass filter at 1 Hz


  out <- apply(out,2,function(x) detrend(x,TRUE)) #detrend and standardize variance
  out <- apply(out,2,function(x) signal::filter(bf, x)) #filter out frequencies below 1 Hz
  colnames(out) <- channels

  return(list(signal=out,Ts=Ts,N=N,ff=ff))
}

#function to simulate data for 5 different settings
#' @export
meba.simdata <- function(t){

  # set.seed(seed);

  #associated frequencies for DFT
  f <- seq(from=0,by=1/t,length.out=floor(t/2)+1);
  f <- c(f,rev(f[c(-1,-which(f==0.5))]));

  #white noise
  X.wn <- rnorm(t,0,1);
  #X.wn <- arima.sim(n=t,list(ar=0.9));

  #nonstationary 3 segments linear

  #low frequency series
  wn1 <- rnorm(t,0,1);
  dft <- fft(wn1)/t; #discrete Fourier transform (DFT)
  dft[which(f>0.15)] <- 0; #0 out frequencies above f=0.15
  wn1.new <- Re(fft(dft,inverse=TRUE));

  # #middle band frequency series
  wn2 <- rnorm(t,0,1);
  dft <- fft(wn2)/t; #discrete Fourier transform (DFT)
  dft[which(f<=0.15 | f>0.35)] <- 0; #0 out frequencies not between 0.15 < f <= 0.35
  wn2.new <- Re(fft(dft,inverse=TRUE));

  #high frequency series
  wn3 <- rnorm(t,0,1);
  dft <- fft(wn3)/t; #discrete Fourier transform (DFT)
  dft[which(f<=0.35)] <- 0; #0 out frequencies <= 0.35
  wn3.new <- Re(fft(dft,inverse=TRUE));

  #combine
  coef1 <- seq(from=10,to=1,length.out=t);
  coef2 <- seq(from=1,to=1,length.out=t);
  coef3 <- seq(from=1,to=10,length.out=t);
  X.3bL <- coef1*wn1.new + coef2*wn2.new + coef3*wn3.new;

  #nonstationary 3 segments sinusoidal
  #low frequency series
  wn1 <- rnorm(t,0,1);
  dft <- fft(wn1)/t; #discrete Fourier transform (DFT)
  dft[which(f>0.15)] <- 0; #0 out frequencies above f=0.15
  wn1.new <- Re(fft(dft,inverse=TRUE));

  #middle band frequency series
  wn2 <- rnorm(t,0,1);
  dft <- fft(wn2)/t; #discrete Fourier transform (DFT)
  dft[which(f<=0.15 | f>0.35)] <- 0; #0 out frequencies not between 0.15 < f <= 0.35
  wn2.new <- Re(fft(dft,inverse=TRUE));

  #high frequency series
  wn3 <- rnorm(t,0,1);
  dft <- fft(wn3)/t; #discrete Fourier transform (DFT)
  dft[which(f<=0.35)] <- 0; #0 out frequencies <= 0.35
  wn3.new <- Re(fft(dft,inverse=TRUE));

  #combine
  coef1 <- 10*(sin(4*pi*seq(0,1,length=t)-pi/2)+1);
  coef2 <- 5*(cos(4*pi*seq(0,1,length=t))+1);
  coef3 <-8.5*(sin(3*pi*seq(0,1,length=t)-pi/16)+1);
  X.3bS <- (coef1)*wn1.new + (coef2)*wn2.new + (coef3)*wn3.new;

  #nonstationary 2 segments linear 0.15
  #low frequency series
  wn1 <- rnorm(t,0,1);
  dft <- fft(wn1)/t; #discrete Fourier transform (DFT)
  dft[which(f>0.15)] <- 0; #0 out frequencies above f=0.15
  wn1.new <- Re(fft(dft,inverse=TRUE));

  #high frequency series
  wn2 <- rnorm(t,0,1);
  dft <- fft(wn2)/t; #discrete Fourier transform (DFT)
  dft[which(f<=0.15)] <- 0; #0 out frequencies <= 0.15
  wn2.new <- Re(fft(dft,inverse=TRUE));

  #combine
  coef1 <- seq(from=10,to=1,length.out=t);
  coef2 <- seq(from=1,to=10,length.out=t);
  X.2bL15 <- coef1*wn1.new + coef2*wn2.new;

  #nonstationary 2 segments linear 0.35
  #low frequency series
  wn1 <- rnorm(t,0,1);
  dft <- fft(wn1)/t; #discrete Fourier transform (DFT)
  dft[which(f>0.35)] <- 0; #0 out frequencies above f=0.15
  wn1.new <- Re(fft(dft,inverse=TRUE));

  #high frequency series
  wn2 <- rnorm(t,0,1);
  dft <- fft(wn2)/t; #discrete Fourier transform (DFT)
  dft[which(f<=0.35)] <- 0; #0 out frequencies <= 0.15
  wn2.new <- Re(fft(dft,inverse=TRUE));

  #combine
  coef1 <- seq(from=10,to=1,length.out=t);
  coef2 <- seq(from=1,to=10,length.out=t);
  X.2bL35 <- coef1*wn1.new + coef2*wn2.new;

  #nonstationary 2 segments sinusoidal
  #low frequency series
  wn1 <- rnorm(t,0,1);
  dft <- fft(wn1)/t; #discrete Fourier transform (DFT)
  dft[which(f>0.15)] <- 0; #0 out frequencies above f=0.35
  wn1.new <- Re(fft(dft,inverse=TRUE));

  #high frequency series
  wn2 <- rnorm(t,0,1);
  dft <- fft(wn2)/t; #discrete Fourier transform (DFT)
  dft[which(f<=0.15)] <- 0; #0 out frequencies <= 0.35
  wn2.new <- Re(fft(dft,inverse=TRUE));

  #combine
  coef1 <- 10*(sin(4*pi*seq(0,1,length=t)-pi/2)+1);
  coef2 <- 5*(cos(4*pi*seq(0,1,length=t))+1);
  X.2bS15 <- coef1*wn1.new + coef2*wn2.new;


  #nonstationary 2 segments sinusoidal
  #low frequency series
  wn1 <- rnorm(t,0,1);
  dft <- fft(wn1)/t; #discrete Fourier transform (DFT)
  dft[which(f>0.35)] <- 0; #0 out frequencies above f=0.35
  wn1.new <- Re(fft(dft,inverse=TRUE));

  #high frequency series
  wn2 <- rnorm(t,0,1);
  dft <- fft(wn2)/t; #discrete Fourier transform (DFT)
  dft[which(f<=0.35)] <- 0; #0 out frequencies <= 0.35
  wn2.new <- Re(fft(dft,inverse=TRUE));

  #combine
  coef1 <- 10*(sin(4*pi*seq(0,1,length=t)-pi/2)+1);
  coef2 <- 5*(cos(4*pi*seq(0,1,length=t))+1);
  X.2bS35 <- coef1*wn1.new + coef2*wn2.new;


  return(list(wn=X.wn,bL=X.3bL,bS=X.3bS,
              bL2f15 = X.2bL15, bL2f35 = X.2bL35,
              bS2f15 = X.2bS15, bS2f35 = X.2bS35));

}
