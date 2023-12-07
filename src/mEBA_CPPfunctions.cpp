//date 01/09/2023

#define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
#include <omp.h>
#define _USE_MATH_DEFINES
#include <math.h>
#include <string>
#include <sstream>
#include <cmath>
#include <complex> 
#include <limits>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]

using namespace Rcpp;
using namespace arma;
using namespace std;

//declaring functions
arma::cx_cube fhat(arma::mat X, int N, bool stdz);
arma::cx_cube ghat(arma::cx_cube fhat);
arma::mat tsbootH0(arma::mat x, arma::mat rndraw, int ncore);
arma::field<mat> dfstat(arma::cx_cube ghat, int N, int Wsel);

// [[Rcpp::export]]
Rcpp::List msboot(int nrep, arma::mat x, int Wsel, bool stdz, int ncore){
  
  int T=x.n_rows;  //length of time series
  int ns=x.n_cols;  //number of components
  int N=2*floor(pow(T,0.7))-floor(pow(T,0.7)/2)*2; //neighborhood for local periodogram
  int Fs=floor(N/2)+1; //number of Fourier frequencies
  
  arma::mat freq(Fs,1,fill::zeros);
  for (int i = 0; i < Fs; i++){
    freq(i,0)=1.0*i/N;
  }  
  
  int Wlo = floor(N/12); 
  int Whi = floor(N/4);
  
  int d = abs(Whi-Wlo)+1;
  
  if (d<Wsel){
    Wsel=d;
    Rprintf("Wsel is too high and lowered to Wsel = %i \n",Wsel);
  }
  
  int skp = floor(d/Wsel);
  
  arma::field<arma::mat> dfobsall(Wsel);
  arma::field<arma::mat> dfbootall(Wsel);
  arma::field<arma::mat> dfpvalall(Wsel);
  arma::field<arma::mat> dfobs2all(Wsel);
  arma::field<arma::cube> dfboot2all(Wsel);
  arma::field<arma::mat> dfpval2all(Wsel);
  
  Rprintf("Calculating local periodogram and demeaned local periodogram\n");
  arma::cx_cube pse = fhat(x,N,stdz); 
  arma::cx_cube gpse = ghat(pse);
  arma::mat freqsig(Fs,2,fill::zeros); //significant partition points vec
  arma::mat freqcand(Fs,2,fill::ones); //candidate partition points vec
  
  freqsig.col(0)=freq;
  freqcand.col(0)=freq;
  
  int idxw = 0; 
  for (int W = Wlo; W <= min(Whi,Wlo+Wsel*skp); W += skp){
   if (idxw<Wsel){
      Rprintf("\nW is %i \n",W);
 
      arma::mat dfobs(Fs-2*W,1); //vector of test statistics
      arma::mat dfboot(Fs-2*W,nrep); //matrix of bootstrap samples of test statistic under H0
      arma::mat dfobs2(Fs-2*W,gpse.n_cols); //vector of component-level test statistics
      arma::cube dfboot2(Fs-2*W,gpse.n_cols,nrep); //matrix of bootstrap samples of test statistic under H0
      arma::mat dfpval(Fs-2*W,1,fill::zeros); //vector of p-values
      arma::mat dfpval2(Fs-2*W,gpse.n_cols,fill::zeros); //vector of p-values for component-level test statistic
      arma::cube rndraws(ns,T,nrep,fill::randn); //random normal draws for ts bootstrap
  
      // 1. compute observed test statistics for each frequency
      Rprintf("Calculating observed test statistics\n");
      #pragma omp parallel for num_threads(ncore)
      for(int i = W; i < (Fs-W); i++){
        Rprintf("\r                                                                ");
        Rprintf("\rCalculating test statistic for Fourier frequency %i of %i",i-W+1,Fs-2*W);
        double tmp1 = 0.0;
        arma::cx_mat tmp3(gpse.n_cols,gpse.n_slices,fill::zeros);
        for(int j = 0; j < W; j++){
          tmp1 += accu(square(abs(gpse.row(i-j) - gpse.row(i+j))))/(T*W);
          tmp3 += (gpse.row(i-j) - gpse.row(i+j))/(T*W);
        }
        dfobs(i-W) = tmp1;
        dfobs2.row(i-W) = trans(square(abs(sum(tmp3,1))));
      }
      Rprintf("\r                                                                ");
      Rprintf("\rCalculated observed test statistics\n");
      
      // 2. compute bootstrap draws of test stat under H0 and pvalues
      for (int r = 0; r < nrep; r++){
        Rprintf("\rGenerating bootstrap test statistic %i of %i.",r+1,nrep);
        
        //draw new time series under H0 and estimate demeaned spectrum
        arma::mat tvwnout = tsbootH0(x,rndraws.slice(r),ncore);
        arma::cx_cube pse0 = fhat(tvwnout,N,stdz);
        arma::cx_cube gpse0 = ghat(pse0);
        
        //compute test statistics and pvalues for each frequency
       #pragma omp parallel for num_threads(ncore)
       for(int i = W; i < (Fs-W); i++){
          double tmp2 = 0.0;
          arma::cx_mat tmp4(gpse0.n_cols,gpse0.n_slices,fill::zeros);
          for(int j = 0; j < W; j++){
            tmp2 += accu(square(abs(gpse0.row(i-j) - gpse0.row(i+j))))/(T*W);
            tmp4 += (gpse0.row(i-j) - gpse0.row(i+j))/(T*W);
          }
          dfboot(i-W,r) = tmp2;
          dfpval(i-W) += (dfboot(i-W,r)>dfobs(i-W))/(nrep*1.0);
          dfboot2(span(i-W),span(0,dfboot2.n_cols-1),span(r)) = trans(square(abs(sum(tmp4,1))));
          arma::mat tmp5 = dfboot2.subcube(i-W,0,r,i-W,dfboot2.n_cols-1,r);
          arma::umat tmp6 = tmp5 > dfobs2.submat(i-W,0,i-W,dfobs2.n_cols-1); 
          arma::mat tmp7 = arma::conv_to<arma::mat>::from(tmp6);
          dfpval2.row(i-W) += tmp7/(nrep*1.0);
        }
      }
      
      // 3. select frequency partition points
      freqcand(span(0,W-1),1).for_each( [](mat::elem_type& val) { val=0; } ); //scrub frequencies at ends
      freqcand(span((Fs-W),Fs-1),1).for_each( [](mat::elem_type& val) { val=0; } ); //scrub frequencies at ends
      
      //scrub W around existing cut points
      for (int i=0; i < Fs; i++){
        if (freqsig(i,1)==1){
          freqcand(span(std::max(0,i-W),std::min(Fs-1,i+W)),1).for_each( [](mat::elem_type& val) { val=0; } ); //scrub frequencies within W
        }
      }
      
      uvec srtidx = sort_index(dfobs,"descend"); //ordering of largest test stats
      
      //bonferroni correction for candidate testing points
      double thr = 0.05/accu(freqcand.col(1));
      
      int stp=0;
      while(stp==0){
         for (int i=0; i < (int)srtidx.n_rows; i++){
            if ((dfpval(srtidx(i))<thr) & (freqcand(W+srtidx(i),1)==1)){
              freqsig(W+srtidx(i),1) = 1; //add frequency to partition points
              freqcand(span(std::max(0,1+(int)srtidx(i)),std::min(Fs-1,2*W+(int)srtidx(i))),1).for_each( [](mat::elem_type& val) { val=0; } );; //scrub frequencies within W
              Rprintf("\nfrequency identified is %f \n",freqsig(W+srtidx(i),0));
              Rprintf("p-value is %f and threshold is %f \n",dfpval(srtidx(i)),thr);
            } else{
              stp=1;
            }
         }
      }

      // 4. save to fields
      dfobsall(idxw) = join_horiz(freq.rows(W,Fs-W-1),dfobs); //observed test statistics for all frequencies
      dfbootall(idxw) = join_horiz(freq.rows(W,Fs-W-1),dfboot); //observed test statistics for all frequencies
      dfpvalall(idxw) = join_horiz(freq.rows(W,Fs-W-1),dfpval);
      
      dfobs2all(idxw) = dfobs2; //observed test statistics for all frequencies
      dfboot2all(idxw) = dfboot2; //observed test statistics for all frequencies
      dfpval2all(idxw) = join_horiz(freq.rows(W,Fs-W-1),dfpval2);
      idxw += 1;
   }
  }

  Rcpp::List out=List::create(dfobsall,dfbootall,dfpvalall,freqsig,
                              dfobs2all,dfboot2all,dfpval2all);
  return out;
}

// [[Rcpp::export]]
arma::mat tsbootH0(arma::mat x, arma::mat rndraws, int ncore){
  
  bool par=FALSE;
  int n=x.n_rows;
  int ns=x.n_cols;
  float h=pow(n,-0.3);

  //demean columns of x and vectorise
  x-=repmat(mean(x,0),n,1);
  arma::mat xv = vectorise(x);
  
  //create triangular kernel
  arma::mat kh(n,n);
  arma::mat uv=linspace<mat>(0,1,n);
  // #pragma omp parallel for num_threads(ncore)
  for(int v = 0; v < n; v++){
    for(int u = 0; u < n; u++){
      kh(u,v) = abs((uv(u)-uv(v))*pow(h,-1))<1 ? pow(h,-1)*(1-abs((uv(u)-uv(v))*pow(h,-1))) : 0;
    }
    //scale such that kernel has mean 1 for all time points
    kh.col(v) /= mean(kh.col(v));
  }
  
  //calculate kernel weighted time-varying contemporaneous covariance (lag 0)
  arma::cx_cube tcov(ns,ns,n);
  // #pragma omp parallel for num_threads(ncore) collapse(3)
  for(int u = 0; u < ns; u++){
    for(int v = 0; v < ns; v++){
      for (int k = 0; k < n;k++){
        double sum = 0.0;
        for(int i = 0; i < n; i++){
          sum += xv(i + n*u) * xv(i + n*v) * kh(i,k);
        }
        tcov(u,v,k) = sum/n;
      }
    }
  }
  
  //computes estimate of sigma(t/T)
  arma::cx_cube tmp=tcov;
  tmp.each_slice([&](arma::cx_mat& X){X=sqrtmat(X);},par);
  
  //computes estimate of time-varying WN
  arma::mat tvwnout(n,ns,fill::zeros);
  for(int i = 0; i < n; i++){
    tvwnout.row(i) = trans(real(tmp.slice(i)*rndraws.col(i)));
  }
  
  return tvwnout;
  
}

// [[Rcpp::export]]
arma::cx_cube fhat(arma::mat X, int N, bool stdz){
  //inputs
  //X time series with T rows and R columns (column vectors are components of MV time series)
  //N is neighborhood for local periodogram (must be even)
    
  //initialization
  int R=X.n_cols; //number of components of MV time series
  int Ts=X.n_rows; //total length of time series
  int Fs=floor(N/2)+1; //number of Fourier frequencies for local periodogram
  int Nh=floor(N/2); //half width of neighborhood
  arma::mat xmat=join_horiz(ones(N,1),linspace(1,N,N));
  arma::mat vec(N,1);
  arma::mat linfit(2,1);
  arma::cube Xnew(N,Ts,R);
  arma::cx_mat fftmat(N,R);
  arma::cx_cube pgram(Fs,pow(R,2),Ts,fill::zeros);
  
  //throw error if N is not even or longer than T 
  ostringstream text;
  if(N%2!=0){
    text << "N should be a positive even integer less than the length of the time series.";
    stop(text.str());
  }
  if(N>Ts){
    text << "N should be a positive even integer less than the length of the time series.";
    stop(text.str());
  }
  
  //compute local periodogram estimates
  for(int i=0;i<Ts;i++){
    for (int j=0;j<R;j++){
      
      //linearly detrend
      if (i<Nh){
        vec=X(span(0,N-1),j);
      } else if(i>=Ts-Nh){
        vec=X(span(Ts-N,Ts-1),j);
      } else{
        vec=X(span(i-Nh+1,i+Nh),j);
      }
      
      linfit=inv(xmat.t()*xmat)*xmat.t()*vec;
      vec=vec-xmat*linfit;
      
      //standard to unit variance
      if(stdz){vec=vec/repelem(stddev(vec),N,1);}
      
      //save to Xnew
      Xnew(span(0,N-1),span(i,i),span(j,j))=vec;
      
      //fft
      fftmat.col(j)=fft(vec)/sqrt(2.0*M_PI*N);
    }
    
    arma::cx_mat tmp(Fs,pow(R,2));
    
    //periodogram estimator (vectorized)
    for(int f=0;f<Fs;f++){
      tmp.row(f)=strans(vectorise(strans(fftmat.row(f))*conj(fftmat.row(f))));
    }
    pgram.slice(i) = tmp;
  }

  return pgram;
}

// [[Rcpp::export]]
arma::cx_cube ghat(arma::cx_cube fhat){
  
  arma::cx_cube ghat=fhat;
  arma::cx_mat tmp(fhat.n_rows,fhat.n_cols);
  
  // demeaned multitaper estimator (g)
  tmp = mean(fhat,2);
  ghat.each_slice() -= tmp;
  return ghat;
}