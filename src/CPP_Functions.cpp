#include <RcppArmadillo.h>
// #include <RcppEigen.h>
// #include <ParallelExecution.h>

#define ARMA_64BIT_WORD 1
#define _USE_MATH_DEFINES
#include <omp.h>
#include <math.h>
#include <string>
#include <sstream>
#include <cmath>
#include <complex>
#include <limits>
// #include <thread>

//[[Rcpp::depends(RcppArmadillo)]]
// // [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using namespace arma;
using namespace std;
// using Eigen::Map;                       // 'maps' rather than copies
// using Eigen::MatrixXd;                  // variable size matrix, double precision
// using Eigen::VectorXd;                  // variable size vector, double precision
// using Eigen::SelfAdjointEigenSolver;    // one of the eigenvalue solvers

//declaring functions
arma::cx_cube fhat_lp(arma::mat X, int N, bool stdz);
arma::cx_cube ghat(arma::cx_cube fhat);
arma::mat tsbootH0(arma::mat x, arma::mat rndraw, int ncore);
arma::field<mat> dfstat(arma::cx_cube ghat, int N, int Wsel);

//' @export
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
    arma::cx_cube pse = fhat_lp(x,N,stdz);
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
          arma::cx_cube pse0 = fhat_lp(tvwnout,N,stdz);
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

//' @export
  // [[Rcpp::export]]
  arma::cx_cube fhat_lp(arma::mat X, int N, bool stdz){
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

//' Generate Multitaper Estimator of Power Spectrum for Functional Data
//' @description
//' This function takes in the aforementioned functional data, as well as a few other parameters, and returns the multitaper
//' estimate of the power spectrum for the data. This works in a piecewise manner, hence why it has the suffix _pmt, for piecewise
//' multitaper
//'
//' @param X A T X R matrix of the functional data, where T is the length of the time series, and R is the number of components.
//' @param K The number of tapers to utilize in estimating the local power spectrum. K must satisfy: 1 <= K < floor(N/4 - 1)
//' @param N The number of observations that will be contained in each roughly stationary block. N must satisfy: 30 <= N <= T
//' @param Rsel The number of points within the functional domain that will be used for computing test statistics. Rsel must satisfy: 1 <= Rsel <= R
//' @param stdz A binary indicator to show whether the variance in each stationary block should be standardized.
//' @return A 3-Dimensional matrix containing the multitaper estimator. Its dimensions are A x B x C, where A = floor(N/2 + 1), B = Rsel^2, and C = floor(T / N)
//' @examples
//' nb=15; #number of basis functions used to generate white noise
//' R=5; #number of points in functional domain
//' Ts=2000; #length of time series
//' seed=234; #seed for reproducibility
//' X=fws.sim(nb=nb,gsz=R,Ts=Ts,seed=seed);
//' ##estimate and visualize power spectrum estimates for specific components
//' B=5; #number of time blocks
//' N=Ts/B; #number of observations per time block
//' bw=0.04; #bandwidth for multitaper spectral estimator
//' K=max(1,floor(bw*(N+1)-1)); #number of tapers for multitaper spectral estimator
//' std=FALSE; #standardize variance for points in functional domain (TRUE) or not (FALSE)
//' freq=seq(from=0,by=1/N,length.out=floor(N/2)+1); #Fourier frequencies
//' Rsel=4; #number of points in functional domain used for test statistics
//' pse=fhat_pmt(X,N,K,Rsel,std);
//' @details
//'  Every input must be either a Boolean or Numeric, as mentioned above \cr \cr
//' For more information on how this data is simulated, consult the corresponding paper at https://arxiv.org/abs/2102.01784
//' @export
// [[Rcpp::export]]
 arma::cx_cube fhat_pmt(arma::mat X, int N, int K, int Rsel, bool stdz){

   //reduce components of X if Rsel<R
   int Rcurr=X.n_cols;
   int Ts=X.n_rows;
   arma::uvec Ridx(Rsel);
   arma::mat Xnew(Ts,Rsel);
   if(Rsel<Rcurr){
     Ridx=conv_to<uvec>::from(round(linspace(0,Rcurr-1,Rsel)));
     Xnew=X.cols(Ridx);
   }else if(Rsel>Rcurr){
     stop("Inadmissible value: Rsel is greater than R");
   }else{
     Xnew=X;
   }

   //initialization
   int R=Xnew.n_cols;
   int B=floor(Ts/N);
   int Fs=floor(N/2)+1;
   int drp=floor(Ts%N/2+0.5);

   arma::mat xmat=join_horiz(ones(N,1),linspace(1,N,N));
   arma::mat vec(N,1);
   arma::mat linfit(2,1);
   arma::mat tmp(N,K);
   arma::mat tapers=zeros(N,K);

   arma::cx_mat tmpfft(N,K);
   arma::cx_mat tmp2(Fs,R);
   arma::cx_mat tmp3(Fs,1);
   arma::cx_mat tmp4(Fs,pow(R,2));
   arma::cx_mat tmp6(Fs,pow(R,2));

   arma::cx_cube lmtse(B,R,R);
   arma::cx_cube fftcb(N,K,R);
   arma::cx_cube tmp5(Fs,pow(R,2),K);
   arma::cx_cube mtspec(Fs,pow(R,2),B);

   //sine tapers
   for(int i=0;i<N;i++){
     for(int j=0;j<K;j++){
       tapers(i,j)=sqrt(2/double(N+1))*sin(M_PI*(i+1)*(j+1)/double(N+1));
     }
   }

   for(int i=0;i<B;i++){
     for (int j=0;j<R;j++){
       //linearly detrend
       vec=Xnew(span(i*N+drp,(i+1)*N+drp-1),j);
       linfit=inv(xmat.t()*xmat)*xmat.t()*vec;
       vec=vec-xmat*linfit;
       //standard to unit variance
       if(stdz){vec=vec/repelem(stddev(vec),N,1);}
       //fft
       tmp=tapers;
       tmp.each_col() %= vec;
       fftcb.slice(j)=fft(tmp);
     }

     //multitaper spectral estimator
     for(int k=0;k<K;k++){
       tmp2=fftcb.subcube(0,k,0,Fs-1,k,R-1);
       for(int j=0;j<R;j++){
         tmp3=conj(fftcb.subcube(0,k,j,Fs-1,k,j));
         tmp4.submat(0,j*R,Fs-1,(j+1)*R-1)=sqrt(repmat(tmp3,1,R) % tmp2);
       }
       tmp5.slice(k)=tmp4;
     }
     mtspec.slice(i)=mean(tmp5,2);
   }

   //throw warning if observations discarded
   ostringstream text;
   if(Ts%N!=0){
     if(Ts%N%2!=0){
       text << "Warning: T is not a multiple of N. First "
            << drp << " and last " << drp+1 << " observations have been discarded.";
       warning(text.str());
     }else if(Ts%N%2==0){
       text << "Warning: T is not a multiple of N. First "
            << drp << " and last " << drp << " observations have been discarded.";
       warning(text.str());
     }
   }
   return mtspec;
 }

//' @export
 // [[Rcpp::export]]
 arma::cx_cube ghat(arma::cx_cube fhat_pmt){

   arma::cx_cube ghat=fhat_pmt;
   arma::cx_mat tmp(fhat_pmt.n_rows,fhat_pmt.n_cols);

   // demeaned multitaper estimator (g)
   tmp = mean(fhat_pmt,2);
   ghat.each_slice() -= tmp;
   return ghat;
 }

arma::mat Qtsfn(arma::cx_cube ghat){

  int Fs=ghat.n_rows;
  int Rsq=ghat.n_cols;
  int B=ghat.n_slices;

  arma::mat Qts(Fs-1,Rsq);

  for(int i=1;i<Fs;i++){
    arma::cx_mat tmp4(1,Rsq,fill::zeros);
    for (int j=0;j<B;j++){
      arma::cx_mat tmp=ghat.subcube(0,0,j,i,Rsq-1,j);
      arma::cx_mat tmp2=mean(tmp.rows(0,i-1),0);
      arma::cx_mat tmp3=tmp.row(i);
      tmp4=tmp4+(tmp3-tmp2)%conj(tmp3-tmp2);
    }
    Qts.row(i-1)=real(tmp4);
  }

  return Qts;
}

arma::vec Qintfn(arma::mat Qts){
  int Fs=Qts.n_rows;
  int Rsq=Qts.n_cols;

  arma::vec Qint(Fs);
  arma::vec tmp(Fs);
  tmp.fill(Rsq);
  Qint = sum(Qts,1)/tmp;

  return Qint;

}

std::complex<double> covghat(arma::cx_cube fhat_pmt,
                             int f1,int f2,
                             int b1, int b2,
                             int tau1,int sig1,
                             int tau2,int sig2){

  int R=sqrt(fhat_pmt.n_cols);
  int Bi=fhat_pmt.n_slices;
  std::complex<double> Bc=fhat_pmt.n_slices;
  std::complex<double> cghat;
  std::complex<double> one(1,0);
  std::complex<double> negone(-1,0);
  std::complex<double> two(2,0);

  if (b1==b2){
    cghat=(one-(two/Bc))*(fhat_pmt(f1,tau1+R*tau2,b1)*fhat_pmt(f2,sig1+R*sig2,b1)+
      fhat_pmt(f1,tau1+R*sig2,b1)*fhat_pmt(f2,tau2+R*sig1,b1))+
      (one/pow(Bc,2))*accu(fhat_pmt.subcube(f1,tau1+R*tau2,0,f1,tau1+R*tau2,Bi-1)%
      fhat_pmt.subcube(f2,sig1+R*sig2,0,f2,sig1+R*sig2,Bi-1)+
      fhat_pmt.subcube(f1,tau1+R*sig2,0,f1,tau1+R*sig2,Bi-1)%
      fhat_pmt.subcube(f2,tau2+R*sig1,0,f2,tau2+R*sig1,Bi-1));

  } else{
    cghat=(negone/Bc)*(fhat_pmt(f1,tau1+R*tau2,b1)*fhat_pmt(f2,sig1+R*sig2,b1)+
      fhat_pmt(f1,tau1+R*sig2,b1)*fhat_pmt(f2,tau2+R*sig1,b1))+
      (negone/Bc)*(fhat_pmt(f1,tau1+R*tau2,b2)*fhat_pmt(f2,sig1+R*sig2,b2)+
      fhat_pmt(f1,tau1+R*sig2,b2)*fhat_pmt(f2,tau2+R*sig1,b2))+
      (one/pow(Bc,2))*accu(fhat_pmt.subcube(f1,tau1+R*tau2,0,f1,tau1+R*tau2,Bi-1)%
      fhat_pmt.subcube(f2,sig1+R*sig2,0,f2,sig1+R*sig2,Bi-1)+
      fhat_pmt.subcube(f1,tau1+R*sig2,0,f1,tau1+R*sig2,Bi-1)%
      fhat_pmt.subcube(f2,tau2+R*sig1,0,f2,tau2+R*sig1,Bi-1));
  }

  return(cghat);
}

std::complex<double> covGnull(arma::cx_cube fhat_pmt,
                              int b1, int b2,
                              int tau1,int sig1,
                              int tau2,int sig2){

  int di=fhat_pmt.n_rows;
  std::complex<double> dc=fhat_pmt.n_rows;
  std::complex<double> one(1,0);
  std::complex<double> negone(-1,0);
  std::complex<double> covGnullout;

  arma::cx_vec sumb1b2vec(di-1);
  arma::cx_vec sumb2b1vec(di-1);
  arma::cx_mat sumf1f2mat(di-1,di-1);

  for (int i=0;i<(di-1);i++){
    sumb1b2vec(i)=covghat(fhat_pmt,di-1,i,b1,b2,tau1,sig1,tau2,sig2);
    sumb2b1vec(i)=covghat(fhat_pmt,di-1,i,b2,b1,tau2,sig2,tau1,sig1);
    for (int j=0;j<(di-1);j++){
      sumf1f2mat(i,j)=covghat(fhat_pmt,i,j,b1,b2,tau1,sig1,tau2,sig2);
    }
  }

  std::complex<double> sumb1b2=sum(sumb1b2vec);
  std::complex<double> sumb2b1=sum(sumb2b1vec);
  std::complex<double> sumf1f2=accu(sumf1f2mat);

  covGnullout=covghat(fhat_pmt,di-1,di-1,b1,b2,tau1,sig1,tau2,sig2)+
    (one/pow(dc,2))*sumf1f2+(negone/dc)*sumb1b2+(negone/dc)*sumb2b1;

  return covGnullout;
}

arma::cx_vec rcnormRcpp(int n){

  arma::cx_vec rcn(n,fill::randn);
  arma::vec realvec(n,fill::randn);
  arma::vec imgvec(n,fill::randn);
  arma::vec sqrthalf(n);
  std::complex<double> i(0,1);

  sqrthalf.fill(sqrt(0.5));
  rcn=sqrthalf%(realvec+i*imgvec);
  return rcn;
}

// Eigen::MatrixXcd cast_eigen(arma::cx_mat arma_A) {
//   Eigen::MatrixXcd eigen_B = Eigen::Map<Eigen::MatrixXcd>(arma_A.memptr(),
//                                                         arma_A.n_rows,
//                                                         arma_A.n_cols);
//   return eigen_B;
// }
//
// arma::cx_mat cast_arma_cxmat(Eigen::MatrixXcd eigen_A) {
//   arma::cx_mat arma_B = arma::cx_mat(eigen_A.data(), eigen_A.rows(), eigen_A.cols(),
//                                false, false);
//   return arma_B;
// }
//
// arma::vec cast_arma_vec(Eigen::VectorXd eigen_A) {
//   arma::vec arma_B = arma::vec(eigen_A.data(), eigen_A.rows(),
//                                      false, false);
//   return arma_B;
// }

arma::cx_mat rcmvnormRcpp(int n, arma::cx_mat mean, arma::cx_mat sigma){

  // //RCppEigen BDCSVD for fast SVD computation - seems to produce more false positives than armadillo
  // Eigen::MatrixXcd sigma2=cast_eigen(sigma);
  // Eigen::BDCSVD<Eigen::MatrixXcd> SVD(sigma2, Eigen::ComputeThinU | Eigen::ComputeThinV);
  // arma::vec s=cast_arma_vec(SVD.singularValues());
  // arma::cx_mat U=cast_arma_cxmat(SVD.matrixU());
  // arma::mat smat=diagmat(sqrt(s));
  // arma::cx_mat retval= U*smat*U.t();
  //
  // // Rcout << s << std::endl;
  // // Rcout << U << std::endl;
  // // Rcout << retval << std::endl;
  //
  // double eps=std::numeric_limits<double>::epsilon();
  // arma::uvec ids = find(s < -1*sqrt(eps)*abs(s(1)));
  // if(!ids.is_empty()){
  //   warning("sigma is numerically not positive definite");
  // }
  //
  // arma::cx_mat out;
  // int ncolsig = sigma.n_cols;
  // out=rcnormRcpp(n*ncolsig);
  // out.reshape(ncolsig,n);
  // out=conj(out.st()*retval);
  // out.each_row() += mean.st();

  // // Armadillo matrix square root - also works but a bit slow - try in case svd fails
  // retval=sqrtmat(sigma);

  // Armadillo svd version - default "dc" method sometimes fails for larger matrices - "std" too slow
  int ncolsig = sigma.n_cols;

  double eps=std::numeric_limits<double>::epsilon();

  arma::cx_mat U;
  arma::vec s;
  arma::mat smat;
  arma::cx_mat V;
  arma::cx_mat retval;
  arma::cx_mat retval2;
  arma::cx_mat out;

  svd(U,s,V,sigma);
  arma::uvec ids = find(s < -1*sqrt(eps)*abs(s(1)));
  if(!ids.is_empty()){
    warning("sigma is numerically not positive definite");
  }
  smat=diagmat(sqrt(s));
  retval= U*smat*U.t();

  out=rcnormRcpp(n*ncolsig);
  out.reshape(ncolsig,n);
  out=conj(out.st()*retval);
  out.each_row() += mean.st();

  return out;
}

// [[Rcpp::export]]
arma::vec Qpval(arma::cx_cube fhat_pmt, int K,int ndraw, arma::vec Qts, double Qint, bool blockdiag){

  //initialization
  int R=sqrt(fhat_pmt.n_cols);
  int B=fhat_pmt.n_slices;

  std::complex<double> two(2,0);
  arma::cx_mat gpout(ndraw,B*pow(R,2));
  arma::mat Qtsout(ndraw,pow(R,2),fill::zeros);
  arma::vec Qintout(ndraw,fill::zeros);
  arma::mat Kinv(ndraw,pow(R,2));
  Kinv.fill(pow(K,-1));
  arma::mat R2inv(ndraw,pow(R,2));
  R2inv.fill(1/pow(R,2));
  arma::mat tmp(ndraw,pow(R,2));


  if(blockdiag){//block diagonal covariance matrix approximation


    arma::cx_mat twomat(pow(R,2),pow(R,2));
    twomat.fill(two);

    for(int b=0;b<B;b++){
      int ridx=0;
      int cidx=0;
      arma::cx_mat covGnullmat(pow(R,2),pow(R,2),fill::zeros);

      for(int sig2=0;sig2<R;sig2++){
        for(int tau2=0;tau2<R;tau2++){
          for(int sig1=0;sig1<R;sig1++){
            for(int tau1=0;tau1<R;tau1++){
              covGnullmat(ridx,cidx)=covGnull(fhat_pmt,b,b,tau1,sig1,tau2,sig2);
              ridx++;if(ridx==pow(R,2)){ridx=0;cidx++;}
            }
          }
        }
      }

      //force covariance matrix to be hermitian positive definite
      covGnullmat=(covGnullmat+covGnullmat.t())/twomat;

      //draws from Gaussian process with mean zero and covariance matrix
      arma::cx_mat gpm(pow(R,2),1,fill::zeros);
      gpout.cols(b*pow(R,2),(b+1)*pow(R,2)-1)=rcmvnormRcpp(ndraw, gpm, covGnullmat);

      //compute Qts and Qint for each draw
      tmp=real(gpout.cols(b*pow(R,2),(b+1)*pow(R,2)-1)%conj(gpout.cols(b*pow(R,2),(b+1)*pow(R,2)-1)));
      Qtsout=Qtsout+Kinv%tmp;
      Qintout=Qintout+sum(Kinv%tmp%R2inv,1);
    }

  }else{//full covariance matrix

    int ridx=0;
    int cidx=0;

    arma::cx_mat twomat(B*pow(R,2),B*pow(R,2));
    twomat.fill(two);
    arma::cx_mat covGnullmat(B*pow(R,2),B*pow(R,2),fill::zeros);

    //estimate covariance matrix
    for(int b2=0;b2<B;b2++){
      for(int sig2=0;sig2<R;sig2++){
        for(int tau2=0;tau2<R;tau2++){
          for(int b1=0;b1<B;b1++){
            for(int sig1=0;sig1<R;sig1++){
              for(int tau1=0;tau1<R;tau1++){
                covGnullmat(ridx,cidx)=covGnull(fhat_pmt,b1,b2,tau1,sig1,tau2,sig2);
                ridx++;if(ridx==B*pow(R,2)){ridx=0;cidx++;}
              }
            }
          }
        }
      }
    }

    //force covariance matrix to be hermitian positive definite
    covGnullmat=(covGnullmat+covGnullmat.t())/twomat;

    //draws from Gaussian process with mean zero and covariance matrix
    arma::cx_mat gpm(B*pow(R,2),1,fill::zeros);
    gpout=rcmvnormRcpp(ndraw, gpm, covGnullmat);

    //compute Qts and Qint for each draw
    for (int i=0;i<B;i++){
      tmp=real(gpout.cols(i*pow(R,2),(i+1)*pow(R,2)-1)%conj(gpout.cols(i*pow(R,2),(i+1)*pow(R,2)-1)));
      Qtsout=Qtsout+Kinv%tmp;
      Qintout=Qintout+sum(Kinv%tmp%R2inv,1);
    }
  }

  //compute pvalues
  arma::vec Qtsp(pow(R,2),fill::zeros);
  for (int i=0;i<pow(R,2);i++){
    for (int j=0;j<ndraw;j++){
      Qtsp(i)=Qtsp(i)+((Qtsout(j,i)>Qts(i))*pow(ndraw,-1));
    }
  }

  arma::uvec tmp2=find(Qintout>Qint);
  double Qintp=tmp2.n_elem*pow(ndraw,-1);

  //combine pvalues into one vector (last element for Qint)
  arma::vec Qp(pow(R,2)+1);
  Qp.subvec(0,pow(R,2)-1)=Qtsp;
  Qp(pow(R,2))=Qintp;

  return Qp;
}

arma::vec hstepup(arma::vec pval,double alpha){

  //sort pvalues
  arma::vec pvalsort=sort(pval);
  int n=pval.n_elem;

  //step up procedure
  arma::vec th=regspace(n,1);
  arma::vec tmp(n);
  tmp.fill(alpha);
  th=tmp/th;
  arma::uvec rej=find(pvalsort<th,1,"last");


  //return index of smallest frequency in rejected set
  arma::uvec idx;
  double sig;
  if (!rej.is_empty()){
    th=th(rej); //largest threshold rejected
    arma::vec rejset=pvalsort.subvec(0,rej(0)); //rejected p-values
    arma::uvec ip;
    arma::uvec ir;
    arma::vec rejset2;
    intersect(rejset2,ip,ir,pval,rejset);
    idx=min(ip);
    sig=1;
  } else{

    th=th(0);  //return threshold for smallest p-value
    idx=find(pval==min(pval),1,"first"); //if empty, return index for smallest p-value
    sig=0;
  }

  //return index of smallest frequency in rejected set and threshold
  arma::vec out(4);
  out(0)=conv_to<double>::from(idx);
  out(1)=conv_to<double>::from(pval(idx));
  out(2)=conv_to<double>::from(th);
  out(3)=sig;

  return out;
}

// [[Rcpp::export]]
Rcpp::List fEBA(arma::cx_cube fhat_pmt, arma::cx_cube ghat,
                int K, int ndraw, double alpha, bool blockdiag){

  int Fs=fhat_pmt.n_rows;
  int Rsq=fhat_pmt.n_cols;
  int B=fhat_pmt.n_slices;

  //compute Q test statistics
  arma::mat Qts=Qtsfn(ghat);
  arma::vec Qint=Qintfn(Qts);

  //pvalues for each frequency
  arma::mat Qpv(Fs-1,Rsq+1,fill::zeros);
  for (int i=1;i<Fs;i++){
    Rcout <<  "Computing p-value for frequency " << i << " of " << Fs-1 <<"\r";
    Qpv.row(i-1)=trans(Qpval(fhat_pmt.subcube(0,0,0,i,Rsq-1,B-1),K,ndraw,trans(Qts.row(i-1)),Qint(i-1),blockdiag));
  }

  //Hochberg test across frequencies
  arma::mat Qhb(4,Rsq+1);
  for (int i=0;i<Rsq+1;i++){
    Qhb.col(i)=hstepup(Qpv.col(i),alpha);
  }

  //compile results and output
  Rcpp::List out=List::create(Qts,Qint,Qpv,Qhb);

  return out;
}

