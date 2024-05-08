#include <RcppArmadillo.h>
// #include <RcppEigen.h>
// #include <ParallelExecution.h>

#define _USE_MATH_DEFINES
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

//' @export
// [[Rcpp::export]]
arma::cx_cube fhat(arma::mat X, int N, int K, int Rsel, bool stdz){

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

// [[Rcpp::export]]
arma::cx_cube ghat(arma::cx_cube fhat){

  arma::cx_cube ghat=fhat;
  arma::cx_mat tmp(fhat.n_rows,fhat.n_cols);

  // demeaned multitaper estimator (g)
  tmp = mean(fhat,2);
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

std::complex<double> covghat(arma::cx_cube fhat,
                             int f1,int f2,
                             int b1, int b2,
                             int tau1,int sig1,
                             int tau2,int sig2){

  int R=sqrt(fhat.n_cols);
  int Bi=fhat.n_slices;
  std::complex<double> Bc=fhat.n_slices;
  std::complex<double> cghat;
  std::complex<double> one(1,0);
  std::complex<double> negone(-1,0);
  std::complex<double> two(2,0);

  if (b1==b2){
    cghat=(one-(two/Bc))*(fhat(f1,tau1+R*tau2,b1)*fhat(f2,sig1+R*sig2,b1)+
      fhat(f1,tau1+R*sig2,b1)*fhat(f2,tau2+R*sig1,b1))+
      (one/pow(Bc,2))*accu(fhat.subcube(f1,tau1+R*tau2,0,f1,tau1+R*tau2,Bi-1)%
      fhat.subcube(f2,sig1+R*sig2,0,f2,sig1+R*sig2,Bi-1)+
      fhat.subcube(f1,tau1+R*sig2,0,f1,tau1+R*sig2,Bi-1)%
      fhat.subcube(f2,tau2+R*sig1,0,f2,tau2+R*sig1,Bi-1));

  } else{
    cghat=(negone/Bc)*(fhat(f1,tau1+R*tau2,b1)*fhat(f2,sig1+R*sig2,b1)+
      fhat(f1,tau1+R*sig2,b1)*fhat(f2,tau2+R*sig1,b1))+
      (negone/Bc)*(fhat(f1,tau1+R*tau2,b2)*fhat(f2,sig1+R*sig2,b2)+
      fhat(f1,tau1+R*sig2,b2)*fhat(f2,tau2+R*sig1,b2))+
      (one/pow(Bc,2))*accu(fhat.subcube(f1,tau1+R*tau2,0,f1,tau1+R*tau2,Bi-1)%
      fhat.subcube(f2,sig1+R*sig2,0,f2,sig1+R*sig2,Bi-1)+
      fhat.subcube(f1,tau1+R*sig2,0,f1,tau1+R*sig2,Bi-1)%
      fhat.subcube(f2,tau2+R*sig1,0,f2,tau2+R*sig1,Bi-1));
  }

  return(cghat);
}

std::complex<double> covGnull(arma::cx_cube fhat,
                              int b1, int b2,
                              int tau1,int sig1,
                              int tau2,int sig2){

  int di=fhat.n_rows;
  std::complex<double> dc=fhat.n_rows;
  std::complex<double> one(1,0);
  std::complex<double> negone(-1,0);
  std::complex<double> covGnullout;

  arma::cx_vec sumb1b2vec(di-1);
  arma::cx_vec sumb2b1vec(di-1);
  arma::cx_mat sumf1f2mat(di-1,di-1);

  for (int i=0;i<(di-1);i++){
    sumb1b2vec(i)=covghat(fhat,di-1,i,b1,b2,tau1,sig1,tau2,sig2);
    sumb2b1vec(i)=covghat(fhat,di-1,i,b2,b1,tau2,sig2,tau1,sig1);
    for (int j=0;j<(di-1);j++){
      sumf1f2mat(i,j)=covghat(fhat,i,j,b1,b2,tau1,sig1,tau2,sig2);
    }
  }

  std::complex<double> sumb1b2=sum(sumb1b2vec);
  std::complex<double> sumb2b1=sum(sumb2b1vec);
  std::complex<double> sumf1f2=accu(sumf1f2mat);

  covGnullout=covghat(fhat,di-1,di-1,b1,b2,tau1,sig1,tau2,sig2)+
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
arma::vec Qpval(arma::cx_cube fhat, int K,int ndraw, arma::vec Qts, double Qint, bool blockdiag){

  //initialization
  int R=sqrt(fhat.n_cols);
  int B=fhat.n_slices;

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
              covGnullmat(ridx,cidx)=covGnull(fhat,b,b,tau1,sig1,tau2,sig2);
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
                covGnullmat(ridx,cidx)=covGnull(fhat,b1,b2,tau1,sig1,tau2,sig2);
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
Rcpp::List fEBA(arma::cx_cube fhat, arma::cx_cube ghat,
                int K, int ndraw, double alpha, bool blockdiag){

  int Fs=fhat.n_rows;
  int Rsq=fhat.n_cols;
  int B=fhat.n_slices;

  //compute Q test statistics
  arma::mat Qts=Qtsfn(ghat);
  arma::vec Qint=Qintfn(Qts);

  //pvalues for each frequency
  arma::mat Qpv(Fs-1,Rsq+1,fill::zeros);
  for (int i=1;i<Fs;i++){
    Rcout <<  "Computing p-value for frequency " << i << " of " << Fs-1 <<"\r";
    Qpv.row(i-1)=trans(Qpval(fhat.subcube(0,0,0,i,Rsq-1,B-1),K,ndraw,trans(Qts.row(i-1)),Qint(i-1),blockdiag));
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


/*** R
*/
