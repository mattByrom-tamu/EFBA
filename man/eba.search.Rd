% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EBA_functions.R
\name{eba.search}
\alias{eba.search}
\title{Searches for frequency partitions for univariate time series}
\usage{
eba.search(X, N, K, std, alpha)
}
\arguments{
\item{X}{Vector: univariate time series with a length > 0 and not missing or non-finite values}

\item{N}{Double: number of of observations per approximately stationary block}

\item{K}{Double: number of tapers to use in multitaper spectral estimator}

\item{std}{TRUE/FALSE Boolean: should the variance of each stationary block be standardized to one across all blocks?}

\item{alpha}{Double: significance level to use for testing partition points using FRESH statistic}
}
\value{
Results on partitions of frequency space  \cr \cr
flat: tests to see if the spectra have any time varying behavior. Low p value indicates low time varying behavior. \cr \cr
pvals: tests each partition as a possible frequency \cr \cr
final: final estimated frequency partition points \cr \cr
list: gives you a list of identified frequency bands each pass of the algorithm \cr \cr
log: for each pass of he algorithm, gives the indetified frequency, test statistic, thrshiold, pval, and significance (boolean) \cr \cr
}
\description{
Searches for frequency partitions for univariate time series
}
\details{
How to run the EBA with the FRESH statistic \cr  \cr
'X' is a vector containing a realization of the time series process you wish to analyze \cr  \cr
'N' is a number representing how many observations should be contained in each approximately stationary block for the estimation procedure of the local approximately stationary power spectrum. For example, N=1000 means that the time series is broken up into approximately stationary segments each containing 1000 observations. Note that N must be significantly smaller than the total length of the time series. \cr \cr
'K' is a number representing how many tapers to use in estimating the approximately stationary local power spectrum using multitaper spectral estimation. Note here that K must be significantly smaller than N in order to achieve reasonable frequency resolution. For example, if you wish to distinguish behavior for frequencies separated by 0.01 or larger (this is the bandwidth) and you had N=1000 observations in each approximately stationary block, then you could have at most K=9 tapers. More generally, using the sine tapers, bw=(K+1)/(N+1). \cr \cr
'std' is a binary indicator to determine if the variance in each stationary block should be standardized to unit variance (std=TRUE) or not (std=FALSE).  \cr  \cr
'alpha' is the significance level for testing each frequency partition. For example, alpha=0.05 corresponds to the 5% significance level or 95% confidence level. \cr \cr
Once you have created the inputs pass them into the 'eba.search' function. You can refer to the EBA vignette for an example using demo data. \cr \cr
}
\examples{
eba.search(X = eba.simdata(T= 50000)$wn, N = 500, K = 15, std = FALSE, alpha = .05)
}
