# EBA tests for error handling
test_that("errors if X is not a numeric Vector", {
  T = 50000
  X = c("a", "b", "c")
  N = 500
  K = 15
  alpha = .05
  std = FALSE
  expect_error(eba.search(X=X,N=N,K=K,std=std,alpha=alpha))
})

test_that("errors if X has no missing or non-finite values", {
  T = 50000
  X <- eba.simdata(T=T);
  X$wn[2] = NA
  N = 500
  K = 15
  alpha = .05
  std = FALSE
  expect_error(eba.search(X=X$wn,N=N,K=K,std=std,alpha=alpha))
})

test_that("errors if N is missing", {
  T = 50000
  X <- eba.simdata(T=T);
  N = NA
  K = 15
  alpha = .05
  std = FALSE
  expect_error(eba.search(X=X$wn,N=N,K=K,std=std,alpha=alpha))
})

test_that("errors if N < 0", {
  T = 50000
  X <- eba.simdata(T=T);
  N = -5000
  K = 15
  alpha = .05
  std = FALSE
  expect_error(eba.search(X=X$wn,N=N,K=K,std=std,alpha=alpha))
})

test_that("errors if N is greater than the length of X", {
  T = 50000
  X <- eba.simdata(T=T);
  N = 500000
  K = 15
  alpha = .05
  std = FALSE
  expect_error(eba.search(X=X$wn,N=N,K=K,std=std,alpha=alpha))
})

test_that("error if N is less than 30", {
  T = 50000
  X <- eba.simdata(T=T);
  N = 25
  K = 15
  alpha = .05
  std = FALSE
  expect_error(eba.search(X=X$wn,N=N,K=K,std=std,alpha=alpha))
})

test_that("error if K is non-finite or missing", {
  T = 50000
  X <- eba.simdata(T=T);
  N = 500
  K = NA
  alpha = .05
  std = FALSE
  expect_error(eba.search(X=X$wn,N=N,K=K,std=std,alpha=alpha))
})

test_that("error if K is too large, greater than floor(2*N*.15-1)", {
  T = 50000
  X <- eba.simdata(T=T);
  N = 500
  K = 500
  alpha = .05
  std = FALSE
  expect_error(eba.search(X=X$wn,N=N,K=K,std=std,alpha=alpha))
})

# eba.search tests for FRESH statistic calculations

test_that("error in FRESH statistic calculations linear", {
  set.seed(823819) #if you change the seed, you will get different results
  T <- 50000
  X <- eba.simdata(T=T)
  N <- 500
  K <- 15
  alpha <- 0.05
  std <- FALSE
  ebaout.bl <- eba.search(X=X$bL,N=N,K=K,std=std,alpha=alpha)
  expect_equal(ebaout.bl$part.final, c(.000, .150, .344, .500))
})

test_that("error in FRESH statistic calculations white noise", {
  set.seed(823819) #if you change the seed, you will get different results
  T <- 50000
  X <- eba.simdata(T=T)
  N <- 500
  K <- 15
  alpha <- 0.05
  std <- FALSE
  ebaout.wn <- eba.search(X=X$wn,N=N,K=K,std=std,alpha=alpha)
  expect_equal(ebaout.wn$part.final, c(.000, .500))
})

test_that("error in FRESH statistic calculations Sinusoidal", {
  set.seed(823819) #if you change the seed, you will get different results
  T <- 50000
  X <- eba.simdata(T=T)
  N <- 500
  K <- 15
  alpha <- 0.05
  std <- FALSE
  ebaout.bs <- eba.search(X=X$bS,N=N,K=K,std=std,alpha=alpha)
  expect_equal(ebaout.bs$part.final, c(.000, 0.144, 0.338, 0.500))
})




