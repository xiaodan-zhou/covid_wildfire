\name{tlnise}
\alias{tlnise}
\alias{initTLNise}

\concept{Hierarchical Normal model}

\title{TLNise}
\description{
  Two level Normal independent sampling estimation
}
\usage{
tlnise(Y, V, w = NA, V0 = NA, prior = NA, N = 1000, seed = NULL,
       Tol = 1e-06, maxiter = 1000, intercept = TRUE, labelY = NA,
       labelYj = NA, labelw = NA, digits = 4, brief = 1, prnt = TRUE)
}

\arguments{
  \item{Y}{Jxp (or pxJ) matrix of p-dimensional Normal outcomes}
  \item{V}{pxpxJ array of pxp Level-1 covariances (assumed known)}
  \item{w}{Jxq (or qxJ) covariate matrix (adds column of 1's if not included and \code{intercept = TRUE})}
  \item{V0}{"typical" Vj (default is average of Vj's)}
  \item{prior}{prior parameter (see Details)}
  \item{N}{number of Constrained Wishart draws for inference}
  \item{seed}{seed for the random number generator}
  \item{Tol}{tolerance for determining modal convergence}
  \item{maxiter}{maximum number of EM iterations for finding mode}
  \item{intercept}{if \code{TRUE}, an intercept term is included in the regression}
  \item{labelY}{optional names vector for the J observations}
  \item{labelYj}{optional names vector for the p elements of Yj}
  \item{labelw}{optional names vector for covariates}
  \item{digits}{number of significant digits for reporting results}
  \item{brief}{level of output, from 0 (minimum) to 2 (maximum)}
  \item{prnt}{controls printing during execution}
}
\details{
  The prior is \eqn{p(B0) = |B0|^{(prior-p-1)/2}}{p(B_0) =
    |B_0|^{(prior - p - 1)/2}}.  
  
  Note that for the prior distribution, \code{prior = -(p+1)}
  corresponds to a uniform on level-2 covariance matrix A (default),
  \code{prior = 0} is the Jeffreys' prior, and \code{prior = (p+1)}
  is the uniform prior on shrinkage matrix B0.
  
}
\value{
  \code{tlnise} returns a list, the precise contents of which depends on
  the value of the \code{brief} argument.  Setting \code{brief = 2}
  returns the maximum amount of information.  Setting \code{brief = 1}
  or \code{brief = 0} returns a subset of that information.

  If \code{brief = 2}, the a list with the following components is returned:
  \item{gamma}{matrix of posterior mean and SD estimates of Gamma, and
    thei ratios}
  \item{theta}{pxJ matrix of posterior mean estimates for thetaj's}
  \item{SDtheta}{pxJ matrix of posterior SD estimates for thetaj's}
  \item{A}{pxp estimated posterior mean of variance matrix A}
  \item{rtA}{p-vector of between group SD estimates}
  \item{Dgamma}{rxr estimated posterior covariance matrix for Gamma}
  \item{Vtheta}{pxpxJ array of estimated covariances for thetaj's}
  \item{B0}{pxpxN array of simulated B0 values}
  \item{lr}{N-vector of log density ratios for each B0 value}
  \item{lf}{N-vector of log f(B0|Y) evaluated at each B0}
  \item{lf0}{N-vector of log f0(B0|Y) evaluated at each B0 (f0 is the CWish envelope density for f)}
  \item{df}{degrees of freedom for f0}
  \item{Sigma}{scale matrix for f0}
  \item{nvec}{number of matrices begun, diagonal and off-diagonal elements simulated to get N CWish matrices}
  \item{nrej}{number of rejections that occurred at each step 1,..,p}
}
\references{
  Everson PJ, Morris CN (2000). \dQuote{Inference for Multivariate Normal
  Hierarchical Models,} Journal of the Royal Statistical Society, Series
  B, 62 (6) 399--412.
}

\author{S-PLUS original by Philip Everson; R port by Roger D. Peng}

\examples{
x <- rnorm(10)  ## Second level
y <- rnorm(10, x)  ## First level means

out <- tlnise(Y = y, V = rep(1, 10), w = rep(1, 10), seed = 1234)
}
\keyword{models}% at least one, from doc/KEYWORDS
