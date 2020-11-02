\name{initMCAPS}
\alias{initMCAPS}

\title{Initialize MCAPS database}
\description{
  Initialize MCAPS database and setup directory for local cache
}
\usage{
initMCAPS(basedir = "MCAPS")
}

\arguments{
  \item{basedir}{character, directory where local cache of database will
  be stored}
}
\details{
  The \code{initMCAPS} function must be run first before using the
  \code{MCAPS} package.    \code{initMCAPS} creates a local directory structure for storing a
  local cache of the MCAPS objects.  Internal code makes sure that the
  local copy is synchronized with the remote version of the database.

}

\references{
  Dominici F, Peng RD, Bell ML, Pham L, McDermott A, Zeger SL, Samet JM
  (2006). "Fine Particulate Air Pollution and Hospital Admission for
  Cardiovascular and Respiratory Diseases," Journal of the American
  Medical Association, 295 (10) 1127--1134.
}

\author{Roger D. Peng \email{rpeng@jhsph.edu}}
\seealso{\code{\link{getData}}}
\keyword{datasets}
