\name{getData}
\alias{getData}

\title{Get MCAPS data objecdts}
\description{
  Retrieve MCAPS data objects from remote database.
}
\usage{
getData(name = NULL)
}

\arguments{
  \item{name}{character, name of data object to retrieve}
}
\details{
  If \code{name} is \code{NULL} for \code{getData}, then a character
  vector is returned (invisibly) containing the names of all the available data
  objects.  Current data objects are
  \describe{
    \item{APWdata}{a list of air pollution and weather data for all 204
      U.S. counties.}
    \item{siteList}{character vector containing 5-digit FIPS codes for all
      counties.}
    \item{estimates.subset}{subset of county-specific log-relative risk
      estimates from MCAPS study.}
    \item{estimates.full}{full set of county-specific log-relative risk
      estimates from MCAPS study.}
  }
}

\author{Roger D. Peng \email{rpeng@jhsph.edu}}

\references{
  Dominici F, Peng RD, Bell ML, Pham L, McDermott A, Zeger SL, Samet JM
  (2006). "Fine Particulate Air Pollution and Hospital Admission for
  Cardiovascular and Respiratory Diseases," Journal of the American
  Medical Association, 295 (10) 1127--1134.  
}

\examples{
\dontrun{
initMCAPS()
getData()
siteList <- getData("sitesList")
head(siteList)
}
}
\keyword{datasets}
