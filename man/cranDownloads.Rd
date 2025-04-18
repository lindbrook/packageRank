% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cranDownloads.R
\name{cranDownloads}
\alias{cranDownloads}
\title{Daily package downloads from the RStudio CRAN mirror.}
\usage{
cranDownloads(packages = NULL, when = NULL, from = NULL, to = NULL,
  check.package = TRUE, dev.mode = FALSE, fix.cranlogs = TRUE,
  pro.mode = FALSE)
}
\arguments{
\item{packages}{A character vector, the packages to query,
or \code{NULL} for a sum of downloads for all packages.
Alternatively, it can also be \code{"R"}, to query downloads
of R itself. \code{"R"} cannot be mixed with packages.}

\item{when}{\code{last-day}, \code{last-week} or \code{last-month}.
If this is given, then \code{from} and \code{to} are ignored.}

\item{from}{Start date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.}

\item{to}{End date as \code{yyyy-mm-dd}, \code{yyyy-mm} or \code{yyyy}.}

\item{check.package}{Logical. Validate and "spell check" package.}

\item{dev.mode}{Logical. Use validatePackage0() to scrape CRAN.}

\item{fix.cranlogs}{Logical. Use RStudio logs to fix 8 dates with duplicated data in 'cranlogs' results.}

\item{pro.mode}{Logical. Faster but fewer checks/features. Closer to cranlogs::cran_downloads() but with cranDownloads()'s plot method.}
}
\description{
Enhanced implementation of cranlogs::cran_downloads().
}
\examples{
\dontrun{
cranDownloads(packages = "HistData")
cranDownloads(packages = "HistData", when = "last-week")
cranDownloads(packages = "HistData", when = "last-month")

# January 7 - 31, 2019
cranDownloads(packages = "HistData", from = "2019-01-07", to = "2019-01-31")

# February through March 2019
cranDownloads(packages = "HistData", from = "2019-02", to = "2019-03")

# 2024 year-to-date
cranDownloads(packages = "HistData", from = 2024)
}
}
