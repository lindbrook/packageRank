#' Blog post data.
#'
#' \describe{
#'    \item{\code{archive.pkg_ver}}{}
#'    \item{\code{archive.pkg_ver.filtered}}{}
#'    \item{\code{cran.pkg_ver}}{}
#'    \item{\code{cran.pkg_ver.filtered}}{}
#'    \item{\code{dl.ct}}{}
#'    \item{\code{dl.ct2}}{}
#'    \item{\code{pkg.ct}}{}
#'    \item{\code{pkg.ct2}}{}
#'    \item{\code{oct.data}}{}
#'    \item{\code{cholera.data}}{}
#'    \item{\code{ggplot2.data}}{}
#'    \item{\code{VR.data}}{}
#'    \item{\code{smpl}}{}
#'    \item{\code{smpl.histories}}{}
#'    \item{\code{smpl.archive}}{}
#'    \item{\code{smpl.archive.histories}}{}
#'    \item{\code{ccode.ct}}{}
#'    \item{\code{crosstab_2019_10_01}}{}
#'    \item{\code{percentiles}}{}
#'    \item{\code{top.n.oct2019}}{}
#'    \item{\code{top.n.jul2020}}{}
#'    \item{\code{download.country}}{}
#'    \item{\code{october.downloads}}{}
#'    \item{\code{july.downloads}}{}
#'    \item{\code{cran.pkgs.oct}}{}
#'    \item{\code{arch.pkgs.oct}}{}
#'    \item{\code{cran.pkgs.jul}}{}
#'    \item{\code{arch.pkgs.jul}}{}
#'    \item{\code{pkg.history}}{}
#' }
#' @format A list with 29 elements.
#' @docType data
"blog.data"

# blog.data <- list(archive.pkg_ver = archive.pkg_ver,
#                   archive.pkg_ver.filtered = archive.pkg_ver.filtered,
#                   cran.pkg_ver = cran.pkg_ver,
#                   cran.pkg_ver.filtered = cran.pkg_ver.filtered,
#                   dl.ct = dl.ct,
#                   dl.ct2 = dl.ct2,
#                   pkg.ct = pkg.ct,
#                   pkg.ct2 = pkg.ct2)

# blog.data$oct.data <- oct.data
# blog.data$cholera.data <- cholera.data
# blog.data$ggplot2.data <- ggplot2.data
# blog.data$VR.data <- VR.data
# blog.data$smpl <- smpl
# blog.data$smpl.histories <- smpl.histories
# blog.data$smpl.archive <- smpl.archive
# blog.data$smpl.archive.histories <- smpl.archive.histories
# blog.data$ccode.ct <- ccode.ct
# blog.data$crosstab_2019_10_01 <- crosstab_2019_10_01
# blog.data$percentiles <- percentiles
# blog.data$top.n.oct2019 <- top.n.oct2019
# blog.data$top.n.jul2020 <- top.n.jul2020
# blog.data$download.country <- download.country
# blog.data$october.downloads <- downloads
# blog.data$july.downloads <- downloads
# blog.data$cran.pkgs.oct <- cran.pkgs
# blog.data$arch.pkgs.oct <- arch.pkgs
# blog.data$cran.pkgs.jul <- cran.pkgs
# blog.data$arch.pkgs.jul <- arch.pkgs
# blog.data$pkg.history <- pkg.history

# usethis::use_data(blog.data, overwrite = TRUE, version = 3L)

#' Eight RStudio Download Logs to Fix Duplicate Logs Errors in 'cranlogs'.
#'
#' October 6-8, 2012; October 11, 2012; December 26-28; and January 1, 20113.
#'
#' @format
#' \describe{
#'    \item{\code{date}}{}
#'    \item{\code{time}}{}
#'    \item{\code{size}}{}
#'    \item{\code{r_version}}{}
#'    \item{\code{r_arch}}{}
#'    \item{\code{r_os}}{}
#'    \item{\code{package}}{}
#'    \item{\code{version}}{}
#'    \item{\code{country}}{}
#'    \item{\code{ip_id}}{}
#' }
#' @docType data
"rstudio.logs"

#' Missing/NA Posit/RStudio logs.
#'
#' August 25-26, 29-31 2025; September 1-2, 2025. 7 days.
#'
#' @docType data
"missing.dates"
