#' Package download counts by country (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param sort Logical.
#' @param memoization Logical. Use memoization when downloading logs.
#' @export

packageCountry <- function(packages = "HistData", date = Sys.Date() - 1,
  memoization = TRUE, sort = FALSE) {
  dat <- packageLog(packages = packages, date = date, memoization = memoization)
  if (sort) sort(table(dat$country), decreasing = TRUE)
  else table(dat$country)
}

#' Plot download counts by country (prototype).
#'
#' From RStudio's CRAN Mirror http://cran-logs.rstudio.com/
#' @param packages Character. Vector of package name(s).
#' @param date Character. Date.
#' @param memoization Logical. Use memoization when downloading logs.
#' @param type Character. "map"
#' @export

packageCountryPlot <- function(packages = "HistData", date = Sys.Date() - 1,
  memoization = TRUE, type = "map") {

  download.countries <- packageCountry(packages = packages, date = date,
    memoization = memoization)

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # todo: .eu
  tld.test <- vapply(names(download.countries), function(nm) {
    nm %in% world$iso_a2
  }, logical(1L))

  download.countries <- download.countries[tld.test]
  world$Downloads <- NA

  for(nm in names(download.countries)) {
    world[world$iso_a2 %in% nm, "Downloads"] <- download.countries[nm]
  }

  # world[world$iso_a2 %in% names(download.countries), ]
  # Error in is.finite(x) : default method not implemented for type 'list'

  ggplot(data = world) +
    geom_sf(aes(fill = Downloads)) +
    scale_fill_viridis_c(option = "plasma", na.value = "white") +
    theme_bw() +
    ggtitle(paste(packages, "@", date)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5))
}
