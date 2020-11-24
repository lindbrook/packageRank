#' Scrape CRAN Mirrors data.
#'
#' https://cran.r-project.org/mirrors.html
#' @param mirror.description Logical. Mirror details.
#' @export

cranMirrors <- function(mirror.description = FALSE) {
  mirrors.url <- "https://cran.r-project.org/mirrors.html"
  web_page <- readLines(mirrors.url)

  hosts.id <- grep("<dt>", web_page)
  hosts <- web_page[hosts.id]
  hosts <- unname(vapply(hosts, function(x) {
    gsub("<.*?>", "", x)
  }, character(1L)))

  country.code <- c(NA, "dz", "ar", "au", "at", "be", "br", "bg", "ca", "cl",
    "cn", "co", "cr", "cz", "dk", "asia", "ec", "sv", "ee", "fr", "de", "gr",
    "hu", "is", "in", "id", "ir", "ie", "it", "jp", "kr", "my", "mx", "ma",
    "nl", "nz", "no", "ph", "pt", "ru", "za", "es", "se", "ch", "tw", "th",
    "tr", "uk", "us", "uy")

  out <- lapply(seq_along(hosts)[-length(hosts)], function(i) {
    h1 <- hosts.id[i]
    h2 <- hosts.id[i + 1] - 1
    host.tmp <- web_page[h1:h2]
    data.tmp <- host.tmp[grep("href", host.tmp)]

    urls <- unname(vapply(data.tmp, function(x) {
      gsub("<.*?>", "", x)
    }, character(1L)))

    mirror <- gsub("<.*?>", "", web_page[h1])
    desc <- host.tmp[grep("<td>", host.tmp) + 1]

    data.frame(country = hosts[i],
               url = urls,
               country.code = country.code[i],
               description = desc)
  })

  out <- do.call(rbind,out)
  if (mirror.description) out
  else out[, names(out) != "description"]
}
