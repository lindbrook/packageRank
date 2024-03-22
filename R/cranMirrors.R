#' Scrape CRAN Mirrors data.
#'
#' https://cran.r-project.org/mirrors.html
#' @param description Logical. Mirror details.
#' @export

cranMirrors <- function(description = FALSE) {
  mirrors.url <- "https://cran.r-project.org/mirrors.html"
  web_page <- readLines(mirrors.url)
  start.line <- grep("<dt>", web_page)
  stop.line <- grep("</dd>", web_page)

  hosts <- web_page[start.line]
  hosts <- unname(vapply(hosts, function(x) {
    gsub("<.*?>", "", x)
  }, character(1L)))
  
  other.name <- c("0-Cloud", "Czech Republic",  "Iran", "Korea", "Russia", 
    "Taiwan", "Turkey", "UK", "USA", "Worldwide")
  host.tld <- c(NA, "CZ", "IR", "KR", "RU", "TW", "TR","GB", "US", NA)
  other.hosts <- data.frame(name = other.name, tld = host.tld)
  no.match <- hosts[!hosts %in% ISOcodes::ISO_3166_1$Name]
  
  tld_etc <- other.hosts[other.hosts$name %in% no.match, ]
  
  vars <- c("Name", "Alpha_2")
  tld <- ISOcodes::ISO_3166_1[ISOcodes::ISO_3166_1$Name %in% hosts, vars]

  tld <- stats::setNames(tld, c("name", "tld"))
  tld <- rbind(tld, tld_etc)
  tld <- tld[order(tld$name), ]
  row.names(tld) <- NULL

  if (any(!hosts %in% tld$name)) stop("Update mirrors!", call. = FALSE)

  out <- lapply(seq_along(hosts), function(i) {
    start <- start.line[i]
    stop <- stop.line[i]
    host.tmp <- web_page[start:stop]
    data.tmp <- host.tmp[grep("href", host.tmp)]

    url <- unname(vapply(data.tmp, function(x) {
      gsub("<.*?>", "", x)
    }, character(1L)))

    mirror <- gsub("<.*?>", "", web_page[start])
    desc <- host.tmp[grep("<td>", host.tmp) + 1]
    data.frame(country = hosts[i],
               url = url,
               country.code = tolower(tld[tld$name == mirror, "tld"]),
               description = desc)
  })

  out <- do.call(rbind,out)
  if (description) out
  else out[, names(out) != "description"]
}
