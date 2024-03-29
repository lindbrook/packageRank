Logs
================
lindbrook
2022-04-24

## Filename/URL 2012

In the initial set of logs, there are three problems with
filenames/URLs.

First, three logs are duplicated: “2012-10-07”, “2012-10-08”, and
“2012-10-11”. You can find logs for those days at the following
filenames/URLs:

    Log              Filename/URL
    2012-10-07 ----- 2012-10-07
    2012-10-07 ----- 2012-10-11

    2012-10-08 ----- 2012-10-08
    2012-10-08 ----- 2012-10-13

    2012-10-11 ----- 2012-10-12
    2012-10-11 ----- 2012-10-15

    (*) the log for "2012-10-11" is found in the file/URL "2012-10-12" and
    "2012-10-15" but not in "2012-10-11".

Second, one log is simply mis-labelled:

    Log              Filename/URL
    2012-10-12 ----- 2012-10-14

Third, the filenames/URLs for logs from October 13 through December 31
2012 are offset by +3 days. For example, the log for “2012-11-28” is
found in the file/URL “2012-12-01”.

## ‘cranlogs’ and duplicate logs

Functions that rely on ‘cranlogs’ will be susceptible to duplicate logs
(same log, different name). For example, because the log for
“2012-10-07” exists in two files “2012-10-07” and “2012-10-11” and
because ‘cranlogs’ doesn’t seem to use the file/URLs, the log for
“2012-10-07” is present twice and gets double counted. Above, I found 3
instances of the same log with different names.

I include `fixDate_2012()` here because it is not exported to NAMESPACE.

``` r
fixDate_2012 <- function(date = "2012-12-31") {
  if (class(date) != "Date") ymd <- as.Date(date)
  else ymd <- date
  if (format(ymd, "%Y") == "2012") {
    if (ymd %in% as.Date(c("2012-12-29", "2012-12-30", "2012-12-31"))) {
      stop("Log for ", ymd, " is missing/unavailable.", call. = FALSE)
    } else if (ymd >= as.Date("2012-10-13") & ymd <= as.Date("2012-12-28")) {
      ymd <- ymd + 3
    } else if (ymd %in% as.Date(c("2012-10-11", "2012-10-12"))) {
      # Nominal Actual
      # 11 ----- 07
      # 12 ----- 11
      # 13 ----- 08
      # 14 ----- 12
      # 15 ----- 11
      if (identical(ymd, as.Date("2012-10-11"))) {
        ymd <- as.Date("2012-10-12")
      } else if (identical(ymd, as.Date("2012-10-12"))) {
        ymd <- as.Date("2012-10-14")
      }
    }
  }
  ymd
}
```

``` r
start.date <- "2012-10-01"
end.date <- "2013-01-05"

cranlogs.data <- cranlogs::cran_downloads(from = start.date, to = end.date)

d <- seq(from = as.Date(start.date), to = as.Date(end.date), by = "day")

# fetchCranLog() retrieves RStudio download logs
# fixDate_2012() fixes filename/URLs so you get the log for the date you want

# The expression below retrieves the logs directly from RStudio and counts the
# number of rows in the log (i.e., number of downloads), excluding those rows
# where the package is NA.

packageRank.data <- vapply(d, function(x) {
  tmp <- try(fetchCranLog(fixDate_2012(x), TRUE), silent = TRUE)
  if (any(class(tmp) == "try-error")) 0L
  else nrow(tmp[!is.na(tmp$package), ])
}, integer(1L))

packageRank.data <- data.frame(date = d, count = packageRank.data)

# Merge the two data frames by calendar date:
cran.data <- merge(cranlogs.data, packageRank.data, by = "date")
names(cran.data)[-1] <- c("cranlogs", "packageRank")

errors <- cran.data[cran.data$cranlogs != cran.data$packageRank, ]

# Compute the ratio of counts of 'cranlogs' to 'packageRank'
errors$ratio <- errors$cranlogs / errors$packageRank

errors
```

    ##          date cranlogs packageRank    ratio
    ## 6  2012-10-06    13630        6815 2.000000
    ## 7  2012-10-07       50          25 2.000000
    ## 8  2012-10-08      170          85 2.000000
    ## 11 2012-10-11      388         194 2.000000
    ## 87 2012-12-26    80738       26910 3.000297
    ## 88 2012-12-27    49007       24501 2.000204
    ## 89 2012-12-28    21959       10979 2.000091
    ## 93 2013-01-01    21822       10911 2.000000
