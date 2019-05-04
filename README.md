
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![GitHub\_Status\_Badge](https://img.shields.io/badge/GitHub-0.0.9046-red.svg)](https://github.com/lindbrook/packageRank/blob/master/NEWS)

## ‘packageRank’

The ‘[cranlogs](https://cran.r-project.org/package=cranlogs)’ package
computes the number of downloads from RStudio’s [CRAN
mirror](http://cran-logs.rstudio.com). For example, we can see that the
‘[HistData](https://cran.r-project.org/package=HistData)’ package was
downloaded 51 times on the first day of
2019:

``` r
cranlogs::cran_downloads(packages = "HistData", from = "2019-01-01", to = "2019-01-01")
>         date count  package
> 1 2019-01-01    51 HistData
```

And 787 times in the first
week:

``` r
cranlogs::cran_downloads(packages = "HistData", from = "2019-01-01", to = "2019-01-07")
>         date count  package
> 1 2019-01-01    51 HistData
> 2 2019-01-02   100 HistData
> 3 2019-01-03   137 HistData
> 4 2019-01-04   113 HistData
> 5 2019-01-05    85 HistData
> 6 2019-01-06    96 HistData
> 7 2019-01-07   205 HistData
```

In both cases, the “compared to what?” question lurks in the background.
Is 51 downloads large or small? Is the pattern during that week typical
or unusual?

To help answer these questions, ‘packageRank’ provide some perspective
on package download counts. First, it provides plot methods for
‘cranlogs’ output. Second, it computes a package’s rank percentile of
downloads: the percent of packages with fewer downloads. This is a
statistic familiar to anyone who’s taken a standardized exam. Third, it
provides two ways to visualize a package’s position in the distribution
of package downlod counts for a given day (cross-sectionally) or over
time (longitudinally).

### visualizing ‘cranlogs’

To visualize output from cranlogs::cran\_download(), ‘packageRank’
provides S3 generic plot() methods. All you need to do is to use
cran\_downloads2() in place of
cran\_download():

``` r
plot(cran_downloads2(package = c("data.table", "Rcpp", "rlang"), from = "2019-01-01",
  to = "2019-01-01"), graphics_pkg = "base")
```

<img src="man/figures/README-cranlogsB1-1.png" style="display: block; margin: auto auto auto 0;" />

``` r
plot(cran_downloads2(package = c("data.table", "Rcpp", "rlang"), when = "last-month"))
```

<img src="man/figures/README-cranlogsB2-1.png" style="display: block; margin: auto auto auto 0;" />

``` r
plot(cran_downloads2(package = c("data.table", "Rcpp", "rlang"), from = "2019-01-01",
  to = "2019-01-31"))
```

<img src="man/figures/README-cranlogsB3-1.png" style="display: block; margin: auto auto auto 0;" />

### computation of percentiles and ranks

In addition to raw download counts, `packageRank()` also computes a
package’s rank percentile (“percentile”) and nominal rank
(“rank”):

``` r
cran_downloads2(package = "HistData", from = "2019-01-01", to = "2019-01-01")
>         date count  package
> 1 2019-01-01    51 HistData

packageRank(package = "HistData", date = "2019-01-01")
>         date  package downloads percentile          rank
> 1 2019-01-01 HistData        51       93.4 920 of 14,020
```

Here, we see that those 51 downloads places ‘HistData’ in the 93rd
percentile (93% of packages had fewer downloads) and puts it in 920th
place of the 14,020 packages downloaded that day.\[1\] 920th place is
only “nominal” because it’s possible, especially when the number of
downloads is small, that multiple packages will have the identical
number of downloads. As a result, a package’s nominal rank will
sometimes be determined by alphabetical order. ‘HistData’’s 920th place
benefits from the fact that it is second in the list (vector) of
packages with 51 downloads:

``` r
pkg.rank <- packageRank(package = "HistData", date = "2019-01-01")
downloads <- pkg.rank$crosstab

downloads[downloads == 51]
> 
>  dynamicTreeCut        HistData          kimisc  NeuralNetTools 
>              51              51              51              51 
>   OpenStreetMap       pkgKitten plotlyGeoAssets            spls 
>              51              51              51              51 
>        webutils            zoom 
>              51              51
```

Currently, `packageRank()` is limited to indvidual days. The reason is
that the log for each date needs to be downloaded (the computation of
ranks is not the bottleneck). However, to reduce the need to re-download
logs for each function call, ‘packageRank’ makes use of the ‘memoise’
package.

### memoization

Here’s relevant code used to memoize the downloading of log files:

``` r
fetchLog <- function(x) data.table::fread(x)

mfetchLog <- memoise::memoise(fetchLog)

if (RCurl::url.exists(url)) {
  cran_log <- mfetchLog(url)
}
```

While `fetchLog()` always downloads the log file, which can be as large
as 50 MB, `mfetchLog()` caches the logs and only downloads logs that
have not already been downloaded in the current session.

### visualization (cross-sectional)

‘packageRank’ visualizes a package’s position in the distribution of a
given day’s
downloads.

``` r
plot(packageRank(package = "HistData", date = "2019-05-01"), graphics_pkg = "base")
```

<img src="man/figures/README-plot1-1.png" style="display: block; margin: auto auto auto 0;" />

The cross-sectional view above plots a package’s rank (x-axis) against
the logarithm of its downloads (y-axis). The plot highlights the
package’s relative position in the overall distribution. In addition,
it indicates its percentile and the number of downloads (in red); the
location of the 75th, 50th and 25th percentiles (dotted gray vertical;
lines); the package with the most downloads (in this case ‘devtools’)
and the total number of downloads (2,982,767) from the CRAN mirror on
that day (both in blue).

Just like cranlogs::cran\_downloads(), you can also pass a vector of
packages:

``` r
plot(packageRank(package = c("cholera", "HistData", "regtools"), date = "2019-05-01"))
```

<img src="man/figures/README-plot2-1.png" style="display: block; margin: auto auto auto 0;" />

### visualization (longitudinal)

‘packageRank’ visualizes a package’s position in the distribution of
downloads over time (currently only “last-week” and “last-month” is
available).

``` r
plot(packageRankTime(package = "HistData", when = "last-month"), graphics_pkg = "base")
```

<img src="man/figures/README-plot_ts-1.png" style="display: block; margin: auto auto auto 0;" />
The longitudinal view above plots the date (x-axis) against the
logarithm of a package’s downloads (y-axis). In the background, the
function plots the same data (in gray) for a cohort defined by a
stratified random sample of packages.\[2\] This cohort is way to get an
estimate of the overall temporal pattern of package downloads in the
selected time period.

As above, you can pass a vector of
packages:

``` r
plot(packageRankTime(package = c("Rcpp", "HistData", "rlang"), when = "last-month"))
```

<img src="man/figures/README-plot_ts2-1.png" style="display: block; margin: auto auto auto 0;" />

### graphics: base R and ‘ggplot2’

All plot are available as both base R graphics and ‘ggplot2’ figures via
the graphics\_pkg argument (“base” or “ggplot2”) in the plot() methods.

### Installation

To install the development version of ‘packageRank’ from GitHub:

``` r
devtools::install_github("lindbrook/packageRank")
```

‘packageRank’ relies on an active internet connection.

### Notes

1.  Note that because packages with zero downloads are not recorded in
    the log, there is a censoring problem.

2.  Within each 5% rank percentile bin (e.g., 0 to 5, 5 to 10, etc.), a
    random sample of 5% of packages is selected and then tracked over
    time as a cohort.
