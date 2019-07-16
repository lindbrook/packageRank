
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/packageRank)](https://cran.r-project.org/package=packageRank)
[![GitHub\_Status\_Badge](https://img.shields.io/badge/GitHub-0.2.0.9001-red.svg)](https://github.com/lindbrook/packageRank/blob/master/NEWS)
## packageRank: compute and visualize package download counts and percentiles

### features

  - compute the rank percentile and nominal rank of a package’s
    downloads from RStudio’s [CRAN
    mirror](http://cran-logs.rstudio.com).
  - visualize a package’s relative and absolute position in the
    distribution of package downloads for a day (cross-sectionally) or
    over time (longitudinally).
  - provide S3 plot methods for ‘cranlogs’ output.

NOTE: ‘packageRank’ relies on an active internet connection.

### getting started

To install ‘packageRank’ from CRAN:

``` r
install.packages("packageRank")
```

To install the latest development version from
GitHub:

``` r
# You may need to first install the 'devtools' via install.packages("devtools").

devtools::install_github("lindbrook/packageRank", build_vignettes = TRUE)
```

### background

The ‘[cranlogs](https://cran.r-project.org/package=cranlogs)’ package
computes the raw number of downloads using RStudio’s [CRAN
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

In both cases, lurking in the background is the “compared to what?”
question. Is 51 downloads large or small? Is the pattern that week
typical or unusual? To answer these questions, ‘packageRank’ puts
package download counts into greater context.

### compute percentiles and ranks

To do so, the package can compute the rank percentile and nominal rank
of a package’s downloads:

``` r
packageRank(packages = "HistData", date = "2019-01-01")
>         date packages downloads percentile          rank
> 1 2019-01-01 HistData        51       93.4 920 of 14,020
```

Here, we see that the 51 downloads puts ‘HistData’ in the 93rd
percentile. This statistic, familiar to anyone who’s taken a
standardized test, tell us that 93% of packages had fewer downloads than
‘HistData’: \[1\]

``` r
pkg.rank <- packageRank(packages = "HistData", date = "2019-01-01")
downloads <- pkg.rank$crosstab

round(100 * mean(downloads < downloads["HistData"]), 1)
> [1] 93.4

# OR

(pkgs.with.fewer.downloads <- sum(downloads < downloads["HistData"]))
> [1] 13092

(tot.pkgs <- length(downloads))
> [1] 14020

round(100 * pkgs.with.fewer.downloads / tot.pkgs , 1)
> [1] 93.4
```

We also see that 51 downloads puts ‘HistData’ in 920th place among the
14,020 packages with at least one download. What makes this rank
“nominal” is the fact that multiple packages can have the same number
of downloads. As a result, a package’s nominal rank (but not its rank
percentile) will sometimes be affected by its name: packages with the
same number of downloads will be sorted in alphabetical order. For the
case at hand, ‘HistData’ benefits from the fact that it is second in the
list (vector) of packages with 51 downloads:

``` r
pkg.rank <- packageRank(packages = "HistData", date = "2019-01-01")
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

### visualization (cross-sectional)

To visualize a package’s relative position on a given day’s downloads,
simply use the
following:

``` r
plot(packageRank(packages = "HistData", date = "2019-05-01"))
```

<img src="man/figures/README-plot1-1.png" style="display: block; margin: auto auto auto 0;" />

This cross-sectional view plots a package’s rank (x-axis) against the
logarithm of its downloads (y-axis) and highlights its position in the
overall distribution of downloads.

In addition, it also illustrates 1) a package’s rank percentile and its
raw count of downloads (in red); 2) the location of the 75th, 50th and
25th percentiles (dotted gray vertical lines); 3) the package with the
most downloads, in this case ‘devtools’ (in blue); and 4) the total
number of downloads (2,982,767) on that day (in blue).

Note that you can even pass a vector of
packages:

``` r
plot(packageRank(packages = c("cholera", "HistData", "regtools"), date = "2019-05-01"))
```

<img src="man/figures/README-plot2-1.png" style="display: block; margin: auto auto auto 0;" />

### visualization (longitudinal)

To visualize a package’s relative position over time, use
`packageRankTime()`:

``` r
plot(packageRankTime(packages = "HistData", when = "last-month"), graphics_pkg = "base")
```

<img src="man/figures/README-plot_ts-1.png" style="display: block; margin: auto auto auto 0;" />

This longitudinal view plots the date (x-axis) against the logarithm of
a package’s downloads (y-axis).

In the background, the same variable are plotted (in gray) for a
stratified random sample of packages.\[2\] This sample approximates the
“typical” pattern of package downloads for that time period.

As above, you can pass a vector of
packages:

``` r
plot(packageRankTime(packages = c("Rcpp", "HistData", "rlang"), when = "last-month"))
```

<img src="man/figures/README-plot_ts2-1.png" style="display: block; margin: auto auto auto 0;" />

Note that only two time frames are available: “last-week” and
“last-month”.

### visualizing ‘cranlogs’

To visualize the download counts from `cranlogs::cran_download()`,
‘packageRank’ provides a generic S3 plot() method. All you need to do
is substitute cran\_downloads2() for
cran\_download():

``` r
plot(cran_downloads2(packages = c("data.table", "Rcpp", "rlang"), from = "2019-01-01", to = "2019-01-01"))
```

<img src="man/figures/README-cranlogsB1-1.png" style="display: block; margin: auto auto auto 0;" />

``` r
plot(cran_downloads2(packages = c("data.table", "Rcpp", "rlang"), when = "last-month"))
```

<img src="man/figures/README-cranlogsB2-1.png" style="display: block; margin: auto auto auto 0;" />

``` r
plot(cran_downloads2(packages = c("data.table", "Rcpp", "rlang"), from = "2019-01-01", to = "2019-01-31"))
```

<img src="man/figures/README-cranlogsB3-1.png" style="display: block; margin: auto auto auto 0;" />

### graphics: base R and ‘ggplot2’

All plot are available as both base R and ‘ggplot2’ graphs. By default,
plot with single frame/panels (one package or one day) use base graphics
while those with multiple frames/panels use ‘ggplot2’. You can override
these defaults by using the “graphics” argument in the plot() method.

### memoization

To avoid the bottleneck of downloading multiple log files,
`packageRank()` is limited to individual days. However, to reduce the
need to re-download logs for a given day, ‘packageRank’ makes use of
memoization via the ‘memoise’ package.

Here’s relevant code:

``` r
fetchLog <- function(x) data.table::fread(x)

mfetchLog <- memoise::memoise(fetchLog)

if (RCurl::url.exists(url)) {
  cran_log <- mfetchLog(url)
}
```

If you use `fetchLog()`, the log file, which can sometimes be as large
as 50 MB, will be downloaded every time you call the function. If you
use `mfetchLog()`, logs are intelligently cached; those that have
already been downloaded, in your current R session, will not be
downloaded again.

### Notes

1.  Note that because packages with zero downloads are not recorded in
    the log, there is a censoring problem.

2.  Within each 5% interval of rank percentiles (e.g., 0 to 5, 5 to 10,
    95 to 100, etc.), a random sample of 5% of packages is selected and
    tracked over time.
