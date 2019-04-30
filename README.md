
<!-- README.md is generated from README.Rmd. Please edit that file -->

## ‘packageRank’

‘packageRank’ builds on the on the ‘cranlogs’ package
(<https://cran.r-project.org/package=cranlogs>) by putting the raw
counts of package downloads into context. Numerically, it computes a
package’s rank percentile among all downloads (e.g., the 90th percentile
or the percent of packages with fewer downloads). Visually, it locates a
package’s position in the distribution of downloads either for
individual days (cross-sectionally) or, in more limited fashion, over
time
(longitudinally).

### Computation

``` r
cranlogs::cran_downloads(packages = "HistData", from = "2019-01-01", to = "2019-01-01")
>         date count  package
> 1 2019-01-01    51 HistData
```

``` r
packageRank(package = "HistData", date = "2019-01-01")
>         date  package downloads percentile          rank
> 1 2019-01-01 HistData        51       93.4 920 of 14,020
```

### Visualization:

Using the cran\_downloads2() function, ‘packageRank’ includes an S3 plot
method for ‘cranlogs’ output. These generate both base R graphics and
‘ggplot2’ style
graphs:

``` r
plot(cran_downloads2(package = c("Rcpp", "rlang", "dplyr")), graphics_pkg = "base")
```

<img src="man/figures/README-cranlogsB-1.png" style="display: block; margin: auto auto auto 0;" />

``` r
plot(cran_downloads2(package = c("Rcpp", "rlang", "dplyr"), from = "2019-01-01", to = "2019-01-31"))
```

<img src="man/figures/README-cranlogsC-1.png" style="display: block; margin: auto auto auto 0;" />

Using the packageRank() function, we get a cross-sectional
view:

``` r
plot(packageRank(package = "HistData", date = "2019-01-01"), graphics_pkg = "base")
```

<img src="man/figures/README-plot1-1.png" style="display: block; margin: auto auto auto 0;" />

``` r
plot(packageRank(package = c("HistData", "cholera", "regtools"), date = "2019-04-01"),
  graphics_pkg = "ggplot2")
```

<img src="man/figures/README-plot2-1.png" style="display: block; margin: auto auto auto 0;" />

Using the packageRankTime() function, we get a longitudinal
(time-series)
view:

``` r
plot(packageRankTime(package = "HistData", when = "last-month"), graphics_pkg = "base")
```

<img src="man/figures/README-plot_ts-1.png" style="display: block; margin: auto auto auto 0;" />

``` r
plot(packageRankTime(package = c("Rcpp", "HistData", "rlang"), when = "last-month"))
```

<img src="man/figures/README-plot_ts2-1.png" style="display: block; margin: auto auto auto 0;" />

### Installation

To install the development version of ‘packageRank’ from GitHub:

``` r
devtools::install_github("lindbrook/packageRank")
```

### Note

‘packageRank’ relies on having an active internet connection.
# packageRank
