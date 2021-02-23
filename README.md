
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/packageRank)](https://cran.r-project.org/package=packageRank)
[![GitHub\_Status\_Badge](https://img.shields.io/badge/GitHub-0.3.5.9130-red.svg)](https://github.com/lindbrook/packageRank/blob/master/NEWS)
## packageRank: compute and visualize package download counts and rank percentiles

[‘packageRank’](https://CRAN.R-project.org/package=packageRank) is an R
package that helps put package download counts into context. It does so
via two functions, `cranDownloads()` and `packageRank()`, and one set of
filters that remove “invalid” entries from the download logs. I cover
these topics in three parts. A fourth part covers package related
issues.

-   [Part I Package Download
    Counts](#i---computing-package-download-counts), describes how
    `cranDownloads()` extends the functionality of
    [`cranlogs::cran_downloads()`](https://r-hub.github.io/cranlogs/) by
    adding a more user-friendly interface and by providing generic R
    `plot()` methods to makes visualization easy.
-   [Part II Package Download Rank
    Percentiles](#ii---computing-package-download-rank-percentiles),
    describes how `packageRank()` uses rank percentiles, a nonparametric
    statistic that tells you the percentage of observations (i.e.,
    packages) with fewer counts (i.e., downloads), to help you see how
    your package is doing relative to *all* other
    [CRAN](https://CRAN.R-project.org/) packages.
-   [Part III Package Download
    Filters](#iii---filtering-package-download-counts), describes how I
    use filters to reduce the presence of software and behavioral
    artifacts that would otherwise inflate package download counts.
-   [Part IV Notes](#iv---notes-and-miscellanea), describes technical
    issues that may be of interest to users. This includes the use of
    memoization, time zone issues, and internet connection time out
    problems.

[‘packageRank’](https://CRAN.R-project.org/package=packageRank) requires
an active internet connection, and relies on the
[‘cranlogs’](https://CRAN.R-project.org/package=cranlogs) package and
[RStudio’s download logs](http://cran-logs.rstudio.com/). The latter
record traffic to the [“0-Cloud” mirror](https://cloud.R-project.org),
which is “currently sponsored by RStudio” and was previously nominally
RStudio’s CRAN mirror.

Note that logs for the previous day are generally posted by 17:00 UTC.
Updated results for functions that rely on
[‘cranlogs’](https://CRAN.R-project.org/package=cranlogs) are typically
available soon thereafter.

### getting started

To install ‘packageRank’ from CRAN:

``` r
install.packages("packageRank")
```

To install the development version from GitHub:

``` r
# You may need to first install 'remotes' via install.packages("remotes").
remotes::install_github("lindbrook/packageRank", build_vignettes = TRUE)
```

### I - computing package download counts

`cranDownloads()` uses all the same arguments as
`cranlogs::cran_downloads()`:

``` r
cranlogs::cran_downloads(packages = "HistData")
```

    >         date count  package
    > 1 2020-05-01   338 HistData

The only difference is that `cranDownloads()` adds four features:

#### i) “spell check” for package names

``` r
cranDownloads(packages = "GGplot2")
```

    ## Error in cranDownloads(packages = "GGplot2") :
    ##   GGplot2: misspelled or not on CRAN.

<br/>

``` r
cranDownloads(packages = "ggplot2")
```

    >         date count cumulative package
    > 1 2020-05-01 56357      56357 ggplot2

<br/> This also works for inactive or “retired” packages in the
[Archive](https://CRAN.R-project.org/src/contrib/Archive/):

``` r
cranDownloads(packages = "vr")
```

    ## Error in cranDownloads(packages = "vr") :
    ##  vr: misspelled or not on CRAN/Archive.

<br/>

``` r
cranDownloads(packages = "VR")
```

    >         date count cumulative package
    > 1 2020-05-01    11         11      VR

<br/>

#### ii) two additional date formats

With `cranlogs::cran_downloads()`, you specify a time frame using the
`from` and `to` arguments. The downside of this is that you *must* use
the “yyyy-mm-dd” date format. For convenience’s sake, `cranDownloads()`
also allows you to use “yyyy-mm” or “yyyy” (yyyy also works).

##### “yyyy-mm”

Let’s say you want the download counts for
[‘HistData’](https://CRAN.R-project.org/package=HistData) for the month
of February 2020. With `cranlogs::cran_downloads()`, you’d have to type
out the whole date and remember that 2020 was a leap year:

``` r
cranlogs::cran_downloads(packages = "HistData", from = "2020-02-01",
  to = "2020-02-29")
```

<br/> With `cranDownloads()`, you can just specify the year and month:

``` r
cranDownloads(packages = "HistData", from = "2020-02", to = "2020-02")
```

##### “yyyy” or yyyy

Let’s say you want the year-to-date download counts for
[‘rstan’](https://CRAN.R-project.org/package=rstan). With
`cranlogs::cran_downloads()`, you’d type something like:

``` r
cranlogs::cran_downloads(packages = "rstan", from = "2021-01-01",
  to = Sys.Date() - 1)
```

<br/> With `cranDownloads()`, you can use:

``` r
cranDownloads(packages = "rstan", from = "2021")
```

or

``` r
cranDownloads(packages = "rstan", from = 2021)
```

<br/>

#### iii) check date validity

``` r
cranDownloads(packages = "HistData", from = "2019-01-15",
  to = "2019-01-35")
```

    ## Error in resolveDate(to, type = "to") : Not a valid date.

#### iv) cumulative count

``` r
cranDownloads(packages = "HistData", when = "last-week")
```

    >         date count cumulative  package
    > 1 2020-05-01   338        338 HistData
    > 2 2020-05-02   259        597 HistData
    > 3 2020-05-03   321        918 HistData
    > 4 2020-05-04   344       1262 HistData
    > 5 2020-05-05   324       1586 HistData
    > 6 2020-05-06   356       1942 HistData
    > 7 2020-05-07   324       2266 HistData

<br/>

### visualizing package download counts

`cranDownloads()` makes visualizing package downloads easy. Just use
`plot()`:

``` r
plot(cranDownloads(packages = "HistData", from = "2019", to = "2019"))
```

<img src="man/figures/README-cranDownloads_viz1-1.png" style="display: block; margin: auto;" />

If you pass a vector of package names for a single day, `plot()` will
return a dotchart:

``` r
plot(cranDownloads(packages = c("ggplot2", "data.table", "Rcpp"),
  from = "2020-03-01", to = "2020-03-01"))
```

<img src="man/figures/README-cranDownloads_viz2a-1.png" style="display: block; margin: auto;" />

If you pass a vector of package names for multiple days, `plot()` uses
`ggplot2` facets:

``` r
plot(cranDownloads(packages = c("ggplot2", "data.table", "Rcpp"),
  from = "2020", to = "2020-03-20"))
```

<img src="man/figures/README-cranDownloads_viz2-1.png" style="display: block; margin: auto;" />
<br/>

If you want to plot those data in a single frame, set
`multi.plot = TRUE`:

``` r
plot(cranDownloads(packages = c("ggplot2", "data.table", "Rcpp"),
  from = "2020", to = "2020-03-20"), multi.plot = TRUE)
```

![](man/figures/README-cranDownloads_viz3-1.png)<!-- -->

<br/> If you want plot those data in separate plots on the same scale,
use `graphics = "base"` and you’ll be prompted for each plot:

``` r
plot(cranDownloads(packages = c("ggplot2", "data.table", "Rcpp"),
  from = "2020", to = "2020-03-20"), graphics = "base")
```

If you want do the above on separate independent scales, set
`same.xy = FALSE`:

``` r
plot(cranDownloads(packages = c("ggplot2", "data.table", "Rcpp"),
  from = "2020", to = "2020-03-20"), graphics = "base", same.xy = FALSE)
```

#### `packages = NULL`

`cranlogs::cran_download(packages = NULL)` computes the total number of
package downloads from CRAN. You can plot these data by using:

``` r
plot(cranDownloads(from = 2019, to = 2019))
```

![](man/figures/README-null_packages-1.png)<!-- -->

#### `packages = "R"`

`cranlogs::cran_download(packages = "R")` computes the total number of
downloads of the R application (note that you can only use “R” or a
vector of packages names, not both!). You can plot these data by using:

``` r
plot(cranDownloads(packages = "R", from = 2019, to = 2019))
```

![](man/figures/README-r_downloads-1.png)<!-- -->

#### smoothers and confidence intervals

To add a lowess smoother to your plot, use `smooth = TRUE`:

``` r
plot(cranDownloads(packages = "rstan", from = "2019", to = "2019"),
  smooth = TRUE)
```

![](man/figures/README-lowess-1.png)<!-- -->

With graphs that use ‘ggplot2’, `se = TRUE` will add confidence
intervals:

``` r
plot(cranDownloads(packages = c("HistData", "rnaturalearth", "Zelig"),
  from = "2020", to = "2020-03-20"), smooth = TRUE, se = TRUE)
```

![](man/figures/README-ci-1.png)<!-- -->

#### package and R release dates

To annotate a graph with a package’s release dates:

``` r
plot(cranDownloads(packages = "rstan", from = "2019", to = "2019"),
  package.version = TRUE)
```

![](man/figures/README-pkg_release_date-1.png)<!-- -->

To annotate a graph with R release dates:

``` r
plot(cranDownloads(packages = "rstan", from = "2019", to = "2019"),
  r.version = TRUE)
```

![](man/figures/README-r_release_date-1.png)<!-- -->

#### plot growth curves (cumulative download counts)

To plot growth curves, set `statistic = "cumulative"`:

``` r
plot(cranDownloads(packages = c("ggplot2", "data.table", "Rcpp"),
  from = "2020", to = "2020-03-20"), statistic = "cumulative",
  multi.plot = TRUE, points = FALSE)
```

![](man/figures/README-cranDownloads_growth_curves-1.png)<!-- -->

#### population plot

To visualize a package’s downloads relative to “all” other packages over
time:

``` r
plot(cranDownloads(packages = "HistData", from = "2020", to = "2020-03-20"),
  population.plot = TRUE)
```

![](man/figures/README-pop_plot_code-1.png)<!-- -->

This longitudinal view of package downloads plots the date (x-axis)
against the logarithm of a package’s downloads (y-axis). In the
background, the same variable are plotted (in gray) using a stratified
random sample of packages: within each 5% interval of rank percentiles
(e.g., 0 to 5, 5 to 10, 95 to 100, etc.), a random sample of 5% of
packages is selected and tracked. This graphically approximates the
“typical” pattern of downloads on CRAN for the selected time period.

### II - computing package download rank percentiles

Looking at nominal download count data leads one to the “compared to
what?” question. For instance, consider the data for the first week of
March 2020:

``` r
plot(cranDownloads(packages = "cholera", from = "2020-03-01",
  to = "2020-03-07"))
```

<img src="man/figures/README-motivation_code-1.png" style="display: block; margin: auto;" />

Do Wednesday and Saturday reflect surges of interest in the package or
surges of traffic to [CRAN](https://CRAN.R-project.org/)? To put it
differently, how can we know if a given download count is typical or
unusual? One way to answer these questions is to locate your package in
the frequency distribution of download counts.

Below are the distributions of logarithm of download counts for
Wednesday and Saturday. The location of a vertical segment along the
x-axis represents a download count and the height of a segment
represents that download count’s frequency. The location of
[‘cholera’](https://CRAN.R-project.org/package=cholera) in the
distribution is highlighted in red.

``` r
plot(packageDistribution(package = "cholera", date = "2020-03-04"))
```

<img src="man/figures/README-packageDistribution_wed_code-1.png" style="display: block; margin: auto;" />

``` r
plot(packageDistribution(package = "cholera", date = "2020-03-07"))
```

<img src="man/figures/README-packageDistribution_sat_code-1.png" style="display: block; margin: auto;" />

While these plots give us a better picture of where
[‘cholera’](https://CRAN.R-project.org/package=cholera) is located,
comparisons between Wednesday and Saturday are impressionistic at best:
all we can confidently say is that the download counts for both days
were greater than the mode.

To facilitate interpretation and comparison, I use the *rank percentile*
of a download count in place of the nominal download count. This
nonparametric statistic tells you the percentage of packages with fewer
downloads. In other words, it gives you the location of your package
relative to the locations of all other packages. More importantly, by
rescaling download counts to lie on the bounded interval between 0 and
100, rank percentiles make it easier to compare packages within and
across distributions.

For example, we can compare Wednesday (“2020-03-04”) to Saturday
(“2020-03-07”):

``` r
packageRank(package = "cholera", date = "2020-03-04")
>         date packages downloads            rank percentile
> 1 2020-03-04  cholera        38 5,556 of 18,038       67.9
```

On Wednesday, we can see that
[‘cholera’](https://CRAN.R-project.org/package=cholera) had 38
downloads, came in 5,556th place out of 18,038 unique packages
downloaded, and earned a spot in the 68th percentile.

``` r
packageRank(package = "cholera", date = "2020-03-07")
>         date packages downloads            rank percentile
> 1 2020-03-07  cholera        29 3,061 of 15,950         80
```

On Saturday, we can see that
[‘cholera’](https://CRAN.R-project.org/package=cholera) had 29
downloads, came in 3,061st place out of 15,950 unique packages
downloaded, earned a spot in the 80th percentile.

So contrary to what the nominal counts tell us, one could say that the
interest in [‘cholera’](https://CRAN.R-project.org/package=cholera) was
actually greater on Saturday than on Wednesday.

#### computing rank percentile

To compute rank percentiles, I do the following. For each package, I
tabulate the number of downloads and then compute the percentage of
packages with fewer downloads. Here are the details using
[‘cholera’](https://CRAN.R-project.org/package=cholera) from Wednesday
as an example:

``` r
pkg.rank <- packageRank(packages = "cholera", date = "2020-03-04")

downloads <- pkg.rank$freqtab

round(100 * mean(downloads < downloads["cholera"]), 1)
> [1] 67.9
```

To put it differently:

``` r
(pkgs.with.fewer.downloads <- sum(downloads < downloads["cholera"]))
> [1] 12250

(tot.pkgs <- length(downloads))
> [1] 18038

round(100 * pkgs.with.fewer.downloads / tot.pkgs, 1)
> [1] 67.9
```

#### nominal ranks

In the example above, 38 downloads puts ‘cholera’ in 5,556th place among
the 18,038 packages downloaded. This rank is “nominal” because it’s
possible that multiple packages can have the same number of downloads.
As a result, a package’s nominal rank (but not its rank percentile) can
be affected by its name: packages with the same number of downloads are
sorted in alphabetical order. Thus, ‘cholera’ benefits from the fact
that it is 31st in the list of 263 packages with 38 downloads:

``` r
pkg.rank <- packageRank(packages = "cholera", date = "2020-03-04")
downloads <- pkg.rank$freqtab

which(names(downloads[downloads == 38]) == "cholera")
> [1] 31
length(downloads[downloads == 38])
> [1] 263
```

### visualizing package download rank percentiles

To visualize `packageRank()`, use `plot()`.

``` r
plot(packageRank(packages = "cholera", date = "2020-03-04"))
```

<img src="man/figures/README-packageRank_plot_code_wed-1.png" style="display: block; margin: auto;" />

<br/>

``` r
plot(packageRank(packages = "cholera", date = "2020-03-07"))
```

<img src="man/figures/README-packageRank_plot_code_sat-1.png" style="display: block; margin: auto;" />

These graphs, customized to be on the same scale, plot the *rank order*
of packages’ download counts (x-axis) against the logarithm of those
counts (y-axis). It then highlights a package’s position in the
distribution along with its rank percentile and download count (in red).
In the background, the 75th, 50th and 25th percentiles are plotted as
dotted vertical lines. The package with the most downloads,
[‘magrittr’](https://CRAN.R-project.org/package=magrittr) in both cases,
is at top left (in blue). The total number of downloads is at the top
right (in blue).

### III - filtering package download counts

We compute package downloads by counting the number of log entries for
each package. While straightforward, this approach can run into
problems. Putting aside whether the download of package dependencies
should be conisdered “valid”, I’d argue that there are two additional
types of log entries that could be viewed as “invalid”. The first are
software artifacts: entries that are smaller, often orders of magnitude
smaller, than a package’s actual binary or source file size. The second
are behavioral artifacts: entries that are a consequence of efforts to
download all of the packages on [CRAN](https://cran.r-project.org/)
rather than an effort to download your particular package. The presence
of these artifacts means that nominal package download counts are
inflated; they suffer from a strictly positive, upward bias.

An early but more detailed analysis and discussion of both inflations is
available as part of this [R-hub blog
post](https://blog.r-hub.io/2020/05/11/packagerank-intro/#inflationary-bias-of-download-counts).

#### software artifacts

When looking at package download logs, the first thing you’ll notice are
wrongly sized log entries. They come in two sizes: “small” and “medium”.
The “small” entries are in the neighborhood of 500 bytes. The “medium”
entries are variable in size. They fall anywhere between a “small” and a
full download (i.e., “small” &lt;= “medium” &lt;= full download).
“Small” entries manifest themselves as standalone entries, as part of
pair with a full download, or as part of a triplet with a “medium” and a
full download. “Medium” entries manifest themselves as standalone
entries, or as part of the aforementioned triplet.

The example below illustrates a triplet:

``` r
packageLog(date = "2020-07-01")[4:6, -(4:6)]
>               date     time    size package version country ip_id
> 3998633 2020-07-01 07:56:15   99622 cholera   0.7.0      US  4760
> 3999066 2020-07-01 07:56:15 4161948 cholera   0.7.0      US  4760
> 3999178 2020-07-01 07:56:15     536 cholera   0.7.0      US  4760
```

The observed full download is the second entry (4,161,948 bytes). The
“small” entry is the last observation (536 bytes). The “medium” entry is
the first observation (99,622 bytes). Incidentally, what makes a triplet
a triplet (or a pair a pair) is that all members have, at a minimum,
identical or adjacent time stamps.

To deal with “small” entries, I filter out entries smaller than 1,000
bytes (the smallest package appears to be
[‘source.gist’](https://cran.r-project.org/package=source.gist), which
weighs in at 1,200 bytes). “Medium” entries are harder to handle. I
remove them by either using a triplet-specific filter or a filter that
looks up a package’s actual size.

#### behavioral artifacts

While wrongly sized entries are fairly easy to see, seeing other
“invalid” entries can sometimes require a change of perspective. What I
have in mind here are downloads that are a consequence of efforts to
download *all* the packages, including *all* past version, from
[CRAN](https://cran.r-project.org/) (I think this activity excludes
mirroring activity via `rsync`). Again, see this [R-hub blog
post](https://blog.r-hub.io/2020/05/11/packagerank-intro/#inflationary-bias-of-download-counts)
for details.

Consider the example below:

``` r
packageLog(packages = "cholera", date = "2020-07-31")[8:14, -(4:6)]
>              date     time    size package version country ip_id
> 132509 2020-07-31 21:03:06 3797776 cholera   0.2.1      US    14
> 132106 2020-07-31 21:03:07 4285678 cholera   0.4.0      US    14
> 132347 2020-07-31 21:03:07 4109051 cholera   0.3.0      US    14
> 133198 2020-07-31 21:03:08 3766514 cholera   0.5.0      US    14
> 132630 2020-07-31 21:03:09 3764848 cholera   0.5.1      US    14
> 133078 2020-07-31 21:03:11 4275831 cholera   0.6.0      US    14
> 132644 2020-07-31 21:03:12 4284609 cholera   0.6.5      US    14
```

Here, we see that seven different versions of the package were
downloaded in a sequential bloc. A little digging show that these seven
versions actually represent *all* prior versions of ‘cholera’:

``` r
packageHistory(package = "cholera")
>   Package Version       Date Repository
> 1 cholera   0.2.1 2017-08-10    Archive
> 2 cholera   0.3.0 2018-01-26    Archive
> 3 cholera   0.4.0 2018-04-01    Archive
> 4 cholera   0.5.0 2018-07-16    Archive
> 5 cholera   0.5.1 2018-08-15    Archive
> 6 cholera   0.6.0 2019-03-08    Archive
> 7 cholera   0.6.5 2019-06-11    Archive
> 8 cholera   0.7.0 2019-08-28       CRAN
```

While there are legitimate reasons for downloading past versions (e.g.,
research, container-based software distribution, etc.), examples like
the above are a signature of efforts to download
[CRAN](https://cran.r-project.org/). As such, when your package is
downloaded as part of such efforts, they reflect more an interest in
[CRAN](https://cran.r-project.org/) as collection of packages than an
interest in your package *per se*. Since one of the uses of counting
package downloads is to estimate interest in your packages, it may be
useful to exclude these entries.

I try to filter these out these entries in two ways. The first works at
the level of the log itself: I identify IP addresses that download “too
many” packages and then filter out “campaigns”, large blocs of downloads
that occur in (nearly) alphabetical order. The second works at the level
of individual packages. Since some campaigns may not be associated with
“greedy” IP addresses, I also filter out sequences of past versions
downloaded in a narrowly defined time window.

#### an example

To get an idea of how inflated your package’s download count may be, use
`filteredDownloads()`. Below are the results for ‘cholera’ for 31 July
2020.

``` r
filteredDownloads(package = "cholera", date = "2020-07-31")
>         date package downloads filtered.downloads inflation
> 1 2020-07-31 cholera        14                  5       180
```

While there were 14 nominal downloads, applying all the filters reduced
the number of downloads to 5, an inflation of 180%.

Note that these filters are computationally demanding. Excluding the
time it takes to download the log file, the filters in the above example
take approximate 75 seconds to run using parallelized code (currently
only available on macOS and Unix) on a 3.1 GHz Dual-Core Intel Core i5
processor.

#### limitations

There are two sets of filters in
[‘packageRank’](https://CRAN.R-project.org/package=packageRank): the
CRAN specific, which work independently of packages, at the level of the
entire log (i.e., `ipFilter()` and `smallFilter()`) and the package
specific, rely on information about packages like the size of its source
or binary file (i.e., `tripletFilter()`, `sequenceFilter()`, and
`sizeFilter()`)

Ideally, we’d like to use both sets of filters. However, to make useful
relative comparisons (e.g., rank percentiles) we’d have to apply package
specific filters for each of the tens of thousands of packages typically
found in a log file. While feasible, this is currently very
computationally expensive. For that reason, when using and setting the
`all.filters = TRUE`, certain functions will default to only use CRAN
specific filters.

The functions that default to both CRAN and package specific functions
are `packageLog()`, `packageCountry()`, and `filteredDownloads()`. The
functions that default only to CRAN specific functions: `packageRank()`,
`ipPackage()`, `countryPackage()`, `countryDistribution()` and
`packageDistribution()`.

``` r
packageLog(package = "cholera", all.filters = TRUE)
packageRank(package = "cholera", all.filters = TRUE)
```

### IV - notes

#### country codes (top level domains)

While IP addresses are anonymized, the logs do attempt to provide the
corresponding ISO country codes or top level domains (e.g., AT, JP, US).
This covers about 85% of observations (i.e., approximately 15% country
codes are NA). Also, for what it’s worth, there seems to be a a couple
of typos for country codes: “A1” (A + number one) and “A2” (A + number
2). According to [RStudio’s
documentation](http://cran-logs.rstudio.com/), this coding was done
using MaxMind’s free database, which no longer seems to be available.

#### memoization

To avoid the bottleneck of downloading multiple log files,
`packageRank()` is currently limited to individual calendar dates. To
reduce the bottleneck of re-downloading logs, which can be upwards of 50
MB, [‘packageRank’](https://CRAN.R-project.org/package=packageRank)
makes use of memoization via the
[‘memoise’](https://CRAN.R-project.org/package=memoise) package.

Here’s relevant code:

``` r
fetchLog <- function(url) data.table::fread(url)

mfetchLog <- memoise::memoise(fetchLog)

if (RCurl::url.exists(url)) {
  cran_log <- mfetchLog(url)
}

# Note that data.table::fread() relies on R.utils::decompressFile().
```

This means that logs are intelligently cached; those that have already
been downloaded, in your current R session, will not be downloaded
again.

#### time zones

The calendar date (e.g. “2021-01-01”) is the unit of observation for
[‘packageRank’](https://CRAN.R-project.org/package=packageRank)
functions. However, because the typical use case involves the *latest*
log file, time zone differences can come into play.

Let’s say that it’s 09:01 on 01 January 2021 and you want to compute the
rank percentile for [‘ergm’](https://CRAN.R-project.org/package=ergm)
for the last day of 2020. You might be tempted to use the following:

``` r
packageRank(packages = "ergm")
```

However, depending on *where* you make this request, you may not get the
data you expect. If you’re in Honolulu, USA, you will. If you’re in
Sydney, Australia, you won’t. The reason is that you’ve somehow
forgotten a key piece of trivia: RStudio typically posts yesterday’s log
around 17:00 UTC the following day.

The expression works in Honolulu because 09:01 HST on 01 January 2021 is
19:01 UTC 01 January 2021; the log you want has been available for 2
hours. The expression fails in Sydney because 09:01 AEDT on 01 January
2021 is 31 December 2020 22:00 UTC. The log you want won’t actually be
available for another 19 hours.

To make life a little easier,
[‘packageRank’](https://CRAN.R-project.org/package=packageRank) does two
things. First, when the log for the date you want is not available (due
to time zone rather than server issues), you’ll just get the last
available log. If you specified a date in the future, you’ll either get
an error message or a warning that provides an estimate of when the log
should be available.

Using the Sydney example and the expression above, you’d get the results
for 30 December 2020:

``` r
packageRank(packages = "ergm")
```

    >         date packages downloads          rank percentile
    > 1 2020-12-30     ergm       292 873 of 20,077       95.6

If you had specified the date, you’d get an additional warning:

``` r
packageRank(packages = "ergm", date = "2021-01-01")
```

    >         date packages downloads          rank percentile
    > 1 2020-12-30     ergm       292 873 of 20,077       95.6

    Warning message:
    2020-12-31 log arrives in appox. 19 hours at 02 Jan 04:00 AEDT. Using last available!

Second, to help you check/remember when logs are posted in your
location, there’s `logPostInfo()`. When you run that function, you’ll
get the date for the latest available log along with the expected local
and UTC times when that log should be posted to RStudio’s server.

Here’s what you’d see in Honolulu:

``` r
logPostInfo()
```

    > $log.date
    > [1] "2021-02-01"
    >
    > $GMT
    > [1] "2021-02-02 17:00:00 GMT"
    >
    > $local
    > [1] "2021-02-02 07:00:00 HST"

The default uses your local time zone, via `Sys.timezone()`. To use a
specific time zone, pass the desired zone name from `OlsonNames()` to
the `tz` argument:

``` r
logPostInfo(tz = "Australia/Sydney")
```

    > $log.date
    > [1] "2021-02-01"
    >
    > $GMT
    > [1] "2021-02-02 17:00:00 GMT"
    >
    > $local
    > [1] "2021-02-03 04:00:00 AEDT"

This functionality depends on R’s ability to to compute your local time
and time zone (e.g., `Sys.time()`). My understanding is that there may
be operating system or platform specific issues that could affect this.

#### timeout

With R 4.0.3, the timeout value for internet connections became more
explicit. Here are the relevant details from that release’s [“New
features”](https://cran.r-project.org/doc/manuals/r-release/NEWS.html):

    The default value for options("timeout") can be set from environment variable
    R_DEFAULT_INTERNET_TIMEOUT, still defaulting to 60 (seconds) if that is not set
    or invalid.

This change occasionally affected functions that download logs. This was
especially true over slower internet connections and with larger log
files. To fix this, functions that use `fetchCranLog()` will, if needed,
temporarily set the timeout to 300 seconds.
