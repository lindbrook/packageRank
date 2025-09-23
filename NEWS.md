### packageRank 0.9.6.9028

- amend fix for missing dates in cranDownloads("R").


### packageRank 0.9.6.9027

- compute ylim for missingDatesPolygons() in rTotPlot() w/ 
  unit.observation = day.


### packageRank 0.9.6.9026

- fix missing dates in cranDownloads("R").


### packageRank 0.9.6.9025

- fix singlePlot() and multiPlot() w/o missing data with unit.observation = day
  & smooth = TRUE.


### packageRank 0.9.6.9024

- amend data type in fetchCranLog(): Character to Date.


### packageRank 0.9.6.9023

- add missing dates discussion to README.


### packageRank 0.9.6.9022

- fix missingDatesPolygons() in rTotPlot().
- use missingDatesPolygons(ylim, log.y = log.y).
- use graphics::polygon for RStudio R CMD check.


### packageRank 0.9.6.9021

- add missingDatesPolygons().


### packageRank 0.9.6.9020

- exclude missing.dates from addRPlotSmoother() and addRTotPlotSmoother().
- archive addSmoother(), smoothWeekData(), addSinglePlotSmoother() and 
  addMultiPlotSmoother().


### packageRank 0.9.6.9019

- exclude missing.dates from smoothers in cranDownloadsPlotFunctions.R.


### packageRank 0.9.6.9018

- move/amend fix for log(0) to plot.cranDownloads().


### packageRank 0.9.6.9017

- add/use packageRank::missing.dates.
- fix cranDownloads("R") for missing dates.


### packageRank 0.9.6.9016

- note missing Posit CRAN logs: 8/25-8/26 and  8/29-9/02 in cranDownloads().


### packageRank 0.9.6.9015

- set cachem::cache_mem(max_size = 1.5 * 1024 * 1024^2) for memoise().


### packageRank 0.9.6.9014

- use format() instead of as.POSIXlt() in fetchCranLog().


### packageRank 0.9.6.9013

- remove computation of sequenceFilter() from packageLog(all.filters = TRUE).


### packageRank 0.9.6.9012

- add/set annualPlot(to = NULL) to compute current year.


### packageRank 0.9.6.9011

- add/set logInfo(check.days = 7) and amend 'cranlogs' check.


### packageRank 0.9.6.9010

- move package log data out of filter loop in packageLog().


### packageRank 0.9.6.9009

- set ipFilter(multi.core = FALSE) in packageLog().


### packageRank 0.9.6.9008

- add/set packageLog(version.filter = FALSE).


### packageRank 0.9.6.9007

- use seq() and clean code in annualPlot().


### packageRank 0.9.6.9006

- add/set annualPlot(pro.mode = FALSE, sunday.week = TRUE).
- fix overflow weeks.


### packageRank 0.9.6.9005

- add annualPlot() prototype.


### packageRank 0.9.6.9004

- specify package function with ggplot2::.


### packageRank 0.9.6.9003

- archive annualDownloads().


### packageRank 0.9.6.9002

- clean transform_pkgsearch() in packageHistory().


### packageRank 0.9.6.9001

- fix singlePlot() for plot.cranDownloads(chatgpt).


### packageRank 0.9.6.9000

- use pkgsearch::cran_package_history()$`crandb_file_date` for 
  plot.cranDownloads(package.version).


### packageRank 0.9.6

#### Function Changes

- rename argument 'show.weekend' to 'weekend' in plot.cranDownloads().
- use fasttime::fastPOSIXct() in dateTime().
- replace as.POSIXlt() with as.POSIXct() in utc.R functions.
- make rLog() internal.
- replace "GMT" with "UTC".
- re-enable/set packageLog(multi.core = FALSE).
- set sequenceFilter(delta.time = 240).

#### Graphic/Plot Changes (base graphics)

- remove legend box and title for rPlot(unit.observation = "day").
- set legend(bg = NULL) in plot.cranDownloads() functions.
- allow package.version = "line" and r.version = "line" in plot.cranDownloads().
- add/set plot.cranDownloads(chatgpt = FALSE) for graphics = "base".
- allow chatgpt = "line" in plot.cranDownloads().
- add/set plot.cranDownloads(weekend = FALSE).

#### Fixes

- fix packageHistory() with multiple packages in singlePlot().
- compute date-time for packages before use of sequenceFilter().
- add exception in packageLog() for packages that don't survive ipFilter().
- fix rle() duplicates in removeSequences().


### packageRank 0.9.5

#### Function Changes

- set annualDownloads() internal.
- show day of week in print.cranDistribution().
- prioritize packageCRAN() over 'pkgsearch' results in packageHistory().

#### Fixes

- fix rPlot() for log.y = TRUE and graphics = "ggplot2".
- fix unmatched seq.start and seq.stop in removeSequences().
- fix plot.cranDownloads(unit.observation = "year").


### packageRank 0.9.4

#### Function Changes

- match input vector order with output data frame in packageRank() and
  queryPackage().
- add/set queryPackage(check.package = TRUE).
- move unique.packages to cranDistribution() list element and return via
  summary().
- add/set print.cranDistribution(top.n = 20).
- remove parallel implementation of packageLog().
- amend stop() message in fixDate_2012().
- add 2012 exception to rev_fixDate_2012().
- add total downloads to print.cranDistribution() & summary.cranDistribution().
- use localTime() in logInfo().

#### Plot Changes

- amend title and subtitle in plot.cranDistribution().
- amend x-axis in plot.packageDistribution().
- add day of week to plot.cranDistribution() title.

#### Fixes

- fix memoization in cranDistribution().


### packageRank 0.9.3

#### New Functionality

- add cranDistribution() with plot, print and summary methods.
- add queryCount(), queryPackage(), queryPercentile() and queryRank().
- refactor and use memoization and cranDistribution() in packageRank().

#### Function Changes

- add/set packageRank(rank.ties = TRUE).
- compute cores only for cranDistribution(ip.filter = TRUE).

#### Plot Changes

- use grDevices::devAskNewPage() with multiple packages in 
  plot.packageRank(graphics = "base").

#### Fixes

- fix transitory problem of multiple instances of a package on CRAN during a
  package update.


### packageRank 0.9.2

#### Fixes

- amend pro.mode = TRUE error message for cranDownloadsB() 'from' argument.
- amend 'cranlogs' guess-timate in logInfo().
- fix multiple smoother subtitles: addMultiPlotSmoother and addRPlotSmoother().


### packageRank 0.9.1

#### New Functionality

- add/set cranDownloads(pro.mode = FALSE).

#### Fixes/Updates

- update cranMirrors().
- fix cranDownloads(to = NULL).
- fix dayOfMonth(end.of.month = TRUE).
- fix plot(cranDownloads(pkg, when = "last-month"), unit.observation = "week", 
  smooth = TRUE).
- fix smoothWeekData() call in addSmoother()

#### Plot Changes

- use title case for axes labels.
- contextualize smoother subtitle in plot.cranDownloads(graphics = "base").

#### Documentation

- amend discussion of smoother parameters (loess as "default") in README.


### packageRank 0.9.0

#### New Functionality

- add rLog().
- add cranPackageSize(mac.ver = "arm").

#### Deprecated

- remove tripletFilter() - redundant with amended sizeFilter().

#### Fixes/Updates

- If available, multi.core = FALSE by default in all functions.

- amend/correct aggregateData(unit.observation = "week").
- fix computeFileSizeB() for multiple size units. 
- update cranMirrors().
- update/set cranPackageSize(r.ver = "4.3").
- fix argument error in packageArchive() for sizeFilter().
- fix packageDistribution() for multiple packages.
- add timeout fix to packages_on_CRAN(), packageArchive() and packageCRAN().
- fix/ensure exact package name matching in packageCRAN().
- return latest release in package_info() for packageCRAN() (n.b. pkg updates).
- fix packageCRAN(size) and packageArchive(size).
- fix packageHistory("R").
- match point types in 
  plot.cranDownloads(graphics = "ggplot2", unit.observation = "week").
- modify removeSequences() for high volume packages.
- fix typo/error in sequenceFilter().

#### Function Changes

- use previous day if today not available in cranDownloads().
- remove non-user facing functions from NAMESPACE.

- update 'ggplot2' syntax in cranDownloads() plot functions,
  ggPlot(), gg_bioc_plot(), plot.annualDownloads(), plotDownloadsCountry(), 
  plot.packageDistribution(), plotTopCountryCodes(), plot.weeklyDownloads() and 
  plot.packageVersionPercent().

- check internet connection in bioconductorDownloads() and bioconductorRank().
- change "warn.msg" to "print.message" in checkPackage().
- remove dev.mode argument from checkPackage().
- add/set plot.countryDistribution(N = 10) for top N country domains.
- match cranlogs::cran_downloads() behavior with duplicate packages in 
  cranDownloads() and packageHistory().
- add delta count and inflation unit in filteredDownloads().
- replace ip_filter() with greedyIP() in ipFilter().
- use only counts in greedyIP() in ipFilter().
- compute run length encoding and candidate.data in ipFilter(campaigns = TRUE).
- change/set ipCount(sort.count = TRUE).
- use "file.url.date" in ipCount() and ipDownloads().
- disable parallel code for ipFilter(campaigns = TRUE).
- include local time with logInfo(details = TRUE).
- check last 3 rather than last 7 logs for logInfo(details = TRUE).
- name list elements in packageHistory().
- set packageHistory(check.package = TRUE).
- use "crandb_file_date" for dates in packageHistory().
- use packageCRAN() instead of mpackages_on_CRAN() in packageHistory().
- return empty data frame for no CRAN, no Archive in packageHistory0().
- make packageHistory0(), packageCRAN() and packageArchive() standalone and 
  private.
- change plot.annualDownloads(f = 1/4).
- add/set plot.annualDownloads(line.col = "gray") and fix outliers.
- replace log_count argument with log.y in plot.bioconductorRank and 
  plot.packageRank().
- rename/set sequenceFilter(delta.time = 10).
- add pre-flight checks to sequenceFilter() and smallFilter().
- replace identifySequences() with removeSequences() in sequenceFilter().
- set packageHistory(check.package = FALSE) in sequenceFilter().
- filter out undersized downloads of past versions in sizeFilter().
- add exception for "R" in validatePackage() and validatePackage0().
- remove tripletFilter() code from utilities.R functions.

#### Documentation

- add annualDownloads() example to README.
- add personal default plots to README.


### packageRank 0.8.3

#### Fixes

- use packageLifeFilter() only when cranDownloads(check.package = TRUE).
- remove fix for doubled cranlogs::cran_downloads(packages %in% c(NULL, pkg))
  counts; underlying 'cranlogs' issue [#68](https://github.com/r-hub/cranlogs/issues/68) fixed.

#### Documentation

- amend/update README.

####  Changes

- clean DESCRIPTION Imports.


### packageRank 0.8.2

#### Fixes

- fix "doubled" cranlogs::cran_downloads(packages = "R") R application 
  download counts in cranDownloads() from 2023-09-12 to 2023-10-02.
- fix doubled cranlogs::cran_downloads(packages %in% c(NULL, pkg)) download
  counts in cranDownloads() from 2023-09-19 to 2023-10-01.
- add timeout fix to checkPackage().

#### Function Changes

- set packageHistory(check.package = FALSE)
- remove "Today's results not available" message from cranDownloads().

#### Documentation

- amend Sunday/Wednesday R Windows application download spikes README note.
- add discussion of "doubled" counts in cranlogs::cran_downloads() to README.


### packageRank 0.8.1

#### New Functionality

- add packages_on_CRAN(), packages_in_Archive(), packages_observed_in_logs()
  and packages_partitioned().
- add extractArchiveDate().
- add/set packages_partitioned(observed.downloads = FALSE).

#### Fixes

- amend packageLog() for packages that get filtered to zero.
- amend/fix sizeFilter() for archive-only packages.
- fix pkgsearch::cran_package_history() "try-error" in packageHistory().
- fix ylim in plot.cranDownloads(graphics = "base", unit.observation = "week").
- fix logInfo() when 'cranlogs' not available.
- set filteredDownloads(all.filters = FALSE) if any individual filter is TRUE.
- add temporary timeout = 600L to packages_partitioned().
- set options(timeout = 600L) in cranPackageSize().
- sort list element names in packageHistory().

#### Function Changes

- set packageLog(check.package = FALSE) in filteredDownloads().
- memoize packages_partitioned().
- add/set dev.mode = FALSE for ipFilter() in countryDistribution(), 
  countryPackage() and ipPackage().
- add memoized archivePackages().
- refactor checkPackage().
- change result data type and content of validatePackage().
- add 'cranlogs' check to cranDownloads().
- add/use memoized packageHistory0() in packageHistory().
- allow mix of CRAN and Archive-only packages (and R) in packageHistory().
- add memoized packages_on_CRAN().
- add CRAN check to packageHistory().

#### Documentation

- note Wednesday R Windows application download spikes in README.


### packageRank 0.8.0

#### Fixes

- add lookup for exceptions in cranMirrors().
- fix plot.cranDownloads(graphics = "base", multi.plot = TRUE, smooth = TRUE).
- fix sequenceFilter() for packages not in CRAN Archive.

#### Function Change

- set annualDownloads(log.y = FALSE).
- add/set annualDownloads(sep.y = FALSE).
- add/set cranMirrors(description = FALSE).
- change logInfo(list.available = FALSE) to logInfo(show.available = FALSE).
- set class of packageHistory()$Date to "Date".
- archive/deprecate packageMRAN() and related functionality.
- set loess as default smoother in plot.cranDownloads()

#### Documentation

- add discussion about R Windows Sunday downloads to README.


### packageRank 0.7.2

#### Function Change

- add exception to logInfo() when 'cranlogs' is down.


### packageRank 0.7.1

#### Fixes
- fix/clean rPlot(r.version = TRUE).
- fix plot(cranDownloads("R"), r.total = TRUE).

#### Function Change

- logInfo() checks for last available log.

### packageRank 0.7.0

#### New Features

- add plot.cranDownloads(unit.observation = “week”).
- discuss ‘from =’ and ‘to =’ cranDownloads() shortcuts in README.

#### Fixes

- enable ipPackage().
- fix cranDownloads(“R”, to = ).
- fix output for packageHistory(“R”).

#### Function Changes

- add logInfo() and deprecate logPostInfo().
- change ‘log.count’ to ‘log.y’ in plot.bioconductorDownloads() and
    plot.cranDownloads().
- limit available unit of observations:
    - with when = “last-week” only unit.observation = “day” available.
    - with when = “last-month” AND graphics = “ggplot2”,
        unit.observation = “month” not available
- recompute cumulative count with plot.cranDownloads(unit.observation
    = “week”) to include backdate data.
- return cores = 1L on Windows with multiCore(); parallelization on
    Windows not currently available.
- use ‘ISOcodes’ in cranMirrors().

#### Graphical Changes

- amend plot.cranDownloads(graphics = “ggplot2”) legends.
- plot.cranDownloads(unit.observation = “month”) date plotted on first
    rather than last day of month.
- use observed date for first week observed data in
    plot.cranDownloads()
- set 0s to 1s to avoid NAs when plot.cranDownloads(log.y = TRUE).

#### Graphical Fixes

- amend xy labels for “count” or “cumulative” in plot.cranDownloads().

#### New Auxiliary/Helper Functions

- add fetchRLog().
- add getCorrectLogs() and rstudio.logs list object.
- add rev_fixDate_2012().
- add/set cranDownloads(fix.cranlogs = TRUE).
- add/use fixCranlogs().
- add/set logDate(fix.date = TRUE).
- add/set resolveDate(fix.date = FALSE).

### packageRank 0.6.0

#### Fixes

- amend fetchCranLog() message.
- fix tripletFilter().
- add exception and warning for packages with zero downloads (not in
    log) for packageLog() and packageRank().
- fix variable names in packageCRAN() and packageArchive().
- fix y-axis typo and labels in cranPlot().
- set data.frame(row.names = NULL) in packageHistory().
- amend/update country code top level domains in cranMirrors().

#### Function Changes

- add packageHistory(“R”).
- allow multiple packages (vector) in packageHistory() and
    packageMRAN().
- restrict parLapply() to dev.mode = TRUE.

#### Graphical Changes

- amend graphical elements for estimate and in-progress download
    counts in plot.cranDownloads().

### packageRank 0.5.0

#### Fixes

- increase fetchCranLog() timeout to 600.

#### Function Changes

- visualize different units of observation: “day” (default), “month”,
    and “year” with plot.cranDownloads(unit.observation = “day”).
- add in-progress plots with nominal and estimated totals for
    aggregate units of observation.

### packageRank 0.4.2

#### Fixes

- fix plot.cranDownloads(package.version = TRUE) for multiple
    packages.
- fix multiple version in sizeFilter() with packageHistory().
- fix and enable sizeFilter().

#### Function Changes

- add/set filteredDownloads(multi.core = TRUE).
- amend warning message with timeMsg() in logDate().

### packageRank 0.4.1

#### Function Changes

- logDate(): check download log URL before computing available_log()
    for logs available before 17:00 UTC.
- change facet_wrap(\~ package, ncol = 2) to facet_wrap(\~ package,
    nrow = 2).
- use logDate() in logPostInfo().
- enable log.count argument for plot.cranDownloads(r.total = TRUE and
    FALSE).
- enable multi.plot argument for rPlot(graphics = “ggplot2”).
- add geom_smooth(span = 3/4).

#### Fixes

- sizeFilter() and cranPackageSize(): fix multiple versions with
    package updates.

#### New Research Functions

- annualDownloads() and weeklyDownloads().

### packageRank 0.4.0

#### New Features and Functionality

- filters shortcut via all.filters = TRUE argument.
- temporarily set options(timeout = 300L) for fetchCranLog() for R
    4.0.3.
- time zone support via logDate().

#### Data Changes

- add pkg.history.
- add arch.pkgs.jul, arch.pkgs.oct, cran.pkgs.jul and cran.pkgs.oct
    samples.
- add october.downloads and july.downloads.
- add top.n.jul2020.
- rename
    blog.data![top.n to blog.data](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;top.n%20to%20blog.data "top.n to blog.data")top.n.oct2019.
- use serialization format = 3 for blog.data object.

#### Fixes

- amend majority rule syntax in timeFix().
- fix Mac directory changes in packageMRAN().
- fix typo in plot.countryDistribution().
- fix resolveDate() by first as.character() all dates.
- fix scrapeData() using strsplit(fixed = TRUE).
- pass ‘f’ argument, lowess() smoother span value, to rPlot().

#### Function Changes

- add add.legend argument to plot.cranFilterCounts().
- add cumsum to cranDownloads() for cumulative counts and growth
    curves.
- add dataset argument to plotTopCountryCodes().
- add packageLog(clean.out = FALSE).
- add population.seed argument to populationPlot().
- add second.place argument to plotTopCountryCodes().
- add subtitle argument to inflationPlot2().
- add wed (Wednesday) argument to inflationPlot2().
- change default orientation of ggplot2 facets in
    plot.cranDownloads().
- note packageArchive() fix for ‘testthat’ filename typo/duplicate.
- set geom_point(size = 1.5) for single day, multiple package plot in
    singlePlot()
- use alpha transparency in plot.packageRank() labels.
- use “crandb_file_date” instead of “Date/Publication” in
    packageHistory().
- use sugrrants::facet_calendar() with packageVersionPercent(),
    plotTopCountryCodes(), and plotDownloadsCountry

#### New Functions

- countryDistribution()
- cranMirrors()
- cranPackages()
- cranPackageSize()
- downloadsCountry()
- filteredDownloads()
- ipCount()
- ipDownloads()
- ipPackage()
- packageMRAN()
- topCountryCodes()

#### New Filter Functions

- ipFilter()
- sequenceFilter()
- sizeFilter()
- smallFilter()
- tripletFilter()

#### New Helper Functions

- checkPackage()
- cleanLog()
- dateTime()
- packageSample()
- validatePackage()

#### New Time/Time Zone Functions

- currentTime()
- utc()
- utc0()
- localTime()
- logDate()
- logPostInfo()

#### New/Amended Blog specific Functions

- countsRanks()
- cranPlot()
- inflationPlot2()
- monthlyLog()
- packageVersionPercent()
- versionPlot()

### packageRank 0.3.5

#### Blog specific functions

- countsRanks()
- inflationPlot()

#### Data Changes

- add blog.data

#### Fixes

- fixDate_2012() for packageLog() Correct mislabeled filenames for
    2012 logs RStudio’s CRAN download logs at
    <http://cran-logs.rstudio.com/>.

- fix “`geom_smooth()` using formula ‘y \~ x’” warning for ‘ggplot2’
    v3.3.0.

#### Function Changes

- enable arbitrary “to” argument without “from” in cranDownloads()
    e.g., cranDownloads(from = NULL, to = “2020-01-01”).
- add/amend plot.cranDownloads(package.version = FALSE, r.version =
    FALSE).
- set plot.cranDownloads(points = “auto”) for \<= 45 days.
- “spell check” package names via “check.package” argument
    cranDownloads(), packageDistribution(), packageRank()
- deprecate packageRankTime() -\> plot.cranDownloads(population.plot =
    TRUE)
- use packageLog(packages = NULL) to view a day’s log.
- enable negative filter values in packageLog() and packageRank().

#### New Functions

- archivePackages()
- countryPackage()
- packageArchive()
- packageCountry()
- packageDistribution()
- packageHistory() uses ‘pkgsearch’; packageHistory0() scrapes CRAN.
- packageInfo()
- validatePackage() uses ‘pkgsearch’; validatePackage0() scrapes CRAN.

### packageRank 0.3.0

- add cranDownloads().
    - enable “yyyy-mm-dd”, “yyyy-mm” or “yyyy” in cranDownloads(from,
        to).
    - add plot.cran_downloads(points = “auto”).
    - add plot.cran_downloads(r.version).
- deprecate cran_downloads2().
- add dayOfMonth().
- add bioconductorDownloads().
- add bioconductorRank().

### packageRank 0.2.0

- implement minor parallelization for packageRankTime().
- base graphics for single package/date; ggplot2 for multiple
    packages/dates.
- fix smooth argument in plot.package_rank_time(graphics_pkg =
    “base”).

### packageRank 0.1.0

- first CRAN release.
