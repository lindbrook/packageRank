#' Check for valid package names.
#'
#' @param package Character. Vector of package name(s).
#' @noRd

validatePackage <- function(package) {
  check <- unlist(lapply(package, function(x) {
    class(try(pkgsearch::cran_package(x), silent = TRUE))
  }))
  
  if (any(check == "try-error")) {
    out <- data.frame(package = package, pkgsearch = check != "try-error")
    if ("R" %in% out$package) {
      out[out$package == "R", "pkgsearch"] <- TRUE
    }
  } else {
    out <- data.frame(package = package, pkgsearch = TRUE)
  }
  out
}

#' Check for valid package names (scrape CRAN).
#'
#' @param package Character. Vector of package name(s).
#' @param check.archive Logical. Include archive when validating package. This is computationally expensive because it scrapes https://cran.r-project.org/src/contrib/Archive/.
#' @noRd

validatePackage0 <- function(package, check.archive = TRUE) {
  url <- "https://cloud.r-project.org/"
  cran <- as.data.frame(utils::available.packages(repos = url),
    stringsAsFactors = FALSE)

  # Platform specific packages
  # By default, utils::available.packages() excludes non-applicable packages.
  if (.Platform$OS.type == "windows") {
    unix.package <- c("bigGP", "bigReg", "CommT", "corrcoverage", "cronR",
      "doMC", "exif", "gcbd", "ieeeround", "kmcudaR", "littler", "nice",
      "PACVr", "permGPU", "qtbase", "R4dfp", "RAppArmor", "RcppGetconf",
      "Rdsm", "Rip46", "Rpoppler", "rPython", "rrd", "RSvgDevice", "rsyslog",
      "RVowpalWabbit", "snpStatsWriter", "solarius", "spp", "ssh.utils",
      "uaparserjs", "unix")
    pkgs <- c(cran$Package, unix.package)

  } else {
    windows.package <- c("BiplotGUI", "blatr", "excel.link", "installr",
      "KeyboardSimulator", "MDSGUI", "R2PPT", "R2wd", "RInno", "RWinEdt",
      "spectrino", "taskscheduleR")
    pkgs <- c(cran$Package, windows.package)
  }

  if (check.archive) {
    archive <- setdiff(marchivePackages(), pkgs)
    pkgs <- c(pkgs, archive, "R")
  } else pkgs <- c(pkgs, "R")

  if (any(package %in% pkgs == FALSE)) {
    list(invalid = package[package %in% pkgs == FALSE],
         valid = package[package %in% pkgs])
  }
}
