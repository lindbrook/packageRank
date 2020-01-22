#' Check for valid package names.
#'
#' @param packages Character. Vector of package name(s).
#' @param include.archive Logical. Include archive when validating package. This is computationally expensive becuase it scrapes https://cran.r-project.org/src/contrib/Archive/.
#' @export

validatePackage <- function(packages, include.archive = TRUE) {
  cran <- as.data.frame(utils::available.packages(), stringsAsFactors = FALSE)

  # Platform specific packages
  # By default, utils::available.packages() excludes non-applicable packages.
  if (.Platform$OS.type == "windows") {
    unix.package <- c("bigGP", "bigReg", "CommT", "corrcoverage", "cronR",
      "doMC","exif", "gcbd", "ieeeround", "kmcudaR", "littler", "nice",
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

  if (include.archive) {
    archive <- setdiff(archivePackages(), pkgs)
    pkgs <- c(pkgs, archive)
  }

  if (any(packages %in% pkgs == FALSE)) {
    list(invalid = packages[packages %in% pkgs == FALSE],
         valid = packages[packages %in% pkgs])
  } else packages
}
