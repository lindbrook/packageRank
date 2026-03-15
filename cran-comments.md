### Test environments

* local: macOS 26.3.1; R 4.5.3 (2026-03-11) 
* win-builder: devel, oldrelease and release


### R CMD check results

R 4.5.3 (2026-03-11), aarch64-apple-darwin20:
* 0 errors | 0 warnings | 0 notes


### R win-builder check results - x86_64-w64-mingw32 (64-bit)

R-devel: R Under development (unstable) (2026-03-13 r89618 ucrt):
* 0 errors | 0 warnings | 0 notes

R-oldrelease: R 4.4.3 Patched (2026-02-12 r89426 ucrt):
* 0 errors | 0 warnings | 1 note

Found the following (possibly) invalide URLs:
  URL: https://cloud.R-project.org/
  From: README.md
CRAN URL not in canonical form
Canonical CRAN.R-project.org URLs use https.

-- I tried https://CLOUD.R-project.org/, https://cloud.R-project.org/, and
  https://cloud.r-project.org/

R-release: R 4.5.2 Patched (2026-02-13 r89426 ucrt):
* 0 errors | 0 warnings | 1 note

Found the following (possibly) invalide URLs:
  URL: https://cloud.R-project.org/
  From: README.md
CRAN URL not in canonical form
Canonical CRAN.R-project.org URLs use https.

-- I tried https://CLOUD.R-project.org/, https://cloud.R-project.org/, and
  https://cloud.r-project.org/

### Reverse dependencies

There are no reverse dependencies:
tools::package_dependencies("packageRank", reverse = TRUE) returns character(0).

Reverse suggests: 'BAwiR'
revdepcheck::revdep_check() reports no problems at all.
