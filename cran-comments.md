### Test environments

* local: macOS 12.6 ; R 4.2.1 (2022-06-23)
* win-builder: devel, oldrelease and release


### R CMD check results

4.2.1 (2022-06-23), x86_64-apple-darwin17.0 (64-bit):
* 0 errors | 0 warnings | 0 notes


### R win-builder check results - x86_64-w64-mingw32 (64-bit)

R-devel: R Under development (2022-09-12 r82842 ucrt):
* 0 errors | 0 warnings | 0 notes

R-oldrelease: R 4.1.3 (2022-03-10:
* 0 errors | 0 warnings | 1 note
Possibly mis-spelled word: RStudio's

R-release: R 4.2.1 (2022-06-23 ucrt):
* 0 errors | 0 warnings | 0 notes


### Reverse dependencies

There are no reverse dependencies:
tools::package_dependencies("packageRank", reverse = TRUE) returns character(0)
