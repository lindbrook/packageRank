#### Test environments

* local: macOS 27.0; R 4.6.1 (2026-06-24)
* win-builder: devel, oldrelease and release


#### R CMD check results

R 4.6.1 (2026-06-24), aarch64-apple-darwin23:  
0 errors | 0 warnings | 0 notes


#### R win-builder check results - x86_64-w64-mingw32 (64-bit)

R-devel: R Under development (unstable) (2026-09-20 r90574 ucrt):  
0 errors | 0 warnings | 0 notes

R-oldrelease: R 4.5.3 (2026-03-11 ucrt):  
0 errors | 0 warnings | 1 note.

```
    Found the following (possibly) invalide URLs:
      URL: https://cloud.R-project.org/
      From: README.md
    CRAN URL not in canonical form
    Canonical CRAN.R-project.org URLs use https.

    << I tried https://CLOUD.R-project.org/, https://cloud.R-project.org/, and
    https://cloud.r-project.org/ >>
```

R-release: R 4.6.1 (2026-06-24 ucrt):  
0 errors | 0 warnings | 0 note


#### Reverse dependencies

Reverse imports: 'packageRankWrapperDriver'   
Reverse suggests: 'BAwiR'

'revdepcheck' results:

```
  We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.
  We saw 0 new problems
  We failed to check 0 packages
```
