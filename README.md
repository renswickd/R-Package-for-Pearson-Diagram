
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pearson.diagram

<!-- badges: start -->
<!-- badges: end -->

The Pearson Diagram R package provides tools to visualize distributions
using Pearson Diagrams. It helps users calculate skewness and kurtosis
for unknown data distributions, plot the Pearson Diagram, and compare
multiple distributions. This package leverages **Rcpp** for efficient
computation and follows the **S3** object-oriented programming paradigm
in R.

## Key Features

- **Plotting Pearson Diagrams:** Visualize distributions on a Pearson
  Diagram by plotting skewness and kurtosis.
- **Comparison of Multiple Distributions:** Compute and compare skewness
  and kurtosis for multiple distributions.
- **Plot the unknown data:** Allows users to include an unknown data in
  the Pearson Diagram and helps to identify the distribution.
- **Efficient Calculation with Rcpp:** Uses C++ for high-performance
  calculation of skewness and kurtosis.
- **S3 Object-Oriented Programming:** Encapsulates functionality within
  S3 classes for Pearson Diagrams.

## To validate the build status

Clone the repository to local folder and run the below codes

``` r
library(devtools)
```

Work on the below command to check if the package is build successfully.

``` r
devtools::check()
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Updating pearson.diagram documentation
#> ℹ Loading pearson.diagram
#> 
#> ══ Building ════════════════════════════════════════════════════════════════════
#> Setting env vars:
#> • CFLAGS    : -Wall -pedantic
#> • CXXFLAGS  : -Wall -pedantic
#> • CXX11FLAGS: -Wall -pedantic
#> • CXX14FLAGS: -Wall -pedantic
#> • CXX17FLAGS: -Wall -pedantic
#> • CXX20FLAGS: -Wall -pedantic
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>      checking for file ‘/Users/rens/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STUDENT/Assignments/T2/DATA501/DATA501-project/project-plan/pearson.diagram/DESCRIPTION’ ...  ✔  checking for file ‘/Users/rens/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STUDENT/Assignments/T2/DATA501/DATA501-project/project-plan/pearson.diagram/DESCRIPTION’
#>   ─  preparing ‘pearson.diagram’:
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>   ─  cleaning src
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>      Omitted ‘LazyData’ from DESCRIPTION
#>   ─  building ‘pearson.diagram_0.1.0.tar.gz’
#>      
#> ══ Checking ════════════════════════════════════════════════════════════════════
#> Setting env vars:
#> • _R_CHECK_CRAN_INCOMING_REMOTE_               : FALSE
#> • _R_CHECK_CRAN_INCOMING_                      : FALSE
#> • _R_CHECK_FORCE_SUGGESTS_                     : FALSE
#> • _R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_: FALSE
#> • NOT_CRAN                                     : true
#> ── R CMD check ─────────────────────────────────────────────────────────────────
#>   ─  using log directory ‘/private/var/folders/z6/47hkfr6s6d1_cchpbbsz96k40000gn/T/RtmpilBqt3/fileb4043a3ec67a/pearson.diagram.Rcheck’
#> ─  using R version 4.4.0 (2024-04-24)
#>   ─  using platform: aarch64-apple-darwin20
#> ─  R was compiled by
#>        Apple clang version 14.0.0 (clang-1400.0.29.202)
#>        GNU Fortran (GCC) 12.2.0
#> ─  running under: macOS Sonoma 14.3
#> ─  using session charset: UTF-8
#>   ─  using options ‘--no-manual --as-cran’
#>   ✔  checking for file ‘pearson.diagram/DESCRIPTION’
#>   ─  checking extension type ... Package
#> ─  this is package ‘pearson.diagram’ version ‘0.1.0’
#> ─  package encoding: UTF-8
#>    checking package namespace information ...  ✔  checking package namespace information
#>      checking package dependencies ...  ✔  checking package dependencies (4.8s)
#>    checking if this is a source package ...  ✔  checking if this is a source package
#> ✔  checking if there is a namespace
#>      checking for executable files ...  ✔  checking for executable files
#>   ✔  checking for hidden files and directories
#>    checking for portable file names ...  ✔  checking for portable file names
#> ✔  checking for sufficient/correct file permissions
#>   ✔  checking serialization versions
#>      checking whether package ‘pearson.diagram’ can be installed ...  ✔  checking whether package ‘pearson.diagram’ can be installed (2.6s)
#>   ─  used C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.3.9.4)’
#> ─  used SDK: ‘MacOSX14.4.sdk’
#>    checking installed package size ...  ✔  checking installed package size
#>      checking package directory ...  ✔  checking package directory
#>    checking for future file timestamps ...  ✔  checking for future file timestamps
#>      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>    checking top-level files ...  ✔  checking top-level files
#> ✔  checking for left-over files
#> ✔  checking index information
#>    checking package subdirectories ...  ✔  checking package subdirectories
#>    checking code files for non-ASCII characters ...  ✔  checking code files for non-ASCII characters
#>    checking R files for syntax errors ...  ✔  checking R files for syntax errors
#>    checking whether the package can be loaded ...  ✔  checking whether the package can be loaded
#>    checking whether the package can be loaded with stated dependencies ...  ✔  checking whether the package can be loaded with stated dependencies
#>    checking whether the package can be unloaded cleanly ...  ✔  checking whether the package can be unloaded cleanly
#>    checking whether the namespace can be loaded with stated dependencies ...  ✔  checking whether the namespace can be loaded with stated dependencies
#>    checking whether the namespace can be unloaded cleanly ...  ✔  checking whether the namespace can be unloaded cleanly
#>      checking dependencies in R code ...  ✔  checking dependencies in R code (446ms)
#>    checking S3 generic/method consistency ...  ✔  checking S3 generic/method consistency
#>    checking replacement functions ...  ✔  checking replacement functions
#>    checking foreign function calls ...  ✔  checking foreign function calls
#>    checking R code for possible problems ...  ✔  checking R code for possible problems (936ms)
#>    checking Rd files ...  ✔  checking Rd files
#>    checking Rd metadata ...  ✔  checking Rd metadata
#>    checking Rd line widths ...  ✔  checking Rd line widths
#>    checking Rd cross-references ...  ✔  checking Rd cross-references
#>    checking for missing documentation entries ...  ✔  checking for missing documentation entries
#>    checking for code/documentation mismatches ...  ✔  checking for code/documentation mismatches (346ms)
#>    checking Rd \usage sections ...  ✔  checking Rd \usage sections
#>      checking Rd contents ...  ✔  checking Rd contents
#>    checking for unstated dependencies in examples ...  ✔  checking for unstated dependencies in examples
#>   ✔  checking line endings in C/C++/Fortran sources/headers
#>      checking pragmas in C/C++ headers and code ...  ✔  checking pragmas in C/C++ headers and code
#>   ✔  checking compilation flags used
#>    checking compiled code ...  ✔  checking compiled code
#>      checking examples ...  ✔  checking examples
#>      checking for non-standard things in the check directory ...  ✔  checking for non-standard things in the check directory
#>    checking for detritus in the temp directory ...  ✔  checking for detritus in the temp directory
#>      
#>    
#> 
#> ── R CMD check results ────────────────────────────── pearson.diagram 0.1.0 ────
#> Duration: 11.7s
#> 
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
devtools::check(document = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Updating pearson.diagram documentation
#> ℹ Loading pearson.diagram
#> 
#> ══ Building ════════════════════════════════════════════════════════════════════
#> Setting env vars:
#> • CFLAGS    : -Wall -pedantic
#> • CXXFLAGS  : -Wall -pedantic
#> • CXX11FLAGS: -Wall -pedantic
#> • CXX14FLAGS: -Wall -pedantic
#> • CXX17FLAGS: -Wall -pedantic
#> • CXX20FLAGS: -Wall -pedantic
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>      checking for file ‘/Users/rens/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STUDENT/Assignments/T2/DATA501/DATA501-project/project-plan/pearson.diagram/DESCRIPTION’ ...  ✔  checking for file ‘/Users/rens/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STUDENT/Assignments/T2/DATA501/DATA501-project/project-plan/pearson.diagram/DESCRIPTION’
#>   ─  preparing ‘pearson.diagram’:
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>   ─  cleaning src
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>      Omitted ‘LazyData’ from DESCRIPTION
#>   ─  building ‘pearson.diagram_0.1.0.tar.gz’
#>      
#> ══ Checking ════════════════════════════════════════════════════════════════════
#> Setting env vars:
#> • _R_CHECK_CRAN_INCOMING_REMOTE_               : FALSE
#> • _R_CHECK_CRAN_INCOMING_                      : FALSE
#> • _R_CHECK_FORCE_SUGGESTS_                     : FALSE
#> • _R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_: FALSE
#> • NOT_CRAN                                     : true
#> ── R CMD check ─────────────────────────────────────────────────────────────────
#>   ─  using log directory ‘/private/var/folders/z6/47hkfr6s6d1_cchpbbsz96k40000gn/T/RtmpilBqt3/fileb40451dd0648/pearson.diagram.Rcheck’
#>   ─  using R version 4.4.0 (2024-04-24)
#> ─  using platform: aarch64-apple-darwin20
#> ─  R was compiled by
#>        Apple clang version 14.0.0 (clang-1400.0.29.202)
#>        GNU Fortran (GCC) 12.2.0
#> ─  running under: macOS Sonoma 14.3
#> ─  using session charset: UTF-8
#>   ─  using options ‘--no-manual --as-cran’
#>      checking for file ‘pearson.diagram/DESCRIPTION’ ...  ✔  checking for file ‘pearson.diagram/DESCRIPTION’
#>   ─  checking extension type ... Package
#> ─  this is package ‘pearson.diagram’ version ‘0.1.0’
#> ─  package encoding: UTF-8
#>    checking package namespace information ...  ✔  checking package namespace information
#>    checking package dependencies ...  ✔  checking package dependencies (6.6s)
#>   ✔  checking if this is a source package
#> ✔  checking if there is a namespace
#>      checking for executable files ...  ✔  checking for executable files
#>    checking for hidden files and directories ...  ✔  checking for hidden files and directories
#>    checking for portable file names ...  ✔  checking for portable file names
#> ✔  checking for sufficient/correct file permissions
#>   ✔  checking serialization versions
#>      checking whether package ‘pearson.diagram’ can be installed ...  ✔  checking whether package ‘pearson.diagram’ can be installed (2.4s)
#> ─  used C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.3.9.4)’
#>   ─  used SDK: ‘MacOSX14.4.sdk’
#>    checking installed package size ...  ✔  checking installed package size
#>      checking package directory ...  ✔  checking package directory
#>    checking for future file timestamps ...  ✔  checking for future file timestamps (434ms)
#>      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>    checking top-level files ...  ✔  checking top-level files
#> ✔  checking for left-over files
#> ✔  checking index information
#>    checking package subdirectories ...  ✔  checking package subdirectories
#>    checking code files for non-ASCII characters ...  ✔  checking code files for non-ASCII characters
#>    checking R files for syntax errors ...  ✔  checking R files for syntax errors
#>    checking whether the package can be loaded ...  ✔  checking whether the package can be loaded
#>    checking whether the package can be loaded with stated dependencies ...  ✔  checking whether the package can be loaded with stated dependencies
#>    checking whether the package can be unloaded cleanly ...  ✔  checking whether the package can be unloaded cleanly
#>    checking whether the namespace can be loaded with stated dependencies ...  ✔  checking whether the namespace can be loaded with stated dependencies
#>    checking whether the namespace can be unloaded cleanly ...  ✔  checking whether the namespace can be unloaded cleanly
#>      checking dependencies in R code ...  ✔  checking dependencies in R code (474ms)
#>    checking S3 generic/method consistency ...  ✔  checking S3 generic/method consistency
#>    checking replacement functions ...  ✔  checking replacement functions
#>    checking foreign function calls ...  ✔  checking foreign function calls
#>    checking R code for possible problems ...  ✔  checking R code for possible problems (929ms)
#>    checking Rd files ...  ✔  checking Rd files
#>    checking Rd metadata ...  ✔  checking Rd metadata
#>    checking Rd line widths ...  ✔  checking Rd line widths
#>    checking Rd cross-references ...  ✔  checking Rd cross-references
#>      checking for missing documentation entries ...  ✔  checking for missing documentation entries
#>    checking for code/documentation mismatches ...  ✔  checking for code/documentation mismatches (350ms)
#>    checking Rd \usage sections ...  ✔  checking Rd \usage sections
#>    checking Rd contents ...  ✔  checking Rd contents
#>    checking for unstated dependencies in examples ...  ✔  checking for unstated dependencies in examples
#>   ✔  checking line endings in C/C++/Fortran sources/headers
#>      checking pragmas in C/C++ headers and code ...  ✔  checking pragmas in C/C++ headers and code
#>   ✔  checking compilation flags used
#>    checking compiled code ...  ✔  checking compiled code
#>      checking examples ...  ✔  checking examples
#>   ✔  checking for non-standard things in the check directory
#>   ✔  checking for detritus in the temp directory
#>      
#>    
#> 
#> ── R CMD check results ────────────────────────────── pearson.diagram 0.1.0 ────
#> Duration: 13.5s
#> 
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
# devtools::check(document = TRUE, manual = TRUE)
```
