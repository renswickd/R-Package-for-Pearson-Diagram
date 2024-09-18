
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
- **Efficient Calculation with Rcpp:** Uses C++ for high-performance
  calculation of skewness and kurtosis.
- **S3 Object-Oriented Programming:** Encapsulates functionality within
  S3 classes for Pearson Diagrams.

## Installation

You can install the development version of pearson.diagram like so:

``` r
# Yet to be added
```

## Example

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
#>      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
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
#>   ─  using log directory ‘/private/var/folders/z6/47hkfr6s6d1_cchpbbsz96k40000gn/T/RtmpqMrXZ0/file39b9119d3c82/pearson.diagram.Rcheck’
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
#>      checking package dependencies ...  ✔  checking package dependencies (5.2s)
#>     ✔  checking if this is a source package
#> ✔  checking if there is a namespace
#>      checking for executable files ...  ✔  checking for executable files
#>   ✔  checking for hidden files and directories
#>    checking for portable file names ...  ✔  checking for portable file names
#>    checking for sufficient/correct file permissions ...  ✔  checking for sufficient/correct file permissions
#>    checking serialization versions ...  ✔  checking serialization versions
#>    checking whether package ‘pearson.diagram’ can be installed ...  ✔  checking whether package ‘pearson.diagram’ can be installed (2.5s)
#> ─  used C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.3.9.4)’
#>   ─  used SDK: ‘MacOSX14.4.sdk’
#>    checking installed package size ...  ✔  checking installed package size
#>      checking package directory ...  ✔  checking package directory
#>    checking for future file timestamps ...  ✔  checking for future file timestamps
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
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
#>      checking dependencies in R code ...  ✔  checking dependencies in R code (451ms)
#>    checking S3 generic/method consistency ...  ✔  checking S3 generic/method consistency
#>    checking replacement functions ...  ✔  checking replacement functions
#>    checking foreign function calls ...  ✔  checking foreign function calls
#>      checking R code for possible problems ...  ✔  checking R code for possible problems (892ms)
#>    checking Rd files ...  ✔  checking Rd files
#>    checking Rd metadata ...  ✔  checking Rd metadata
#>    checking Rd line widths ...  ✔  checking Rd line widths
#>    checking Rd cross-references ...  ✔  checking Rd cross-references
#>    checking for missing documentation entries ...  ✔  checking for missing documentation entries
#>    checking for code/documentation mismatches ...  ✔  checking for code/documentation mismatches
#>    checking Rd \usage sections ...  ✔  checking Rd \usage sections
#>    checking Rd contents ...  ✔  checking Rd contents
#>        checking for unstated dependencies in examples ...  ✔  checking for unstated dependencies in examples
#>      checking line endings in C/C++/Fortran sources/headers ...  ✔  checking line endings in C/C++/Fortran sources/headers
#>      checking pragmas in C/C++ headers and code ...  ✔  checking pragmas in C/C++ headers and code
#>   ✔  checking compilation flags used
#>      checking compiled code ...  ✔  checking compiled code
#>      checking examples ...  ✔  checking examples
#>      checking for non-standard things in the check directory ...  ✔  checking for non-standard things in the check directory
#>    checking for detritus in the temp directory ...  ✔  checking for detritus in the temp directory
#>      
#>    
#> 
#> ── R CMD check results ────────────────────────────── pearson.diagram 0.1.0 ────
#> Duration: 11.8s
#> 
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
devtools::check(document = TRUE, manual = TRUE)
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
#>   ─  using log directory ‘/private/var/folders/z6/47hkfr6s6d1_cchpbbsz96k40000gn/T/RtmpqMrXZ0/file39b947fa69f4/pearson.diagram.Rcheck’
#>   ─  using R version 4.4.0 (2024-04-24)
#> ─  using platform: aarch64-apple-darwin20
#> ─  R was compiled by
#>        Apple clang version 14.0.0 (clang-1400.0.29.202)
#>        GNU Fortran (GCC) 12.2.0
#> ─  running under: macOS Sonoma 14.3
#> ─  using session charset: UTF-8
#>   ─  using option ‘--as-cran’
#>      checking for file ‘pearson.diagram/DESCRIPTION’ ...  ✔  checking for file ‘pearson.diagram/DESCRIPTION’
#>   ─  checking extension type ... Package
#> ─  this is package ‘pearson.diagram’ version ‘0.1.0’
#> ─  package encoding: UTF-8
#>    checking package namespace information ...  ✔  checking package namespace information
#>    checking package dependencies ...  ✔  checking package dependencies (4.2s)
#>    checking if this is a source package ...  ✔  checking if this is a source package
#> ✔  checking if there is a namespace
#>      checking for executable files ...  ✔  checking for executable files
#>   ✔  checking for hidden files and directories
#>    checking for portable file names ...  ✔  checking for portable file names
#>    checking for sufficient/correct file permissions ...  ✔  checking for sufficient/correct file permissions
#>   ✔  checking serialization versions
#>      checking whether package ‘pearson.diagram’ can be installed ...  ✔  checking whether package ‘pearson.diagram’ can be installed (2.6s)
#>   ─  used C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.3.9.4)’
#> ─  used SDK: ‘MacOSX14.4.sdk’
#>    checking installed package size ...  ✔  checking installed package size
#>      checking package directory ...  ✔  checking package directory
#>    checking for future file timestamps ...  ✔  checking for future file timestamps
#>    checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>    checking top-level files ...  ✔  checking top-level files
#> ✔  checking for left-over files
#> ✔  checking index information
#>      checking package subdirectories ...  ✔  checking package subdirectories
#>    checking code files for non-ASCII characters ...  ✔  checking code files for non-ASCII characters
#>    checking R files for syntax errors ...  ✔  checking R files for syntax errors
#>    checking whether the package can be loaded ...  ✔  checking whether the package can be loaded
#>    checking whether the package can be loaded with stated dependencies ...  ✔  checking whether the package can be loaded with stated dependencies
#>    checking whether the package can be unloaded cleanly ...  ✔  checking whether the package can be unloaded cleanly
#>    checking whether the namespace can be loaded with stated dependencies ...  ✔  checking whether the namespace can be loaded with stated dependencies
#>    checking whether the namespace can be unloaded cleanly ...  ✔  checking whether the namespace can be unloaded cleanly
#>      checking dependencies in R code ...  ✔  checking dependencies in R code (460ms)
#>    checking S3 generic/method consistency ...  ✔  checking S3 generic/method consistency
#>    checking replacement functions ...  ✔  checking replacement functions
#>    checking foreign function calls ...  ✔  checking foreign function calls
#>    checking R code for possible problems ...  ✔  checking R code for possible problems (914ms)
#>    checking Rd files ...  ✔  checking Rd files
#>    checking Rd metadata ...  ✔  checking Rd metadata
#>    checking Rd line widths ...  ✔  checking Rd line widths
#>    checking Rd cross-references ...  ✔  checking Rd cross-references
#>    checking for missing documentation entries ...  ✔  checking for missing documentation entries
#>    checking for code/documentation mismatches ...  ✔  checking for code/documentation mismatches (363ms)
#>    checking Rd \usage sections ...  ✔  checking Rd \usage sections
#>      checking Rd contents ...  ✔  checking Rd contents
#>      checking for unstated dependencies in examples ...  ✔  checking for unstated dependencies in examples
#>      checking line endings in C/C++/Fortran sources/headers ...  ✔  checking line endings in C/C++/Fortran sources/headers
#>      checking pragmas in C/C++ headers and code ...  ✔  checking pragmas in C/C++ headers and code
#>   ✔  checking compilation flags used
#>      checking compiled code ...  ✔  checking compiled code
#>      checking examples ...  ✔  checking examples
#>      checking PDF version of manual ...  ✔  checking PDF version of manual (1.5s)
#>      checking HTML version of manual ...  N  checking HTML version of manual
#>      Found the following HTML validation problems:
#>    PearsonDiagram.html:15:44 (PearsonDiagram.Rd:5): Error: <main> is not recognized!
#>    PearsonDiagram.html:15:44 (PearsonDiagram.Rd:5): Warning: discarding unexpected <main>
#>    PearsonDiagram.html:44:1 (PearsonDiagram.Rd:16): Warning: discarding unexpected </main>
#>    PearsonDiagram.html:4:1 (PearsonDiagram.Rd:5): Warning: <link> inserting "type" attribute
#>    PearsonDiagram.html:12:1 (PearsonDiagram.Rd:5): Warning: <script> proprietary attribute "onload"
#>    PearsonDiagram.html:12:1 (PearsonDiagram.Rd:5): Warning: <script> inserting "type" attribute
#>    PearsonDiagram.html:17:1 (PearsonDiagram.Rd:5): Warning: <table> lacks "summary" attribute
#>    add_point.html:15:44 (add_point.Rd:5): Error: <main> is not recognized!
#>    add_point.html:15:44 (add_point.Rd:5): Warning: discarding unexpected <main>
#>    add_point.html:56:1 (add_point.Rd:17): Warning: discarding unexpected </main>
#>    add_point.html:4:1 (add_point.Rd:5): Warning: <link> inserting "type" attribute
#>    add_point.html:12:1 (add_point.Rd:5): Warning: <script> proprietary attribute "onload"
#>    add_point.html:12:1 (add_point.Rd:5): Warning: <script> inserting "type" attribute
#>    add_point.html:17:1 (add_point.Rd:5): Warning: <table> lacks "summary" attribute
#>    add_point.html:35:1 (add_point.Rd:10): Warning: <table> lacks "summary" attribute
#>    compare_distributions.PearsonDiagram.html:15:44 (compare_distributions.PearsonDiagram.Rd:5): Error: <main> is not recognized!
#>    compare_distributions.PearsonDiagram.html:15:44 (compare_distributions.PearsonDiagram.Rd:5): Warning: discarding unexpected <main>
#>    compare_distributions.PearsonDiagram.html:58:1 (compare_distributions.PearsonDiagram.Rd:19): Warning: discarding unexpected </main>
#>    compare_distributions.PearsonDiagram.html:4:1 (compare_distributions.PearsonDiagram.Rd:5): Warning: <link> inserting "type" attribute
#>    compare_distributions.PearsonDiagram.html:12:1 (compare_distributions.PearsonDiagram.Rd:5): Warning: <script> proprietary attribute "onload"
#> compare_distributions.     compare_distributions.PearsonDiagram.html:12:1 (compare_distributions.PearsonDiagram.Rd:5): Warning: <script> inserting "type" attribute
#>    compare_distributions.PearsonDiagram.html:17:1 (compare_distributions.PearsonDiagram.Rd:5): Warning: <table> lacks "summary" attribute
#>    compare_distributions.PearsonDiagram.html:39:1 (compare_distributions.PearsonDiagram.Rd:12): Warning: <table> lacks "summary" attribute
#>    compare_distributions.html:15:44 (compare_distributions.Rd:5): Error: <main> is not recognized!
#>    compare_distributions.html:15:44 (compare_distributions.Rd:5): Warning: discarding unexpected <main>
#>    compare_distributions.html:52:1 (compare_distributions.Rd:15): Warning: discarding unexpected </main>
#>    compare_distributions.html:4:1 (compare_distributions.Rd:5): Warning: <link> inserting "type" attribute
#>    compare_distributions.html:12:1 (compare_distributions.Rd:5): Warning: <script> proprietary attribute "onload"
#>    compare_distributions.html:12:1 (compare_distributions.Rd:5): Warning: <script> inserting "type" attribute
#>    compare_distributions.html:17:1 (compare_distributions.Rd:5): Warning: <table> lacks "summary" attribute
#>    compare_distributions.html:35:1 (compare_distributions.Rd:10): Warning: <table> lacks "summary" attribute
#>    cpp_calculate_skewness_kurtosis.html:15:44 (cpp_calculate_skewness_kurtosis.Rd:5): Error: <main> is not recognized!
#>    cpp_calculate_skewness_kurtosis.html:15:44 (cpp_calculate_skewness_kurtosis.Rd:5): Warning: discarding unexpected <main>
#>    cpp_calculate_skewness_kurtosis.html:42:1 (cpp_calculate_skewness_kurtosis.Rd:10): Warning: discarding unexpected </main>
#>    cpp_calculate_skewness_kurtosis.html:4:1 (cpp_calculate_skewness_kurtosis.Rd:5): Warning: <link> inserting "type" attribute
#>    cpp_calculate_skewness_kurtosis.html:12:1 (cpp_calculate_skewness_kurtosis.Rd:5): Warning: <script> proprietary attribute "onload"
#>    cpp_calculate_skewness_kurtosis.html:12:1 (cpp_calculate_skewness_kurtosis.Rd:5): Warning: <script> inserting "type" attribute
#> cpp_calculate_skewness_kurtosis.html:17:1     cpp_calculate_skewness_kurtosis.html:17:1 (cpp_calculate_skewness_kurtosis.Rd:5): Warning: <table> lacks "summary" attribute
#>    cpp_calculate_skewness_kurtosis.html:29:1 (cpp_calculate_skewness_kurtosis.Rd:7): Warning: <table> lacks "summary" attribute
#>    highlight_point_on_hover.html:15:44 (highlight_point_on_hover.Rd:5): Error: <main> is not recognized!
#>    highlight_point_on_hover.html:15:44 (highlight_point_on_hover.Rd:5): Warning: discarding unexpected <main>
#>    highlight_point_on_hover.html:48:1 (highlight_point_on_hover.Rd:13): Warning: discarding unexpected </main>
#>    highlight_point_on_hover.html:4:1 (highlight_point_on_hover.Rd:5): Warning: <link> inserting "type" attribute
#>    highlight_point_on_hover.html:12:1 (highlight_point_on_hover.Rd:5): Warning: <script> proprietary attribute "onload"
#>    highlight_point_on_hover.html:12:1 (highlight_point_on_hover.Rd:5): Warning: <script> inserting "type" attribute
#>    highlight_point_on_hover.html:17:1 (highlight_point_on_hover.Rd:5): Warning: <table> lacks "summary" attribute
#>    highlight_point_on_hover.html:35:1 (highlight_point_on_hover.Rd:10): Warning: <table> lacks "summary" attribute
#>    plot_diagram.PearsonDiagram.html:15:44 (plot_diagram.PearsonDiagram.Rd:5): Error: <main> is not recognized!
#>    plot_diagram.PearsonDiagram.html:15:44 (plot_diagram.PearsonDiagram.Rd:5): Warning: discarding unexpected <main>
#>    plot_diagram.PearsonDiagram.html:49:1 (plot_diagram.PearsonDiagram.Rd:13): Warning: discarding unexpected </main>
#>    plot_diagram.PearsonDiagram.html:4:1 (plot_diagram.PearsonDiagram.Rd:5): Warning: <link> inserting "type" attribute
#>    plot_diagram.PearsonDiagram.html:12:1 (plot_diagram.PearsonDiagram.Rd:5): Warning: <script> proprietary attribute "onload"
#>    plot_diagram.PearsonDiagram.html:12:1 (plot_diagram.PearsonDiagram.Rd:5): Warning: <script> inserting "type" attribute
#>    plot_diagram.PearsonDiagram.html:17:1 (plot_diagram.PearsonDiagram.Rd:5): Warning: <table> lacks "summary" attribute
#> plot_diagram.PearsonDiagram.html:36:1 (plot_diagram.PearsonDiagram.Rd:10): Warning: <table> lacks "su     plot_diagram.PearsonDiagram.html:36:1 (plot_diagram.PearsonDiagram.Rd:10): Warning: <table> lacks "summary" attribute
#>    plot_diagram.html:15:44 (plot_diagram.Rd:5): Error: <main> is not recognized!
#>    plot_diagram.html:15:44 (plot_diagram.Rd:5): Warning: discarding unexpected <main>
#>    plot_diagram.html:48:1 (plot_diagram.Rd:13): Warning: discarding unexpected </main>
#>    plot_diagram.html:4:1 (plot_diagram.Rd:5): Warning: <link> inserting "type" attribute
#>    plot_diagram.html:12:1 (plot_diagram.Rd:5): Warning: <script> proprietary attribute "onload"
#>    plot_diagram.html:12:1 (plot_diagram.Rd:5): Warning: <script> inserting "type" attribute
#>    plot_diagram.html:17:1 (plot_diagram.Rd:5): Warning: <table> lacks "summary" attribute
#>    plot_diagram.html:35:1 (plot_diagram.Rd:10): Warning: <table> lacks "summary" attribute
#>    validate_input.html:15:44 (validate_input.Rd:5): Error: <main> is not recognized!
#>    validate_input.html:15:44 (validate_input.Rd:5): Warning: discarding unexpected <main>
#>    validate_input.html:48:1 (validate_input.Rd:13): Warning: discarding unexpected </main>
#>    validate_input.html:4:1 (validate_input.Rd:5): Warning: <link> inserting "type" attribute
#>    validate_input.html:12:1 (validate_input.Rd:5): Warning: <script> proprietary attribute "onload"
#>    validate_input.html:12:1 (validate_input.Rd:5): Warning: <script> inserting "type" attribute
#>    validate_input.html:17:1 (validate_input.Rd:5): Warning: <table> lacks "summary" attribute
#>    validate_input.html:35:1 (validate_input.Rd:10): Warning: <table> lacks "summary" attribute
#> ✔  checking for non-standard things in the check directory
#> ✔  checking for detritus in the temp directory
#>      
#>    See
#>      ‘/private/var/folders/z6/47hkfr6s6d1_cchpbbsz96k40000gn/T/RtmpqMrXZ0/file39b947fa69f4/pearson.diagram.Rcheck/00check.log’
#>    for details.
#>    
#> 
#> ── R CMD check results ────────────────────────────── pearson.diagram 0.1.0 ────
#> Duration: 12.6s
#> 
#> ❯ checking HTML version of manual ... NOTE
#>   Found the following HTML validation problems:
#>   PearsonDiagram.html:15:44 (PearsonDiagram.Rd:5): Error: <main> is not recognized!
#>   PearsonDiagram.html:15:44 (PearsonDiagram.Rd:5): Warning: discarding unexpected <main>
#>   PearsonDiagram.html:44:1 (PearsonDiagram.Rd:16): Warning: discarding unexpected </main>
#>   PearsonDiagram.html:4:1 (PearsonDiagram.Rd:5): Warning: <link> inserting "type" attribute
#>   PearsonDiagram.html:12:1 (PearsonDiagram.Rd:5): Warning: <script> proprietary attribute "onload"
#>   PearsonDiagram.html:12:1 (PearsonDiagram.Rd:5): Warning: <script> inserting "type" attribute
#>   PearsonDiagram.html:17:1 (PearsonDiagram.Rd:5): Warning: <table> lacks "summary" attribute
#>   add_point.html:15:44 (add_point.Rd:5): Error: <main> is not recognized!
#>   add_point.html:15:44 (add_point.Rd:5): Warning: discarding unexpected <main>
#>   add_point.html:56:1 (add_point.Rd:17): Warning: discarding unexpected </main>
#>   add_point.html:4:1 (add_point.Rd:5): Warning: <link> inserting "type" attribute
#>   add_point.html:12:1 (add_point.Rd:5): Warning: <script> proprietary attribute "onload"
#>   add_point.html:12:1 (add_point.Rd:5): Warning: <script> inserting "type" attribute
#>   add_point.html:17:1 (add_point.Rd:5): Warning: <table> lacks "summary" attribute
#>   add_point.html:35:1 (add_point.Rd:10): Warning: <table> lacks "summary" attribute
#>   compare_distributions.PearsonDiagram.html:15:44 (compare_distributions.PearsonDiagram.Rd:5): Error: <main> is not recognized!
#>   compare_distributions.PearsonDiagram.html:15:44 (compare_distributions.PearsonDiagram.Rd:5): Warning: discarding unexpected <main>
#>   compare_distributions.PearsonDiagram.html:58:1 (compare_distributions.PearsonDiagram.Rd:19): Warning: discarding unexpected </main>
#>   compare_distributions.PearsonDiagram.html:4:1 (compare_distributions.PearsonDiagram.Rd:5): Warning: <link> inserting "type" attribute
#>   compare_distributions.PearsonDiagram.html:12:1 (compare_distributions.PearsonDiagram.Rd:5): Warning: <script> proprietary attribute "onload"
#>   compare_distributions.PearsonDiagram.html:12:1 (compare_distributions.PearsonDiagram.Rd:5): Warning: <script> inserting "type" attribute
#>   compare_distributions.PearsonDiagram.html:17:1 (compare_distributions.PearsonDiagram.Rd:5): Warning: <table> lacks "summary" attribute
#>   compare_distributions.PearsonDiagram.html:39:1 (compare_distributions.PearsonDiagram.Rd:12): Warning: <table> lacks "summary" attribute
#>   compare_distributions.html:15:44 (compare_distributions.Rd:5): Error: <main> is not recognized!
#>   compare_distributions.html:15:44 (compare_distributions.Rd:5): Warning: discarding unexpected <main>
#>   compare_distributions.html:52:1 (compare_distributions.Rd:15): Warning: discarding unexpected </main>
#>   compare_distributions.html:4:1 (compare_distributions.Rd:5): Warning: <link> inserting "type" attribute
#>   compare_distributions.html:12:1 (compare_distributions.Rd:5): Warning: <script> proprietary attribute "onload"
#>   compare_distributions.html:12:1 (compare_distributions.Rd:5): Warning: <script> inserting "type" attribute
#>   compare_distributions.html:17:1 (compare_distributions.Rd:5): Warning: <table> lacks "summary" attribute
#>   compare_distributions.html:35:1 (compare_distributions.Rd:10): Warning: <table> lacks "summary" attribute
#>   cpp_calculate_skewness_kurtosis.html:15:44 (cpp_calculate_skewness_kurtosis.Rd:5): Error: <main> is not recognized!
#>   cpp_calculate_skewness_kurtosis.html:15:44 (cpp_calculate_skewness_kurtosis.Rd:5): Warning: discarding unexpected <main>
#>   cpp_calculate_skewness_kurtosis.html:42:1 (cpp_calculate_skewness_kurtosis.Rd:10): Warning: discarding unexpected </main>
#>   cpp_calculate_skewness_kurtosis.html:4:1 (cpp_calculate_skewness_kurtosis.Rd:5): Warning: <link> inserting "type" attribute
#>   cpp_calculate_skewness_kurtosis.html:12:1 (cpp_calculate_skewness_kurtosis.Rd:5): Warning: <script> proprietary attribute "onload"
#>   cpp_calculate_skewness_kurtosis.html:12:1 (cpp_calculate_skewness_kurtosis.Rd:5): Warning: <script> inserting "type" attribute
#>   cpp_calculate_skewness_kurtosis.html:17:1 (cpp_calculate_skewness_kurtosis.Rd:5): Warning: <table> lacks "summary" attribute
#>   cpp_calculate_skewness_kurtosis.html:29:1 (cpp_calculate_skewness_kurtosis.Rd:7): Warning: <table> lacks "summary" attribute
#>   highlight_point_on_hover.html:15:44 (highlight_point_on_hover.Rd:5): Error: <main> is not recognized!
#>   highlight_point_on_hover.html:15:44 (highlight_point_on_hover.Rd:5): Warning: discarding unexpected <main>
#>   highlight_point_on_hover.html:48:1 (highlight_point_on_hover.Rd:13): Warning: discarding unexpected </main>
#>   highlight_point_on_hover.html:4:1 (highlight_point_on_hover.Rd:5): Warning: <link> inserting "type" attribute
#>   highlight_point_on_hover.html:12:1 (highlight_point_on_hover.Rd:5): Warning: <script> proprietary attribute "onload"
#>   highlight_point_on_hover.html:12:1 (highlight_point_on_hover.Rd:5): Warning: <script> inserting "type" attribute
#>   highlight_point_on_hover.html:17:1 (highlight_point_on_hover.Rd:5): Warning: <table> lacks "summary" attribute
#>   highlight_point_on_hover.html:35:1 (highlight_point_on_hover.Rd:10): Warning: <table> lacks "summary" attribute
#>   plot_diagram.PearsonDiagram.html:15:44 (plot_diagram.PearsonDiagram.Rd:5): Error: <main> is not recognized!
#>   plot_diagram.PearsonDiagram.html:15:44 (plot_diagram.PearsonDiagram.Rd:5): Warning: discarding unexpected <main>
#>   plot_diagram.PearsonDiagram.html:49:1 (plot_diagram.PearsonDiagram.Rd:13): Warning: discarding unexpected </main>
#>   plot_diagram.PearsonDiagram.html:4:1 (plot_diagram.PearsonDiagram.Rd:5): Warning: <link> inserting "type" attribute
#>   plot_diagram.PearsonDiagram.html:12:1 (plot_diagram.PearsonDiagram.Rd:5): Warning: <script> proprietary attribute "onload"
#>   plot_diagram.PearsonDiagram.html:12:1 (plot_diagram.PearsonDiagram.Rd:5): Warning: <script> inserting "type" attribute
#>   plot_diagram.PearsonDiagram.html:17:1 (plot_diagram.PearsonDiagram.Rd:5): Warning: <table> lacks "summary" attribute
#>   plot_diagram.PearsonDiagram.html:36:1 (plot_diagram.PearsonDiagram.Rd:10): Warning: <table> lacks "summary" attribute
#>   plot_diagram.html:15:44 (plot_diagram.Rd:5): Error: <main> is not recognized!
#>   plot_diagram.html:15:44 (plot_diagram.Rd:5): Warning: discarding unexpected <main>
#>   plot_diagram.html:48:1 (plot_diagram.Rd:13): Warning: discarding unexpected </main>
#>   plot_diagram.html:4:1 (plot_diagram.Rd:5): Warning: <link> inserting "type" attribute
#>   plot_diagram.html:12:1 (plot_diagram.Rd:5): Warning: <script> proprietary attribute "onload"
#>   plot_diagram.html:12:1 (plot_diagram.Rd:5): Warning: <script> inserting "type" attribute
#>   plot_diagram.html:17:1 (plot_diagram.Rd:5): Warning: <table> lacks "summary" attribute
#>   plot_diagram.html:35:1 (plot_diagram.Rd:10): Warning: <table> lacks "summary" attribute
#>   validate_input.html:15:44 (validate_input.Rd:5): Error: <main> is not recognized!
#>   validate_input.html:15:44 (validate_input.Rd:5): Warning: discarding unexpected <main>
#>   validate_input.html:48:1 (validate_input.Rd:13): Warning: discarding unexpected </main>
#>   validate_input.html:4:1 (validate_input.Rd:5): Warning: <link> inserting "type" attribute
#>   validate_input.html:12:1 (validate_input.Rd:5): Warning: <script> proprietary attribute "onload"
#>   validate_input.html:12:1 (validate_input.Rd:5): Warning: <script> inserting "type" attribute
#>   validate_input.html:17:1 (validate_input.Rd:5): Warning: <table> lacks "summary" attribute
#>   validate_input.html:35:1 (validate_input.Rd:10): Warning: <table> lacks "summary" attribute
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```
