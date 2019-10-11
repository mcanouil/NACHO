## Fix CRAN Package Check Results for Package NACHO (0.6.0)

* Now functions are imported explicitly in roxygen documentation
  * Version: 0.6.0  
      Check: dependencies in R code  
      Result: NOTE  
          Namespaces in Imports field not imported from: 'knitr' 'sessioninfo'  
          All declared Imports should be used.  
    Flavors: **r-devel-linux-x86_64-fedora-clang**, **r-devel-linux-x86_64-fedora-gcc**, **r-patched-solaris-x86**

* Add missing `SystemRequirements` field (and check for pandoc availability in tests)
  * Version: 0.6.0  
      Check: tests  
      Result: ERROR  
          pandoc version 1.12.3 or higher is required and was not found (see the help page ?rmarkdown::pandoc_available).  
          1: render(nacho_object = GSE74821) at testthat/test-render.R:2  
          2: rmarkdown::render(input = temp_file, output_file = output_file, output_dir = output_dir,  
          encoding = "UTF-8", quiet = TRUE, params = list(nacho_object = nacho_object))  
          3: pandoc_available(required_pandoc, error = TRUE)  
          4: stop(paste(msg, collapse = " "), call. = FALSE)  
    Flavor: **r-patched-solaris-x86**

## R CMD check results

### Local server (`devtools::check()`)

* Linux Debian 4.9.110-3+deb9u2, R-3.6.1: OK

### Appveyor-ci

* Windows Server 2012 R2 x64 (build 9600), R-3.6.1: OK

### travis-ci

* Ubuntu 16.04.6 LTS, R-devel: OK

* Ubuntu 16.04.6 LTS, R-release: OK

* Ubuntu 16.04.6 LTS, R-3.5: OK

* Ubuntu 16.04.6 LTS, R-3.6: OK

### win-builder (`devtools::check_win_devel()`)

* R-devel: OK

### R-hub builder (`rhub::check()`)

#### OK (or NOTE for "Days since last update")

* Oracle Solaris 10, x86, 32 bit, R-patched: OK

* Windows Server 2008 R2 SP1, R-release, 32/64 bit: OK

* macOS 10.11 El Capitan, R-release: OK

#### NOTE (Bioconductor dependencies)

* Debian Linux, R-devel, GCC: NOTE
    * Package suggested but not available for checking: 'GEOquery'
    
* Debian Linux, R-patched, GCC: NOTE
    * Package suggested but not available for checking: 'GEOquery'

* Debian Linux, R-release, GCC: NOTE
    * Package suggested but not available for checking: 'GEOquery'

* Fedora Linux, R-devel, clang, gfortran: NOTE
    * Package suggested but not available for checking: 'GEOquery'

* Fedora Linux, R-devel, GCC: NOTE
    * Package suggested but not available for checking: 'GEOquery'

* Ubuntu Linux 16.04 LTS, R-release, GCC: NOTE
    * Package suggested but not available for checking: 'GEOquery'
    
#### ERROR

**NOTE:** R development version have some recent issue which was fixed in the virtual machine / Docker containers in Travis and Appveyor, but not yet in R-hub platforms.
    
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit: ERROR
    * Packages required but not available:  
      'tibble', 'dplyr', 'tidyr', 'shiny', 'scales', 'ggplot2',  
      'ggbeeswarm', 'ggrepel', 'ggpubr', 'gtools', 'knitr', 'rmarkdown'  


