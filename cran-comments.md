## Fix CRAN Package Check Results for Package NACHO (0.6.0)

* Now functions are imported explicitly in roxygen documentation
  * Version: 0.6.0  
    Check: dependencies in R code  
    Result: NOTE  
        Namespaces in Imports field not imported from: ‘knitr’ ‘sessioninfo’  
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

### Local server

* Linux Debian 4.9.110-3+deb9u2, R-3.6.1: OK

### Appveyor-ci

* Windows Server 2012 R2 x64 (build 9600), R-3.6.1: OK

### travis-ci

* Ubuntu 16.04.6 LTS, R-devel: OK

* Ubuntu 16.04.6 LTS, R-release: OK

* Ubuntu 16.04.6 LTS, R-3.5: OK

* Ubuntu 16.04.6 LTS, R-3.6: OK

### win-builder

* R-devel: OK

### R-hub builder 

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit: OK

* Fedora Linux, R-devel, clang, gfortran: ERROR  
  * checking package dependencies ... ERROR  
    Package suggested but not available: 'GEOquery'

* Ubuntu Linux 16.04 LTS, R-release, GCC: ERROR  
  * checking package dependencies ... ERROR  
    Package suggested but not available: 'GEOquery'

* Oracle Solaris 10, x86, 32 bit, R-patched: OK

* macOS 10.11 El Capitan, R-release: OK
