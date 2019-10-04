## R CMD check results

### Local server

* Linux Debian 4.9.110-3+deb9u2, R 3.6.0: OK

### Appveyor-ci

* Windows Server 2012 R2 x64 (build 9600), R 3.6.1: OK

### travis-ci

* Ubuntu 16.04.6 LTS, R devel: ERROR  
  * \$ Rscript -e 'sessionInfo()'  
    Error in if (version[, 1:2] != version) return(sprintf("version '%s' must have two components, e.g., '3.7'",  : argument is of length zero  
    Calls: options ... <Anonymous> -> .version_validate -> .version_validity  
    Execution halted

* Ubuntu 16.04.6 LTS, R release: OK

* Ubuntu 16.04.6 LTS, R 3.5: OK

* Ubuntu 16.04.6 LTS, R 3.6: OK

### R-hub builder 

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit: OK

* Fedora Linux, R-devel, clang, gfortran: OK

* Ubuntu Linux 16.04 LTS, R-release, GCC: ERROR  
  * checking package dependencies ... ERROR  
    Package suggested but not available: 'GEOquery'

### win-builder

* R devel: 1 NOTE  
  * checking top-level files ... NOTE  
    Non-standard file/directory found at top level:  
      'cran-comments.md'


## revdepcheck results

* OK: 0
* BROKEN: 0
