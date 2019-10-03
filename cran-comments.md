## Test environments

* Linux Debian 4.9.110-3+deb9u2 install: R 3.6.0
* win-builder: R-devel
* travis-ci: R 3.5, R 3.6, R-devel, R-release

## R CMD check results

0 errors | 0 warnings | 0 notes

### travis-ci results

* R devel: errored
  - $ Rscript -e 'sessionInfo()'  
    Error in if (version[, 1:2] != version) return(sprintf("version '%s' must have two components, e.g., '3.7'",  : argument is of length zero  
    Calls: options ... <Anonymous> -> .version_validate -> .version_validity  
    Execution halted
* R release: passed
* R 3.5: passed
* R 3.6: passed

## revdepcheck results

* OK: 0
* BROKEN: 0