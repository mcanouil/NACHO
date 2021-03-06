## Fix CRAN Package Check Results for Package NACHO v1.0.2

+ Fix functions deprecation warnings (from ggplot2/dplyr).

+ Refine checks for files download in tests to ensure tests continue 
    when distant server is initially down or become unavailable while downloading files.

## R CMD check results

### Local server (`devtools::check(remote = TRUE, manual = TRUE)`)

* Linux Debian 10 (buster), R-4.0.3: OK

### win-builder (`devtools::check_win_devel()`)

* R-devel: 1 NOTE
  Found the following (possibly) invalid URLs:
    URL: https://mcanouil.shinyapps.io/NACHO_data/
      From: README.md
      Status: Error
      Message: libcurl error code 35:
        	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).

### R-hub builder (`rhub::check_for_cran()`)

* Fedora Linux, R-devel, clang, gfortran: OK

* Ubuntu Linux 16.04 LTS, R-release, GCC: OK

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit: OK

### Github Action

* macOS-latest (release): OK

* windows-latest (release): OK

* windows-latest (3.6): OK

* ubuntu-16.04 (devel): OK

* ubuntu-16.04 (release): OK

* ubuntu-16.04 (oldrel): OK
