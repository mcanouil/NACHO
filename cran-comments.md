## R CMD check results

### Local server (`devtools::check(remote = TRUE, manual = TRUE)`)

* Linux Debian 10 (buster), R-4.0.0: OK

### win-builder (`devtools::check_win_devel()`)

* R-devel: 1 NOTE  
Found the following (possibly) invalid URLs:
  URL: https://codecov.io/gh/mcanouil/NACHO
    From: README.md
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
  URL: https://mcanouil.shinyapps.io/NACHO_data/
    From: README.md
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).

### R-hub builder (`rhub::check_for_cran()`)

* Fedora Linux, R-devel, clang, gfortran: OK

* Ubuntu Linux 16.04 LTS, R-release, GCC: OK

* Windows Server 2008 R2 SP1, R-devel, 32/64 bit: OK

### Appveyor-ci

* Windows Server 2012 R2 x64 (build 9600), R-4.0.0: OK

### travis-ci

* Ubuntu 16.04.6 LTS, R-devel: OK

* Ubuntu 16.04.6 LTS, R-release: OK

* Ubuntu 16.04.6 LTS, R-3.5: OK

* Ubuntu 16.04.6 LTS, R-3.6: OK

* Ubuntu 16.04.6 LTS, R-4.0: OK