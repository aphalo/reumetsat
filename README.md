
# reumetsat

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-last-release/reumetsat)](https://cran.r-project.org/package=reumetsat)
<!-- [![cran checks](https://badges.cranchecks.info/worst/reumetsat.svg)](https://cran.r-project.org/web/checks/check_results_reumetsat.html) -->
[![photobiologyInOut status
badge](https://aphalo.r-universe.dev/badges/reumetsat)](https://aphalo.r-universe.dev/reumetsat)
[![R-CMD-check](https://github.com/aphalo/reumetsat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aphalo/reumetsat/actions/workflows/R-CMD-check.yaml)
[![Documentation](https://img.shields.io/badge/documentation-reumetsat-informational.svg)](https://docs.r4photobiology.info/reumetsat/)
<!-- badges: end -->
<!-- [![doi](https://img.shields.io/badge/doi-10.32614/CRAN.package.reumetsat-blue.svg)](https://doi.org/10.32614/CRAN.package.reumetsat) -->

## Purpose

Package ‘reumetsat’ currently only supports reading data files from the
Surface UV offline data product of AC SAF EUMETSAT downloaded from the
server of the Finnish Meteorological Institute (FMI). Two different
functions cater for two different types of files: HDF5 files with data
on a geographic grid and one file per day, and text (.txt) files with
time series data for a single geographic location. Both functions can
read one or more files. Irrespective of the number of files they return
a single combined data frame object.

For more information of these and other data products, please, see the
[EUMETSAT AC SAF website](https://acsaf.org/).

## Installation

Installation of the most recent stable version from CRAN:

``` r
# install.packages("photobiologyInOut")
```

Installation of the current unstable version from R-Universe CRAN-like
repository:

``` r
# install.packages('reumetsat', 
#                  repos = c('https://aphalo.r-universe.dev', 
#                            'https://cloud.r-project.org'))
```

Installation of the current unstable version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("aphalo/reumetsat")
```

## Documentation

HTML documentation is available at
(<https://docs.r4photobiology.info/reumetsat/>), including a *User
Guide*.

## Contributing

Pull requests, bug reports, and feature requests are welcome at
(<https://github.com/aphalo/reumetsat>). Contribution of example data
files that could be supported in future versions will be very much
appreciated.

## Citation

If you use this package to produce scientific or commercial
publications, please cite according to:

``` r
citation("reumetsat")
#> To cite package 'reumetsat' in publications use:
#> 
#>   Aphalo P (2024). _reumetsat: EUMETSAT Offline Data Products_. R
#>   package version 0.1.0, <https://docs.r4photobiology.info/reumetsat/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {reumetsat: EUMETSAT Offline Data Products},
#>     author = {Pedro J. Aphalo},
#>     year = {2024},
#>     note = {R package version 0.1.0},
#>     url = {https://docs.r4photobiology.info/reumetsat/},
#>   }
```

## License

The data example files redistributed in ‘extdata’ are © 2024 AC SAF
project of EUMETSAT and used by courtesy of the AC SAF project of the
EUMETSAT. If reused of republished a note similar to this must be
included.

© 2024 Pedro J. Aphalo (<pedro.aphalo@helsinki.fi>). Released under the
GPL, version 2 or greater. This software carries no warranty of any
kind.
