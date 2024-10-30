
# surfaceuv

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-last-release/surfaceuv)](https://cran.r-project.org/package=surfaceuv)
<!-- [![cran checks](https://badges.cranchecks.info/worst/surfaceuv.svg)](https://cran.r-project.org/web/checks/check_results_surfaceuv.html) -->
[![photobiologyInOut status
badge](https://aphalo.r-universe.dev/badges/surfaceuv)](https://aphalo.r-universe.dev/surfaceuv)
[![R-CMD-check](https://github.com/aphalo/surfaceuv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aphalo/surfaceuv/actions/workflows/R-CMD-check.yaml)
[![Documentation](https://img.shields.io/badge/documentation-surfaceuv-informational.svg)](https://docs.r4photobiology.info/surfaceuv/)
<!-- badges: end -->
<!-- [![doi](https://img.shields.io/badge/doi-10.32614/CRAN.package.surfaceuv-blue.svg)](https://doi.org/10.32614/CRAN.package.surfaceuv) -->

## Development stage and plans

Currently the Surface UV data product supported is “Surface UV” from the
AC SAF (atmospheric composition) project of EUMETSAT. These functions
are thoroughly tested and documented, but as code that is only a few
days old, may still need some further improvements.

Two other surface UV data sources with broad geographic coverage will be
supported in the future: OMI Surface UV and the AC SAF high resolution
Surface UV products.

## Purpose

Data files from the Surface UV offline data product of AC SAF EUMETSAT
can be downloaded from the server of the Finnish Meteorological
Institute (FMI). Two different data “ingestion” (`read_`) functions
cater for two different types of files: HDF5 files with data on a
geographic grid and one file per day, and text (.txt) files with time
series data for a single geographic location. Both functions can read
one or more files. Irrespective of the number of files read, both
functions return a single combined data frame object. Additional query
functions are provided to extract information from the files without
reading them in whole. For more information of these and other related
data products, please, see the [EUMETSAT AC SAF
website](https://acsaf.org/).

## Installation

Installation of the most recent stable version from CRAN:

``` r
# install.packages("surfaceuv")
```

Installation of the current unstable version from R-Universe CRAN-like
repository:

``` r
install.packages('surfaceuv', 
                 repos = c('https://aphalo.r-universe.dev', 
                           'https://cloud.r-project.org'))
```

Installation of the current unstable version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("aphalo/surfaceuv")
```

## Documentation

HTML documentation is available at
(<https://docs.r4photobiology.info/surfaceuv/>), including a *User
Guide*.

## Contributing

Pull requests, bug reports, and feature requests are welcome at
(<https://github.com/aphalo/surfaceuv>). Contribution of example data
files that could be supported in future versions will be very much
appreciated.

## Citation

If you use this package to produce scientific or commercial
publications, please cite according to:

``` r
citation("surfaceuv")
#> To cite package 'surfaceuv' in publications use:
#> 
#>   Aphalo P (2024). _surfaceuv: EUMETSAT Offline Data Products_. R
#>   package version 0.1.0.9000,
#>   <https://docs.r4photobiology.info/surfaceuv/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {surfaceuv: EUMETSAT Offline Data Products},
#>     author = {Pedro J. Aphalo},
#>     year = {2024},
#>     note = {R package version 0.1.0.9000},
#>     url = {https://docs.r4photobiology.info/surfaceuv/},
#>   }
```

## License

The data example files redistributed in folder ‘extdata’ are © 2024 AC
SAF project of EUMETSAT and used by courtesy of the AC SAF project of
the EUMETSAT. If reused of republished a note similar to this must be
included.

© 2024 Pedro J. Aphalo (<pedro.aphalo@helsinki.fi>). Released under the
GPL, version 2 or greater. This software carries no warranty of any
kind.
