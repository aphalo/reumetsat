
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

The main Surface UV data product supported is “Surface UV” from the AC
SAF (atmospheric composition) project of EUMETSAT. In addition, “Surface
UV” data from the OMI/Aura project hosted at NASA are supported giving
access to additional variables. In each case, two file formats are
supported. Currently, data are read much faster from HDF5 files than
from the other formats.

An additional surface UV data source with broad, but not global,
geographic coverage is the AC SAF high resolution Europe Surface UV
product (EUV). Support of these date is planned for a future version of
the package.

## Purpose

Surface UV irradiances and doses derived from remotely sensed data
acquired with instruments on satellites are important as ground level
measurements of these variables are rather sparse, with a much lower
spatial resolution than temperature and rainfall. It is frequent to rely
on remote sensing when studying effects of solar UV radiation on human,
animal and plant health at a large scale. This package supports reading
of data from files downloaded from open-access internet servers. These
data have very few restrictions to their use, in most cases only the
requirement to cite the source.

**O3M SAF Offline surface ultraviolet radiation product (OUV).** Data
files from the Surface UV offline data product of AC SAF EUMETSAT can be
downloaded from the server of the Finnish Meteorological Institute (FMI)
[EUMETSAT AC SAF website](https://acsaf.org/). Two different data
“ingestion” (`sUV_read_`) functions cater for two different types of
files: HDF5 files with data on a geographic grid and one file per day,
and text (.txt) files with time series data for a single geographic
location. For more information on these and other related data products,
please, see the [EUMETSAT AC SAF website](https://acsaf.org/).

**OMI/Aura Surface UVB Irradiance and Erythemal Dose Daily L3 Global
Gridded 1.0 degree x 1.0 degree V3 (OMUVBd).** The OMI/Aura Surface UV
offline data are available through NASA and can be downloaded from the
[NASA EARTHDATA
website](https://disc.gsfc.nasa.gov/datasets/OMUVBd_003/summary?keywords=OMUVBd)
as NetCDF4 files with the possibility of sub-setting before download.
HDF5 files are also available for download without possibility of
sub-setting. In both cases the data are provided as one file per day on
a geographic grid basis. Two different data “ingestion” (`sUV_read_`)
functions cater for the two different types of files. The OMI/Aura
Surface UV offline data are available through NASA and can be downloaded
from the [NASA EARTHDATA
website](https://disc.gsfc.nasa.gov/datasets/OMUVBd_003/summary?keywords=OMUVBd).

All functions can read one or more files in a single call. Irrespective
of the number of files read, all `sUV_read_` functions return a single
combined data frame object, with locations and time coordinates in long
form. Additional query functions are provided to extract information
from files, when possible, without reading them in whole.

## Installation

Installation of the most recent stable version from CRAN:

``` r
# NOT YET IN CRAN
# install.packages("surfaceuv")
```

Installation of the current unstable version from the R-Universe
CRAN-like repository can be done with the code below. Including the URLs
of the CRAN and Bioconductor repositories ensures that dependencies are
installed automatically.

``` r
install.packages('surfaceuv', 
                 repos = c('https://aphalo.r-universe.dev', 
                           'https://cloud.r-project.org',
                           "https://bioconductor.org/packages/3.19/bioc"))
```

Installation of the current unstable version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("aphalo/surfaceuv")
```

## Documentation

HTML documentation is available at
(<https://docs.r4photobiology.info/surfaceuv/>), including a *User
Guide* that is part of the package, and an on-line-only set of
instructions on how to download files from the server of the Finnish
Meteorological Institute (FMI) [EUMETSAT AC SAF
website](https://acsaf.org/).

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
#>   Aphalo P (2024). _surfaceuv: Solar Ultraviolet at Ground Surface Data
#>   Import_. R package version 0.1.1.9000,
#>   <https://docs.r4photobiology.info/surfaceuv/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {surfaceuv: Solar Ultraviolet at Ground Surface Data Import},
#>     author = {Pedro J. Aphalo},
#>     year = {2024},
#>     note = {R package version 0.1.1.9000},
#>     url = {https://docs.r4photobiology.info/surfaceuv/},
#>   }
```

## License

The package includes data example files redistributed in folder
`extdata` that are © 2024 AC SAF project of EUMETSAT and used by
courtesy of the AC SAF project of the EUMETSAT. If reused or
redistributed a note similar to this must be included, but it is
recommended to download them directly from the original source.

The package includes data example files redistributed in folder
`extdata` that contain modified Copernicus Sentinel data \[2024\]. If
reused or redistributed a note similar to this must be included, but it
is recommended to download them directly from the original source.

Package code and documentation © 2024 Pedro J. Aphalo
(<pedro.aphalo@helsinki.fi>). Released under the GPL, version 2 or
greater. This software carries no warranty of any kind.
