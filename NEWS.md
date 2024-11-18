# surfaceuv 0.1.2

* Avoid dependency on 'dplyr' and 'tidync', and consequently also on all the 'tidyverse' packages.
* Temporarily add utils for selection of nearest points and regions from a 2D grid.

# surfaceuv 0.1.1

* Rename all functions (twice, only new names shown below). 
* Read NetCDF4 files from NASA server with OMI/Aura "surface UV" data into data frames in function `sUV_read_OMUVBd_nc4()`. These files contain data on a geographic grid.
* Read HDF5 files from NASA server with OMI/Aura "surface UV" data into data frames in function `sUV_read_OMUVBd_he5()`. These files contain data on a geographic grid.
* Make date extraction from file names less dependent on the non-date-encoding portion of the names.
* Add query functions that support vectors of file names as arguments, returning variable names, grid coordinates, or dates available in files. 

# surfaceuv 0.1.0

* Read HDF5 data files from AC SAF FMI server into data frames in function `sUV_read_OUV_hdf5()`. These files contain data on a geographic grid.
* Read text data files from AC SAF FMI server into data frames in function `sUV_read_OUV_txt()`. These files contain a time series of data for a single geographic location.
