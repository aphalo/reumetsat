# surfaceuv 0.1.1

* Read NetCDF4 files from NASA server with OMI/Aura "surface UV" data into data frames in function `read_OMI_AURA_UV_nc4()`. These files contain data on a geographic grid.
* Rename functions adding `UV_`, e.g., `read_AC_SAF_hdf5()` into `read_AC_SAF_UV_.hdf5()`, as UV data products are the only ones supported.
* Make date extraction from file names less dependent on the non-date-encoding portion of the names.
* Add query functions that support vectors of file names as arguments, returning variable names, grid coordinates, or dates available in files. 

# surfaceuv 0.1.0

* Read HDF5 data files from AC SAF FMI server into data frames in function `read_AC_SAF_hdf5()`. These files contain data on a geographic grid.
* Read text data files from AC SAF FMI server into data frames in function `read_AC_SAF_txt()`. These files contain a time series of data for a single geographic location.
