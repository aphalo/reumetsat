# reumetsat 0.1.0

* Rename `read_AC_SAFT_hdf5()` into `read_AC_SAFT_UV_hdf5()` as non-UV data
products use a different format for storing the data.
* Make date extraction from file names less dependent on the non-date-encoding 
portion of the names `read_AC_SAFT_UV_hdf5()`.

# reumetsat 0.1.0

* Read HDF5 data files from AC SAFT FMI server into data frames
in function `read_AC_SAFT_hdf5()`. These files contain data on a geographic grid.
* Read text data files from AC SAFT FMI server into data frames
in function `read_AC_SAFT_txt()`. These files contain a time series of data for
a single geographic location.
