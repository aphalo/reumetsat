library(reumetsat)

old.wd <- setwd("./inst-not")

files <- list.files(pattern = "HDF5$")

read_AC_SAFT_hdf5(files, verbose = TRUE) -> zz
read_AC_SAFT_hdf5(files[1], verbose = TRUE) -> zz
colnames(zz)

read_AC_SAFT_hdf5(files, verbose = FALSE) -> zz
read_AC_SAFT_hdf5(files[2], verbose = FALSE) -> zz
colnames(zz)

read_AC_SAFT_hdf5(files,
                  vars.to.read = c("DailyDoseUva", "DailyDoseUvb"),
                  verbose = TRUE) -> zz
colnames(zz)
read_AC_SAFT_hdf5(files[1],
                  vars.to.read = c("DailyDoseUva", "DailyDoseUvb"),
                  verbose = TRUE) -> zz
colnames(zz)

read_AC_SAFT_hdf5(files,
                  vars.to.read = c("DailyDoseUva", "DailyDoseUvb", "Test"),
                  verbose = TRUE) -> zz
colnames(zz)
summary(zz)

read_AC_SAFT_hdf5(files,
                  vars.to.read = "Test",
                  verbose = TRUE) -> zz
colnames(zz)
summary(zz)
setwd(old.wd)
