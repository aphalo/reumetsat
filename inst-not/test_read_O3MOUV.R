library(surfaceuv)

old.wd <- setwd("./inst-not")

files <- list.files(pattern = "HDF5$")

# test reading of files

sUV_read_OUV_hdf5(files, verbose = TRUE) -> zz
sUV_read_OUV_hdf5(files[1], verbose = TRUE) -> zz
colnames(zz)

sUV_read_OUV_hdf5(files, verbose = FALSE) -> zz
sUV_read_OUV_hdf5(files[2], verbose = FALSE) -> zz
colnames(zz)

sUV_read_OUV_hdf5(files,
                  vars.to.read = c("DailyDoseUva", "DailyDoseUvb"),
                  verbose = TRUE) -> zz
colnames(zz)
sUV_read_OUV_hdf5(files[1],
                  vars.to.read = c("DailyDoseUva", "DailyDoseUvb"),
                  verbose = TRUE) -> zz
colnames(zz)

sUV_read_OUV_hdf5(files,
                  vars.to.read = c("DailyDoseUva", "DailyDoseUvb", "Test"),
                  verbose = TRUE) -> zz
colnames(zz)
summary(zz)

sUV_read_OUV_hdf5(files,
                  vars.to.read = "Test",
                  verbose = TRUE) -> zz
colnames(zz)
summary(zz)

setwd(old.wd)
