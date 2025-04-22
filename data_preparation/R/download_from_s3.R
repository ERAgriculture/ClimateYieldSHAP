library(s3fs)
library(ERAg)
library(ERAgON)
# devtools::install_github("https://github.com/EiA2030/ERAg",dependencies = T,build_vignettes = TRUE)
# devtools::install_github("https://github.com/EiA2030/ERAgON",
#                          dependencies = T,
#                          build_vignettes = TRUE)

library(s3fs)
library(arrow)
era_dirs = "s3://digital-atlas/era/geodata"
s3<-s3fs::S3FileSystem$new(anonymous = T)
# s3_file<-s3$dir_ls(era_dirs$era_geodata_s3)

files <- s3$dir_ls('digital-atlas/era/geodata/')

print(files)

ds <- arrow::open_dataset(files[8])
# temp_file <- tempfile(fileext = ".parquet")
# s3$download(files[8], temp_file)
# ds <- arrow::read_parquet(temp_file)

local_file<-"../databases/ERA/POWER.CHIRPS.parquet"
s3$file_download(files[8],local_file)

df <- read_parquet("downloads/ERA/POWER.CHIRPS.parquet")
