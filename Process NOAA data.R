#path <- getwd()
#file <- "repdata_data_StormData.csv.bz2"
#zippath <- paste(path, file, sep = "/")
#   unzip(zippath)


if (!exists("repdata")) {
    repdata <- read.csv2("repdata_data_StormData.csv.bz2", header = TRUE, nrow = 10, sep = ",")
}

repdata <- read.csv2("repdata_data_StormData.csv.bz2", header = TRUE, nrow = 10, sep = ",")
