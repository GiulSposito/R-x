options(exifr.perlpath="C:\\Perl\\perl\\bin\\perl.exe")
library(exifr)
library(tidyverse)


fn <- "C:\\Users\\gsposito\\Pictures\\London\\IMG-0959.JPG"

meta <- read_exif(fn)
glimpse(meta)


fn <- "C:\\Users\\gsposito\\Pictures\\Barcelona\\IMG-0959.JPG" #, full.names = T)


metadata <- read_exif(fn)

metadata %>% glimpse()
metadata %>% select(FileName, DateTimeOriginal, CreateDate, SonyDateTime)

exiftool_cmd <- paste("exiftool -alldates=",shQuote(date_exif[which(date_exif[,4]%in%latlon_exif[i,4]),8])," ","./nodejpg/",latlon_exif[i,4],".jpg",sep='')

delta <- ""

exifr::exiftool_call(args = list(
  shQuote("-DateTimeOriginal+=0:0:0 5:0:0"),
  shQuote("-CreateDate+=0:0:0 5:0:0"),
  shQuote("-SonyDateTime+=0:0:0 5:0:0"),
  shQuote("-ModifyDate+=0:0:0 5:0:0")),
  fnames = fn)

london="51.5287352 -0.3817832"
london.lat=str_split(london," ")[[1]][1]
london.lon=str_split(london," ")[[1]][2]


exifr::exiftool_call(args = list(
  shQuote("-DateTimeOriginal+=0:0:0 5:0:0"),
  shQuote("-CreateDate+=0:0:0 5:0:0"),
  shQuote("-SonyDateTime+=0:0:0 5:0:0"),
  shQuote("-ModifyDate+=0:0:0 5:0:0")),
  fnames = fn)
