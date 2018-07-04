options(exifr.perlpath="C:\\Perl\\perl\\bin\\perl.exe")
library(exifr)
library(tidyverse)
library(lubridate)

# london files
path <- "C:/Users/gsposito/Pictures/London/"
files <- dir(path, pattern = ".*08488\\.JPG", full.names = T)

# checking date and time in the exif
exifr::read_exif(files, tags=c("DateTimeOriginal", "CreateDate", "SonyDateTime","ModifyDate")) %>%
  glimpse()

files <- dir(path, pattern = "DSC[0-9]+\\.JPG$", full.names = T)

# changing
exifr::exiftool_call(args = list(
  shQuote("-DateTimeOriginal+=0:0:0 4:0:0"),
  shQuote("-CreateDate+=0:0:0 4:0:0"),
  shQuote("-SonyDateTime+=0:0:0 4:0:0"),
  shQuote("-ModifyDate+=0:0:0 4:0:0")),
  fnames = files)


# london GPS setting
gps="51.5287352 -0.3817832"
gps.lat=str_split(gps," ")[[1]][1]
gps.lon=str_split(gps," ")[[1]][2]

# GPS tags to london
# $ GPSAltitude              <int> 7
# $ GPSDateTime              <chr> "2018:06:05 11:51:22Z"
# $ GPSLatitude              <dbl> 51.49958
# $ GPSLongitude             <dbl> -0.1290806
# $ GPSPosition              <chr> "51.4995805555556 -0.129080555555556"

# check values before change
exifr::read_exif(files[1:5]) %>%
  glimpse()

# set GPS coordinates
exifr::exiftool_call(args = list(
  paste0("-GPSLatitude=",shQuote(gps.lat)),
  paste0("-GPSLongitude=",shQuote(gps.lon)),
  paste0("-GPSPosition=",shQuote(gps)),
  "-GPSLongitudeRef=W",
  "-GPSLatitudeRef=North"),  
  fnames = files)

# check values after change
exifr::read_exif(files[1:5], tags=c("GPSLatitude","GPSLongitude","GPSPosition")) %>%
  glimpse()

# checking timestamp
exifr::read_exif("C:\\Users\\gsposito\\Pictures\\IMG-0996.JPG") %>%
  select( CreateDate, starts_with("GPS") ) %>%
  glimpse()

# timestamp GPS tags  
# $ GPSTimeStamp         <chr> "11:51:22"
# $ GPSDateStamp         <chr> "2018:06:05"
# $ GPSDateTime          <chr> "2018:06:05 11:51:22Z"

# reading allfiles timestap
exifr::read_exif(files, tags=c("CreateDate")) %>%
  mutate( CreateDateDt  = ymd_hms(CreateDate) ) %>%
  mutate( GPSDateTimeDt = CreateDateDt - hours(1),
          GPSDateTime   = format(GPSDateTimeDt,"%Y:%m:%d %H:%M:%SZ"),
          GPSDateStamp = format(GPSDateTimeDt,"%Y:%m:%d"),
          GPSTimeStamp  = format(GPSDateTimeDt, "%H:%M:%S") ) %>%
  select( SourceFile, CreateDateDt, GPSTimeStamp, GPSDateStamp, GPSDateTime) -> newGPSTimeStamps

for(i in 1:nrow(newGPSTimeStamps)){
  newGPSts <- newGPSTimeStamps[i,]  
  exifr::exiftool_call(args = list(
    shQuote(paste0("-GPSDateTime=", newGPSts$GPSDateTime)),
    shQuote(paste0("-GPSDateStamp=",newGPSts$GPSDateStamp)),
    shQuote(paste0("-GPSTimeStamp=",newGPSts$GPSTimeStamp))),
    fnames = newGPSts$SourceFile)
}
