FROM rocker/r-base

## Install packages from CRAN
RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    googleAuthR \
    rvest \
    formattable \
    ## install Github packages
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds