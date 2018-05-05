FROM rocker/r-base

# install R package dependencies
RUN apt-get update \ 
    && apt-get install -y libssl-dev \
    && apt-get install -y libcurl4-openssl-dev \
    && apt-get install -y libxml2-dev \
    ## clean up
    && apt-get clean \ 
    && rm -rf /var/lib/apt/lists/ \ 
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
    
## Install packages from CRAN
RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    googleAuthR \
    rvest \
    formattable \
    ## install Github packages
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds