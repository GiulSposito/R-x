install.packages('devtools')



devtools::install_url(
  'https://github.com/catboost/catboost/releases/download/v0.8.1/catboost-R-Windows-0.8.1.tgz', args = c("--no-multiarch"))
