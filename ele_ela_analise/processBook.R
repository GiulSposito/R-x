# script to download a pdf and extract the text
library(tidyverse)
library(pdftools)

books <- readRDS("./ele_ela_analise/data/book_links.rds")


abook <- books[24,]


tfile <- tempfile()
download.file(abook$link, tfile)
txt <- pdf_text("./ele_ela_analise/data/bv000060.pdf")
txt <- pdf_text("./ele_ela_analise/data/me003427.pdf")
str(txt)

library(tm)

read <- readPDF(control = list(text = "-layout"))
document <- Corpus(URISource(tfile), readerControl = list(reader = read))
doc <- content(document[[1]])
head(doc)
