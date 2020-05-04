setwd("D:\\LSE\\MY459 Quantitative Text Analysis\\project\\jpg\\")
library(stringr)
# tesseract ocr
library(tesseract)
# set up ocr engine
engine <- tesseract(language = 'chi_sim')
# making random sample
set.seed(10)
index <- sample(1:dim(article.web)[1],10)
sample <- article.web[index,]
sample$ocr <- NA
filename <- paste(sample$id,'.jpg',sep='')
for (i in 1:10){
  sample$ocr[i] <- ocr(filename[i], engine = engine)
}
sample$ocr <- str_remove_all(sample$ocr,'\n')
sample$ocr <- str_remove_all(sample$ocr,regex("[a-zA-Z]+"))
sample$ocr <- str_remove_all(sample$ocr,' ')
sample$content <- str_remove_all(sample$content,'\n')
sample$content <- str_remove_all(sample$content,regex("[a-zA-Z]+"))
sample$content <- str_remove_all(sample$content,' ')
sample$ocr.seg <- NA
sample$content.seg <- NA
for (i in 1:10){
  sample$content.seg[i] <- paste(stringi::stri_split_boundaries(sample$content[i])[[1]], collapse = ' ')
  sample$ocr.seg[i] <- paste(stringi::stri_split_boundaries(sample$ocr[i])[[1]], collapse = ' ')
}
library(quanteda)
dist <- vector()
for (i in 1:10){
  doc <- c(sample$content.seg[i],sample$ocr.seg[i])
  d.w <- dfm_weight(dfm(doc),scheme = 'prop')
  dist[i] <- textstat_dist(d.w, method="euclidean")[2,1]
}
