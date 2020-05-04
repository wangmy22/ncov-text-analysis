library(tidyverse)
library(quanteda)
setwd("D:\\LSE\\MY459 Quantitative Text Analysis\\project\\")

load('data.Rdata')
# for those cannot ocr, use archive website
index <- c(220,741,1274,2515,2516,2517,2884,3153,3281,3366,3443,3640,3730,3733,3734,3737,3812,3894,4072,4074,4126)
ar <- vector()
for (i in 1:length(index)){
  ar[i] <- data$archive[which(data$id==as.character(index[i]))]
}

load('ocr.RData')
ocr_index <- ocr$id
setwd('D:\\LSE\\MY459 Quantitative Text Analysis\\project\\ocr.2')
ocr$id <- sort(as.numeric(ocr$id))
data$ocr = F
for (i in 1:length(ocr$id)){
  ocr$content[i] <- try(readtext::readtext(paste(ocr$id[i],'.txt',sep = ''),encoding = 'utf-8')$text)
  data$content[which(data$id==ocr$id[i])] <- try(readtext::readtext(paste(ocr$id[i],'.txt',sep = ''),encoding = 'utf-8')$text)
  data$ocr[which(data$id==ocr$id[i])] <- T
  cat('now',ocr$id[i])
}

data$whether_archive <- F
for (i in 1:length(index)){
  data$whether_archive[which(data$id==index[i])] <- T
  data$ocr[which(data$id==index[i])] <- F
}
data$whether_json <- F

data$whether_json[which(data$content==data$content_json)] <- T 
summary(data$whether_json) # 148

summary(data$webscraping) # 3710
summary(data$ocr) # 354
summary(data$whether_archive) # 21

setwd("D:\\LSE\\MY459 Quantitative Text Analysis\\project\\data")
save(data,file  ='final_raw.RData')

data$source <- NA
data <- data %>%
  mutate(source = ifelse(webscraping==T,'web',source)) %>%
  mutate(source = ifelse(whether_json==T,'json',source)) %>%
  mutate(source = ifelse(ocr==T,'ocr',source)) %>%
  mutate(source = ifelse(whether_archive==T,'archive',source))

data <- data %>%
  select(-c(webscraping,content_json,replace,ocr,whether_archive,whether_json))

save(data, file = 'final.RData')
