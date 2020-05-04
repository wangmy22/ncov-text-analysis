library(tidyverse)
library(rvest)
setwd('D:\\LSE\\MY459 Quantitative Text Analysis\\project')
load('wechat.Rdata')
wechat_content <- vector()
for (i in seq(3101,3509)){
  web <- read_html(wechat$url[i],encoding = 'UTF-8')
  Sys.sleep(runif(1))
  wechat_content[i] <- web %>%
    html_nodes('span') %>%
    html_text() %>%
    paste(collapse = ' ')
  cat('now:', i, '\n')
  
}
wechat$content[3101:3509] <- wechat_content[3101:3509]
save(wechat, file = 'wechat.RData')
