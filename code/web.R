library(tidyverse)
library(rvest)
setwd('D:\\LSE\\MY459 Quantitative Text Analysis\\project')
memory <- read.csv('data.csv',sep = ',', encoding = 'UTF-8', header = T,stringsAsFactors = F)
summary(startsWith(memory$url, 'https://mp.weixin.qq.com/'))
wechat_index <- startsWith(memory$url, 'https://mp.weixin.qq.com/')
wechat <- memory[wechat_index,]
wechat$content <- NA
other <- memory[wechat_index==F,]

# web scraping for wechat content
wechat_content <- vector()
for (i in seq(3001,3010)){
  web <- read_html(wechat$url[i],encoding = 'UTF-8')
  Sys.sleep(runif(1))
  wechat_content[i] <- web %>%
    html_nodes('span') %>%
    html_text() %>%
    paste(collapse = ' ')
    cat('now:', i, '\n')
}
wechat$content[3001:3509] <- wechat_content[3001:3509]
save(wechat, file = 'wechat.RData')
load('wechat.Rdata')

titan24_index <- startsWith(other$url, 'http://www.titan24.com/')
titan24 <- other[titan24_index, ]  
other <- other[titan24_index==F,]

# web scraping for titan24 content
titan24_content <- vector()
for (i in 1:dim(titan24)[1]){
  web <- read_html(titan24$url[i],encoding = 'UTF-8')
  Sys.sleep(runif(1))
  titan24_content[i] <- web %>%
    html_nodes('.articleBox-bd') %>%
    html_text() %>%
    paste(collapse = ' ')
  cat('now:', i, '\n')
}
titan24$content <- titan24_content
save(titan24, file = 'titan24.RData')

yystv_index <- startsWith(other$url, 'https://www.yystv.cn/')
yystv <- other[yystv_index, ]  
other <- other[yystv_index==F,]

# web scraping for yystv content
yystv_content <- vector()
for (i in 1:dim(yystv)[1]){
  web <- read_html(yystv$url[i],encoding = 'UTF-8')
  Sys.sleep(runif(1))
  yystv_content[i] <- web %>%
    html_nodes('p') %>%
    html_text() %>%
    paste(collapse = ' ')
  cat('now:', i, '\n')
}
yystv$content <- yystv_content
save(yystv, file = 'yystv.RData')

weibo_index <- startsWith(other$url, 'https://www.weibo.com/')
weibo <- other[weibo_index, ]  
other <- other[weibo_index==F,]

# web scraping for weibo content
weibo_content <- vector()
for (i in 1:dim(weibo)[1]){
  web <- read_html(weibo$url[i],encoding = 'UTF-8')
  Sys.sleep(runif(1))
  weibo_content[i] <- web %>%
    html_nodes('p') %>%
    html_text() %>%
    paste(collapse = ' ')
  cat('now:', i, '\n')
}
weibo$content <- weibo_content
save(weibo, file = 'weibo.RData')
## fail in web scraping

# caixin
caixin_index <- startsWith(other$url, 'http://www.caixin.com/')
caixin <- other[caixin_index, ]  
other <- other[caixin_index==F,]
save(caixin, file = 'caixin.RData')

# caixin free
caixinfree_index <- startsWith(other$url, 'http://china.caixin.com/')
caixinfree <- other[caixinfree_index,]
other <- other[caixinfree_index==F,]
caixinfree_content <- vector()
for (i in 1:dim(caixinfree)[1]){
  web <- read_html(caixinfree$url[i],encoding = 'UTF-8')
  Sys.sleep(runif(1))
  caixinfree_content[i] <- web %>%
    html_nodes('p') %>%
    html_text() %>%
    paste(collapse = ' ')
  cat('now:', i, '\n')
}
caixinfree$content <- caixinfree_content
save(caixinfree, file = 'caixinfree.RData')

# gcore
gcore_index <- startsWith(other$url, 'https://www.gcores.com/')
gcore <- other[gcore_index,]
other <- other[gcore_index==F,]
gcore_content <- vector()
for (i in 1:dim(gcore)[1]){
  web <- read_html(gcore$url[i],encoding = 'UTF-8')
  Sys.sleep(runif(1))
  gcore_content[i] <- web %>%
    html_nodes('.public-DraftStyleDefault-ltr span') %>%
    html_text() %>%
    paste(collapse = ' ')
  cat('now:', i, '\n')
}
gcore$content <- gcore_content
save(gcore, file = 'gcore.RData')

# cb
cb_index <- startsWith(other$url, 'http://www.cb.com.cn/')
cb <- other[cb_index,]
other <- other[cb_index==F,]
cb_content <- vector()
for (i in 1:dim(cb)[1]){
  web <- read_html(cb$url[i],encoding = 'UTF-8')
  Sys.sleep(runif(1))
  cb_content[i] <- web %>%
    html_nodes('.contenttext p') %>%
    html_text() %>%
    paste(collapse = ' ')
  cat('now:', i, '\n')
}
cb$content <- cb_content
save(cb, file = 'cb.RData')

# fangfang
fangfang_index <- startsWith(other$url, 'http://fangfang.blog.caixin.com/')
fangfang <- other[fangfang_index,]
other <- other[fangfang_index==F,]
fangfang_content <- vector()
for (i in 1:dim(fangfang)[1]){
  web <- read_html(fangfang$url[i],encoding = 'UTF-8')
  Sys.sleep(runif(1))
  fangfang_content[i] <- web %>%
    html_nodes('span') %>%
    html_text() %>%
    paste(collapse = ' ')
  cat('now:', i, '\n')
}
fangfang$content <- fangfang_content
save(fangfang, file = 'fangfang.RData')
## fail

# eeo
eeo_index <- startsWith(other$url, 'http://www.eeo.com.cn/')
eeo <- other[eeo_index,]
other <- other[eeo_index==F,]
eeo_content <- vector()
for (i in 1:dim(eeo)[1]){
  web <- try(read_html(eeo$url[i],encoding = 'UTF-8'))
  if ('try-error' %in% class(web)){
    eeo_content[i] <- NA
  } else {
    Sys.sleep(runif(1))
    eeo_content[i] <- web %>%
      html_nodes('.xx_boxsing p') %>%
      html_text() %>%
      paste(collapse = ' ')
  }
  cat('now:', i, '\n')
}
eeo$content <- eeo_content
save(eeo, file = 'eeo.RData')

# zhiisland
zhi_index <- startsWith(other$url, 'http://mp.zhisland.com/')
zhi <- other[zhi_index,]
other <- other[zhi_index==F,]
save(zhi, file = 'zhiisland.RData')
# fail

# pengpai
pengpai_index <- startsWith(other$url, 'https://www.thepaper.cn/')
pengpai <- other[pengpai_index,]
other <- other[pengpai_index==F,]
pengpai_content <- vector()
for (i in 1:dim(pengpai)[1]){
  web <- try(read_html(pengpai$url[i],encoding = 'UTF-8'))
  if ('try-error' %in% class(web)){
    pengpai_content[i] <- NA
  } else {
    Sys.sleep(runif(1))
    pengpai_content[i] <- web %>%
      html_nodes('.news_txt') %>%
      html_text() %>%
      paste(collapse = ' ')
  }
  cat('now:', i, '\n')
}
pengpai$content <- pengpai_content
save(pengpai, file = 'pengpai.RData')

# caixin weekly-need premium
caixinweek_index <- startsWith(other$url, 'http://weekly.caixin.com/')
caixinweek <- other[caixinweek_index,]
other <- other[caixinweek_index==F,]
save(caixinweek, file = 'caixinweek.RData')

# jiemian
jiemian_index <- startsWith(other$url, 'https://www.jiemian.com/')
jiemian <- other[jiemian_index,]
other <- other[jiemian_index==F,]
jiemian_content <- vector()
for (i in 1:dim(jiemian)[1]){
  web <- try(read_html(jiemian$url[i],encoding = 'UTF-8'))
  if ('try-error' %in% class(web)){
    jiemian_content[i] <- NA
  } else {
    Sys.sleep(runif(1))
    jiemian_content[i] <- web %>%
      html_nodes('.article-content p') %>%
      html_text() %>%
      paste(collapse = ' ')
  }
  cat('now:', i, '\n')
}
jiemian$content <- jiemian_content
save(jiemian, file = 'jiemian.RData')

# caixin international
caixininternational_index <- startsWith(other$url, 'http://international.caixin.com/')
caixininternational <- other[caixininternational_index,]
other <- other[caixininternational_index==F,]
caixininternational_content <- vector()
for (i in 1:dim(caixininternational)[1]){
  web <- try(read_html(caixininternational$url[i],encoding = 'UTF-8'))
  if ('try-error' %in% class(web)){
    caixininternational_content[i] <- NA
  } else {
    Sys.sleep(runif(1))
    caixininternational_content[i] <- web %>%
      html_nodes('#Main_Content_Val p') %>%
      html_text() %>%
      paste(collapse = ' ')
  }
  cat('now:', i, '\n')
}
caixininternational$content <- caixininternational_content
save(caixininternational, file = 'caixininternational.RData')

# sohu
sohu_index <- startsWith(other$url, 'https://www.sohu.com/')
sohu <- other[sohu_index,]
other <- other[sohu_index==F,]
sohu_content <- vector()
for (i in 1:dim(sohu)[1]){
  web <- try(read_html(sohu$url[i],encoding = 'UTF-8'))
  if ('try-error' %in% class(web)){
    sohu_content[i] <- NA
  } else {
    Sys.sleep(runif(1))
    sohu_content[i] <- web %>%
      html_nodes('.ql-align-justify') %>%
      html_text() %>%
      paste(collapse = ' ')
  }
  cat('now:', i, '\n')
}
sohu$content <- sohu_content
save(sohu, file = 'sohu.RData')

# chuapp
chuapp_index <- startsWith(other$url, 'http://www.chuapp.com/')
chuapp <- other[chuapp_index,]
other <- other[chuapp_index==F,]
chuapp_content <- vector()
for (i in 1:dim(chuapp)[1]){
  web <- try(read_html(chuapp$url[i],encoding = 'UTF-8'))
  if ('try-error' %in% class(web)){
    chuapp_content[i] <- NA
  } else {
    Sys.sleep(runif(1))
    chuapp_content[i] <- web %>%
      html_nodes('.the-content p') %>%
      html_text() %>%
      paste(collapse = ' ')
  }
  cat('now:', i, '\n')
}
chuapp$content <- chuapp_content
save(chuapp, file = 'chuapp.RData')

# weibo
weibo_index <- startsWith(other$url, 'https://weibo.com/')
weibo <- other[weibo_index,]
other <- other[weibo_index==F,]
# fail
save(weibo, file = 'weibo.RData')

# qqnews
qqnews_index <- startsWith(other$url, 'https://new.qq.com/')
qqnews <- other[qqnews_index,]
other <- other[qqnews_index==F,]
# fail
save(qqnews, file = 'qqnews.RData')

# for test use
url <- 'https://www.sohu.com/a/376165378_220095?spm=smpc.author.fd-d.10.1582892817970SL5tFlm'
web <- read_html(url,encoding = 'UTF-8')
web %>%
  html_nodes('.ql-align-justify') %>%
  html_text() %>%
  paste(collapse = ' ')
