library(tidyverse)
library(quanteda)
  setwd("D:\\LSE\\MY459 Quantitative Text Analysis\\project\\")
# dataset <- c('caixin','caixinfree','caixininternational','caixinweek','cb','chuapp','eeo','fangfang','gcore','jiemian','pengpai','qqnews','sohu','titan24','wechat','weibo','yystv','zhiisland','other')
# for (i in dataset){
#   dataset.name <- paste(i,'.RData',sep='')
#   load(dataset.name)
# }
# data <- merge(caixinfree,caixin, all=T) %>%
#   merge(caixininternational,all=T) %>%
#   merge(caixinweek,all=T) %>%
#   merge(cb,all=T) %>%
#   merge(chuapp,all=T) %>%
#   merge(eeo,all=T) %>%
#   merge(fangfang,all=T) %>%
#   merge(gcore,all=T) %>%
#   merge(jiemian,all=T) %>%
#   merge(pengpai,all=T) %>%
#   merge(qqnews,all=T) %>%
#   merge(sohu,all=T) %>%
#   merge(titan24,all=T) %>%
#   merge(wechat,all=T) %>%
#   merge(weibo,all=T) %>%
#   merge(yystv,all=T) %>%
#   merge(zhi,all=T) %>%
#   merge(other,all=T)
# data <- data[-(1:3),]
# 
# data <- data %>%
#   mutate(webscraping = ifelse(is.na(content)==F&content!='', TRUE,FALSE))
#  save(data, file = 'data.RData')
load('data.RData')

# extract those articles without webscraping
ocr <- data[!(data$id %in% article.web$id),]
ocr$id <- as.numeric(ocr$id)
# save(ocr, file = 'ocr.RData')
load('ocr.Rdata')
# 
# article.web <- data %>%
#   filter(webscraping==T)
# 
library(rvest)
archive <- data %>%
  filter(webscraping == F)
archive_content <- vector()
for (i in 1:dim(archive)[1]){
  web <- try(read_html(aichive$url[i],encoding = 'UTF-8'))
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


library(jiebaR)
library(stringr)
cutter = worker(user = USERPATH)
article.web$content.seg <- NA
for (i in 1:dim(article.web)[1]){
  article.web$content.seg[i] <- paste(segment(article.web$content[i],cutter),collapse = ' ')
  cat('now cutting',i,'\n')
}
save(article.web,file='articleweb.RData')
load('articleweb.RData')
web.corpus <- corpus(article.web, text_field = 'content.seg')
# save(web.corpus,file='webcorpus.RData')
web.dfm.group <- dfm(web.corpus,remove = stopwords('zh', source = "misc"),remove_numbers=T,groups = 'media')
web.dfm.group
topfeatures(web.dfm.group,50)
web.dfm.group2 <- dfm_trim(web.dfm.group,min_termfreq = 10)
wf <- textmodel_wordfish(web.dfm.group2, dir = c(20,8))
theta <- wf$theta
dotchart(theta)
theta.plot <- as.data.frame(cbind(docnames(web.dfm.group2),as.numeric(theta)))

# extract the type of account
type <- article.web %>%
  select(media,category) %>%
  unique() %>%
  mutate(value = ifelse(category == 'non_fiction',1,0)) %>%
  group_by(media) %>%
  mutate(type = mean(value)) %>%
  mutate(type = factor(type, levels = c(0,0.5,1),labels = c('narrative','mixed','non_fiction'))) %>%
  select(media,type) %>%
  unique() 

theta.plot <- merge(theta.plot,type,by.x='V1',by.y='media')
ggplot(data = theta.plot,aes(x=theta))+
  geom_density(aes(color=type,fill=type),alpha=0.3,position='identity',bw=.5)

# topic model
library(topicmodels)

web.dfm.individual <- dfm(web.corpus,remove = stopwords('zh', source = "misc"),remove_numbers=T)
web.dfm.individual <- dfm_trim(web.dfm.individual, min_termfreq = 10)
lda2 <- LDA(web.dfm.individual,k=60, method = "Gibbs",control = list(verbose=10L,seed=330,iter=800))
terms2 <- get_terms(lda2,30)



json <- jsonlite::fromJSON('data.json')
js <- data.frame(title = rep(NA,2382),content_json = rep(NA,2382))
for (i in 1:2382){
  js[i,1] <- json[[i]]$title
  js[i,2] <- json[[i]]$text
  cat('now',i,'\n')
}
data$content[is.na(data$content)] <- ''
data <- merge(data,js,by.x='title',by.y='title',all=T)
data <- data[is.na(data$title)==F,]
data <- data[is.na(data$content)==F,]
data <- data %>%
  mutate(replace = ifelse(is.na(content_json)==F & content=='',T,F)) %>%
  mutate(content = ifelse(replace==T,content_json,content))
# save(data,file='data.RData')
load('data2.RData')

article.web <- data2 %>%
  filter(content!='')
save(article.web, file = 'articleweb.RData')
