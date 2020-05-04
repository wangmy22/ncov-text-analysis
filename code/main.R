# load in packages
library(tidyverse)
library(quanteda)
library(jiebaR)
library(stm)
library(gridExtra)

# load in data
setwd("D:\\LSE\\MY459 Quantitative Text Analysis\\project\\data")
load('final.RData')

# delete those with no text
data <- data[-which(data$content == ''),] # 3 deleted

# seperate chinese text data with spacing between words
data$content.single <- NA
for (i in 1:dim(data)[1]){
  data$content.single[i] <- paste(stringi::stri_split_boundaries(data$content[i])[[1]], collapse = ' ')
}

# calculate word count
data$wc <- NA
for (i in 1:dim(data)[1]){
  data$wc[i] <- wordcount(data$content.single[i], sep = ' ')
}  

# histogram on word count
hist(data$wc)
plot(density(data$wc))
hist(log(data$wc))

# descriptive statistics for delected text
data$is_deleted[is.na(data$is_deleted)] <- FALSE
summary(data$is_deleted)

# calculate maximum and minimum word count and other statistics
max(data$wc);min(data$wc)
mean(data$wc);median(data$wc)

# return its name
data$title[which(data$wc==max(data$wc))];data$media[which(data$wc==max(data$wc))]
data$title[which(data$wc==min(data$wc))];data$media[which(data$wc==min(data$wc))]

# some errors in the media name, fixed them manually
data$media[str_detect(data$media,'剥洋葱people')] <- '剥洋葱people'
data$media[str_detect(data$media,'第一财经')] <- '第一财经'

# extract media name
media <- unique(data$media)
write_excel_csv(as.data.frame(media),'media.csv')


# define cutter - using our defined dictionary
USER_PATH <- as.character(USERPATH)
STOP_PATH <- as.character(STOPPATH)

cutter = worker(user = "D:/LSE/MY459 Quantitative Text Analysis/project/dict/user.dict.utf8",
                stop_word = "D:/LSE/MY459 Quantitative Text Analysis/project/dict/stop_words.utf8",user_weight = 'max')

# change content from list to string and remove '\n' pattern and all english characters(meaningless words)
data$content <- as.character(data$content)
data$content <- str_remove_all(data$content,'\n')
data$content <- str_remove_all(data$content,'[a-zA-Z]+')

# use the cutter to cut texual data
data$content.cut <- NA
for (i in 1:dim(data)[1]){
  data$content.cut[i] <- paste(segment(data$content[i],cutter),collapse = ' ')
  if (i%%10==0) cat('now cutting',i,'\n')
}

# there're repeated article, delete them manually
data <- data %>%
  group_by(title) %>%
  mutate(sum = length(title)) %>%
  arrange(desc(sum))

data <- data[-c(1,3,5,7,12,14,18,22,26,30,34,36,38,40,42,44,46,50,52,54,58),]

data <- data %>%
  select(-sum)

data <- data %>%
  filter(wc>200)

# save(data,file='data_cut.RData')

###################################################################################
load('data_cut.RData')

# histogram
hist(data$wc,main='Histogram of Word Count', xlab='Word Count')

# maximum of word count
max(data$wc)
data$title[which(data$wc==max(data$wc))];data$media[which(data$wc==max(data$wc))]

# count the number of deleted
sum(data$is_deleted==T)
table(data$is_deleted,data$category)

# density of release over date
par(mfrow=c(1,2))
data$date[which(data$id=='3686')] <- '2020-02-28'
data.temp <- data %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  mutate(sum = length(id)) %>%
  select(date,sum) %>%
  unique()
plot1 <- ggplot(data=data.temp) +
  geom_step(aes(x=date,y=sum)) +
  geom_smooth(aes(x=date,y=sum),se=F,span=0.5)+
  ggtitle('Number of Documents Released over Date')+
  theme_classic() +
  scale_x_date(date_breaks = '2 week', date_labels = '%m-%d')

# for deleted 
data.temp <- data %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  mutate(sum = length(id)) %>%
  mutate(delete = sum(is_deleted)) %>%
  select(date,sum,delete) %>%
  mutate(proportion = delete/sum) %>%
  unique() %>%
  filter(date > '2020-01-18')
plot2 <- ggplot(data=data.temp) +
  geom_step(aes(x=date,y=proportion)) +
  geom_smooth(aes(x=date,y=proportion),se=F,span=0.5)+
  ggtitle('Proportion of Deleted Documents over Date')+
  theme_classic()+
  scale_x_date(date_breaks = '1 week', date_labels = '%m-%d')

grid.arrange(plot1,plot2,ncol=2)

# transform the textual data into corpus
cp <- corpus(data, text_field = 'content.cut')

# wordcloud plot by category and isdelted
dfm.plot <- dfm(cp,remove = c(stopwords('zh', source='misc'),c('共','字','载','加','功能','介绍','信号','一扫','媒体','新闻','具有','内涵','思考','来源','人群','本期','独立')),
                remove_number = T,group='category')
textplot_wordcloud(dfm.plot,max_size = 3,min_size = 0.2,min_count = 200,max_word=150,comparison = T,color = c("blue", "red"))

dfm.plot <- dfm(cp,remove = c(stopwords('zh', source='misc'),c('共','字','载','加','功能','介绍','信号','一扫','媒体','新闻','具有','内涵','思考','来源','人群','本期','独立')),
                remove_number = T,group='is_deleted')
textplot_wordcloud(dfm.plot,max_size = 3,min_size = 0.2,min_count = 200,max_word=150,comparison = T,color = c("blue", "red"))

#####################################################################################

# create dfm matrix
cp.dfm.group <- dfm(cp,remove = c(stopwords('zh', source='misc'),c('共','字','载','加','功能','介绍','信号','一扫','媒体','新闻','具有','内涵','思考','来源','人群','本期','独立')),
                    remove_number=TRUE,group='media')
topfeatures(cp.dfm.group,50)
cp.dfm.group2 <- dfm_trim(cp.dfm.group,min_termfreq = 10)

# get the docnames
docnames(cp.dfm.group2)

# implement wordfish model 
wf <- textmodel_wordfish(cp.dfm.group2, dir = c(25,132))#  direction '方方' to '财新网'


# extract the document position
theta <- wf$theta
theta.plot <- as.data.frame(cbind(docnames(cp.dfm.group2),as.numeric(theta)))

# extract the type of account
type <- data %>%
  ungroup() %>%
  select(media,category) %>%
  mutate(value = ifelse(category == 'non_fiction',1,0)) %>%
  group_by(media) %>%
  mutate(prop = sum(value==1)/(sum(value==1)+sum(value==0))) %>%
  unique() %>%
  group_by(media) %>%
  mutate(type = mean(value)) %>%
  mutate(type = factor(type, levels = c(0,0.5,1),labels = c('narrative','mixed','non_fiction'))) %>%
  select(media,type,prop) %>%
  unique() 

# extract is deleted
delete <- data %>%
  ungroup() %>%
  select(media,is_deleted) %>%
  group_by(media) %>%
  mutate(delete = max(is_deleted)) %>%
  mutate(delete_prop = sum(is_deleted==1)/(sum(is_deleted==1)+sum(is_deleted==0))) %>%
  select(media, delete, delete_prop) %>%
  unique() %>%
  mutate(delete = factor(delete, levels = c(0,1), labels = c('not deleted', 'deleted')))

# merge the theta with the type of account
theta.plot <- merge(theta.plot,type,by.x='V1',by.y='media')
# density by group
wf.plot1 <- ggplot(data = theta.plot,aes(x=theta))+
  geom_density(aes(color=type,fill=type),alpha=0.1,position='identity',bw=0.5) +
  ggtitle('Smoothed Density by Category')+
  theme_classic()
# histogram by group
wf.plot2 <- ggplot(data = theta.plot,aes(x=theta))+
  geom_histogram(aes(color=type,fill=type),bins=10)+
  ggtitle('Histogram by Category')+
  theme_classic()

grid.arrange(wf.plot2,wf.plot1,ncol=2)

# merge the theta with the type of account
theta.plot <- merge(theta.plot,delete,by.x='V1',by.y='media')
wf.plot3 <- ggplot(data = theta.plot,aes(x=theta))+
  geom_density(aes(color=delete,fill=delete),alpha=0.1,position='identity',bw=0.5)+
  ggtitle('Smoothed Density by Is Deleted')+
  theme_classic()

wf.plot4 <- ggplot(data = theta.plot,aes(x=theta))+
  geom_histogram(aes(color=delete,fill=delete),bins=10)+
  ggtitle('Histogram by Is Deleted')+
  theme_classic()

grid.arrange(wf.plot4,wf.plot3,ncol=2)

# textplot
textplot_scale1d(wf, groups = theta.plot$type)
textplot_scale1d(wf, groups = theta.plot$delete)


# extract medias with most extreme theta
theta.plot <- theta.plot %>%
  arrange(V2)

head(theta.plot$V1,20);tail(theta.plot$V1,20)

# is there such relationship inside the 'mixed' category
theta.mixed <- theta.plot %>%
  filter(type == 'mixed')
theta.mixed$x <- log(theta.mixed$prop/(1-theta.mixed$prop))
summary(lm(as.numeric(theta.mixed$V2)~theta.mixed$x))

# the same for 'delete'
theta.mixed <- theta.plot %>%
  filter(delete == 'deleted')
summary(lm(as.numeric(theta.mixed$V2)~theta.mixed$delete_prop))


# extract most extreme words
word <- as.data.frame(cbind(wf$beta,featnames(cp.dfm.group2)))
word <- word %>%
  arrange(V1)
head(word$V2,50)
tail(word$V2,50)

# structured topic model
cp.dfm <- dfm(cp,remove = c(stopwords('zh', source='misc'),c('共','字','载','加','功能','介绍','信号','一扫','媒体','新闻','具有','内涵','思考','来源','人群','本期','独立')),
              remove_number=TRUE,verbose=TRUE)
cp.dfm2 <- dfm_trim(cp.dfm, min_termfreq = 20)
docvars(cp.dfm2)$is_deleted <- as.numeric(docvars(cp.dfm2)$is_deleted)
docvars(cp.dfm2)$date <- as.numeric(as.Date(docvars(cp.dfm2)$date))-min(as.numeric(as.Date(docvars(cp.dfm2)$date)))+1
docvars(cp.dfm2)$category <- as.factor(docvars(cp.dfm2)$category)

# 67
stmfit.1 <- stm(cp.dfm2, K=0, prevalence = ~ is_deleted +s(date)+category+category:s(date),data=docvars(cp.dfm2),max.em.its = 1,seed=459)
# 63
stmfit.2 <- stm(cp.dfm2, K=0, prevalence = ~ is_deleted +s(date)+category+category:s(date),data=docvars(cp.dfm2),max.em.its = 1,seed=460)
# 52
stmfit.3 <- stm(cp.dfm2, K=0, prevalence = ~ is_deleted +s(date)+category+category:s(date),data=docvars(cp.dfm2),max.em.its = 1,seed=461)
# 57
stmfit.4 <- stm(cp.dfm2, K=0, prevalence = ~ is_deleted +s(date)+category+category:s(date),data=docvars(cp.dfm2),max.em.its = 1,seed=462)
# 64
stmfit.5 <- stm(cp.dfm2, K=0, prevalence = ~ is_deleted +s(date)+category+category:s(date),data=docvars(cp.dfm2),max.em.its = 1,seed=463)

# stmfit <- stm(cp.dfm2, K=50, prevalence = ~ is_deleted +s(date),data=docvars(cp.dfm2),max.em.its = 75,seed=459)

# spectrum estimate
stmfit.spec <- stm(cp.dfm2, K=60, prevalence = ~ is_deleted*s(date)+category*s(date),data=docvars(cp.dfm2),max.em.its = 150,seed=459)
# save(stmfit.spec,file='stmspec.RData') 
load(file='stmspec.RData')

# LDA estimate
Select <- selectModel(cp.dfm2, K = 60,
                              prevalence =~ is_deleted*s(date)+category*s(date), max.em.its = 100, data = docvars(cp.dfm2),
                              runs = 20, seed = 1)
# save(Select,file='select.RData')
load('select.RData')

plotModels(Select, pch = c(1, 2,3,4),
           legend.position = "bottomright")

# select the best model
stm.best <- Select$runout[[3]]

stm.model <- stm.best
# get the most relevent word of each topics
Top <- labelTopics(stm.model, 1:60,n=10)$frex
con<-file('words.csv',encoding="UTF-8")
write_excel_csv(as.data.frame(Top),con)

plot(stm.model, type = "summary",labeltype = 'frex',
     n=5,xlim = c(0, 0.08))


# estimate the effect of each topics
prep <- estimateEffect(1:60 ~ is_deleted*s(date)+category*s(date), stm.model,
                       meta = docvars(cp.dfm2), uncertainty = "Global")

# get the coefficient
summary(prep, topics = 28)

# difference plot
plot(prep, covariate = "is_deleted", topics = 1:60,
     model = stm.model, method = "difference",cov.value1 = 0,cov.value2 = 1,
     labeltype = 'custom',custom.labels = paste('Topic ',1:60),
     xlim=c(-0.12,0.12),xlab='Deleted ......................... Not Deleted',
     main='Deleted vs. Not Deleted',ci.level = 0.8)
plot(prep, covariate = "category", topics = 1:60,
     model = stm.model, method = "difference",cov.value1 = 'narrative',cov.value2 = 'non_fiction',
     labeltype = 'custom',custom.labels = paste('Topic ',1:60),xlim=c(-0.12,0.12),xlab='More Non Fiction ......................... More Narrative',
     main='Non Fiction vs. Narrative')

# proportion of each topics
plot(stm.model, type = "summary",n=5)

# find the mose relevent articles
get.relevent <- function(stm,n,topics){
  thoughts.title <- findThoughts(stm, texts = data$title, n = n,
                                   topics = topics)$docs[[1]]
  thoughts.deleted <- findThoughts(stm, texts = data$is_deleted, n = n,
                                     topics = topics)$docs[[1]]
  thoughts.media <- findThoughts(stm, texts = data$media, n = n,
                                   topics = topics)$docs[[1]]
  cbind(thoughts.title,thoughts.deleted,thoughts.media)
}
(get.relevent(stm.model,20,38))
(get.relevent(stm.model,10,51))
(get.relevent(stm.model,10,54))
(get.relevent(stm.model,10,55))
(get.relevent(stm.model,10,60))

plot(prep, 'date', method = "continuous", topics = 1:5+5*11)

plot(stm.model, type = "perspectives", topics = c(23,24))

# 8
prep <- estimateEffect(c(8,9) ~ is_deleted*s(date)+category*s(date), stm.model,
                       metadata = docvars(cp.dfm2), uncertainty = "None")

plot(prep, covariate = "date", model = stm.best,
     method = "continuous", xlab = "Days", moderator = "category",
     moderator.value = 'narrative', linecol = "blue", ylim = c(0, 0.8),
     printlegend = FALSE,xlim=c(32,96),xaxt='n')
dayseq <- c('01-20','02-10','03-01','03-24')
axis(1,c(32,53,73,96),labels=dayseq,tick=T)

plot(prep, covariate = "date", model = stm.best,
     method = "continuous", xlab = "Days", moderator = "category",
     moderator.value = 'non_fiction', linecol = "red", add = TRUE,
     printlegend = FALSE)

####################
plot(prep, covariate = "date", model = stm.model,
     method = "continuous", xlab = "Days", moderator = "is_deleted",
     moderator.value = 0, linecol = "blue",ylim = c(0, 0.8),
     printlegend = FALSE,xlim=c(32,96),xaxt='n')
dayseq <- c('01-20','02-10','03-01','03-24')
axis(1,c(32,53,73,96),labels=dayseq,tick=T)


plot(prep, covariate = "date", model = stm.model,
     method = "continuous", xlab = "Days", moderator = "is_deleted",
     moderator.value = 1, linecol = "red", add = TRUE,
     printlegend = FALSE)
legend('topright',col = c('red','blue'),lty=c(1,1),legend=c('deleted','not deleted'))

# plotall
par(mfrow=c(6,10))
for (i in 1:60){
  prep <- estimateEffect(c(i) ~ is_deleted*s(date)+category*s(date), stm.model,
                         metadata = docvars(cp.dfm2), uncertainty = "None")
  
  plot(prep, covariate = "date", model = stm.best,
       method = "continuous", xlab = "Days", moderator = "category",
       moderator.value = 'narrative', linecol = "blue",ylim=c(0,0.3),
       printlegend = FALSE,xlim=c(32,96),main=paste('topic',i))
  
  plot(prep, covariate = "date", model = stm.best,
       method = "continuous", xlab = "Days", moderator = "category",
       moderator.value = 'non_fiction', linecol = "red", add = TRUE,
       printlegend = FALSE)
}

for (i in 1:60){
  prep <- estimateEffect(c(i) ~ is_deleted*s(date)+category*s(date), stm.model,
                         metadata = docvars(cp.dfm2), uncertainty = "None")
  
  plot(prep, covariate = "date", model = stm.model,
       method = "continuous", xlab = "Days", moderator = "is_deleted",
       moderator.value = 0, linecol = "blue",ylim=c(0,0.3),
       printlegend = FALSE,xlim=c(32,96),main=paste('topic',i))
  
  plot(prep, covariate = "date", model = stm.model,
       method = "continuous", xlab = "Days", moderator = "is_deleted",
       moderator.value = 1, linecol = "red", add = TRUE,
       printlegend = FALSE)
}

# correlation plot
mod.corr <- topicCorr(stm.model)
plot(mod.corr)


# get relevant article
topic <- ceiling(1:300/5)
article <- tibble(topic, title = rep(NA,300), deleted = rep(NA,300),media= rep(NA,300))

for (i in 1:60){
    article[(5*i-4):(5*i),2:4] <- get.relevent(stm.model,5,i)
}
write_excel_csv(article,file('relearticle.csv',encoding = 'UTF-8'))
