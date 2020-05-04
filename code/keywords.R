# read in packages required
library(tidyverse)
library(quanteda)
library(ngram)

# read in the text data
setwd("D:\\LSE\\MY459 Quantitative Text Analysis\\project\\data")
load('final.RData')

# delete those with no text
data <- data[-which(data$content == ''),] # 3 deleted

# extract keywords
key <- textstat_collocations(data$content.single,size=2:8,min_count = 400)
# save(key,file='keyword.RData')
# load('keyword.RData')

key.all <- key %>%
  mutate(nest_prop = count_nested/count) %>%
  filter(nest_prop < 1) %>%
  filter(z > 1.96)

word <- vector()
for (i in 1:dim(key.all)[1]){
  word[i] <- str_remove_all(key.all$collocation[i], pattern = ' ')
}
word <- as.data.frame(word)
write_excel_csv(word,'keyword.csv')
