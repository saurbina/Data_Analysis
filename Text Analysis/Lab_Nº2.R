###### 03/25/2024
### QMSSGR5018
# Assignment: Lab Nº2
### ADV. ANALYTIC TECHNIQUES 

library(tidyverse)
library(tidytext)
library(stringr)
library(SnowballC)
library(tm)
library(wordcloud)
library(lsa)
library(stargazer)
library(RTextTools)
library(topicmodels)



spanish_stopwords <- stopwords("spanish")
stopwords_df <- data_frame(word = spanish_stopwords)


paper_words <- data_frame(file = paste0("/Users/saurbina/Documents/pdf-txt/",
                                        c("boric_v1.txt", "pinera_V1.txt"))) %>%
  mutate(text = map(file, read_lines)) %>%
  unnest() %>%
  group_by(file = str_sub(basename(file), 1, -5)) %>%
  mutate(line_number = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stopwords_df, by = "word") 


## count each word per speech
pw = paper_words[,c("file","word")]
d= count_(pw, c("file", "word"))
## make a document term matrix ##
pwdtm = d %>%
  cast_dtm(file, word, n)
## make the dtm into a dataframe ##
mpwdtm=as.matrix(pwdtm)
df.mpwdtm=as.data.frame(mpwdtm)
## make the dtm into a tdm instead ##
t.t = t(mpwdtm)

# make t.t as dataframe 
df.t.t <- as.data.frame(t.t)

summing = function(x) x/sum(x, na.rm=T)
df.t.t.2 = apply(df.t.t, 2, summing)
df.t.t$names < -rownames(df.t.t)
df.t.t = as.data.frame(t.t)
df.t.t$names<-rownames(df.t.t)
head(df.t.t)
df.t.t.2 = as.data.frame(df.t.t.2)
df.t.t.2$names<-rownames(df.t.t.2)
df.t.t.2 = as.data.frame(df.t.t.2)
total <- merge(df.t.t,df.t.t.2,by="names")
total$boric.over.pinera = (total$boric_v1.y) -
  (total$pinera_V1.y)
sort.OT <- total[order(total$boric.over.pinera) , ]
sort.OT[1:30, ]

total$pinera.over.boric = (total$pinera_V1.y) -
  (total$boric_v1.y)
sort.OT <- total[order(total$pinera.over.boric) , ]
sort.OT[1:30, ]

# Graph
df.t.t = as.data.frame(t.t)
summing = function(x) x/sum(x, na.rm=T)
df.t.t.2 = apply(df.t.t, 2, summing)
df.t.t$names<-rownames(df.t.t)
df.t.t = as.data.frame(t.t)
df.t.t$names<-rownames(df.t.t)
head(df.t.t)
df.t.t.2 = as.data.frame(df.t.t.2)
df.t.t.2$names<-rownames(df.t.t.2)
df.t.t.2 = as.data.frame(df.t.t.2)
q = qplot(boric_v1, pinera_V1, data = df.t.t.2)
q + geom_text(aes(label=names), size = 4.5)

cosine(t.t)
cor(t.t, method="pearson")
ctable <- table(t.t)
chisq.test(ctable)


m1a = lm(boric_v1 ~ pinera_V1, df.t.t)
m2a = lm(boric_v1.y ~ pinera_V1.y , total)

stargazer(m1a, m2a, type = "text")

# Topic modeling
paper_words <- data_frame(file = paste0("/Users/saurbina/Documents/pdf-txt/",
                                        c("boric_v1.txt"))) %>%
  mutate(text = map(file, read_lines)) %>%
  unnest() %>%
  group_by(file = str_sub(basename(file), 1, -5)) %>%
  mutate(line_number = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  filter(!grepl("\\d", word)) %>%  
  anti_join(stopwords_df, by = "word")

## count each word per speech
pw2 = paper_words[,c("file","word")]
d2= count_(pw2, c("file", "word"))
## make a document term matrix ##
pwdtm2 = d2 %>%
  cast_dtm(file, word, n)

lda <- LDA(pwdtm2, 10)
terms(lda,10)

# Generate word cloud for each speech
wordcloud(df.t.t$names, df.t.t$boric_v1, min.freq = 20, random.color = TRUE, ordered.colors = TRUE)
wordcloud(df.t.t$names, df.t.t$pinera_V1, min.freq = 20, random.color = TRUE, ordered.colors = TRUE)




