rm(list=ls())


#install.packages("digest")
#install.packages("devtools")
library("devtools")
#devtools::install_github("lchiffon/wordcloud2")
#install.packages("wordcloud2")
library("devtools")
library(wordcloud2)
library(tm)
library(dplyr)
library(stringr)
memory.limit(size=100000)
library(tidyr)
library(stargazer)

##############################################################
load("text_analysis_Weslyan_Ad.Rdata")
unique(df$party)

# drop parties other than Dem or Rep 
df1 <- df[df$party == 1 | df$party == 2,]
unique(df1$party) # Now we only have Dem or Rep 

sum(is.na(df1$text)) #How many NA 
sum(!is.na(df1$text))#How many non-NA  

df1 <- df1 %>% drop_na(text) #drop NA 

sum(is.na(df1$text)) #How many NA 
sum(!is.na(df1$text))#How many non-NA  
##############################################################


#wordcloud Dem & China ads
wc_dem <- df1 %>% filter(party ==1 & issue65==1)

medium.corpus = Corpus(VectorSource(wc_dem$text)) 

removethings = function(text){
  text = gsub(pattern = 'message', '', text)
  text = gsub(pattern = 'approve', '', text)
  text = gsub(pattern = 'China', '', text)
  text = gsub(pattern = "\n", '', text)
  return(text)
}
             
medium.corpus = medium.corpus %>%
  tm_map(content_transformer(removethings)) %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(stemDocument)

tdm <- TermDocumentMatrix(medium.corpus) %>%
  as.matrix()
words <- sort(rowSums(tdm), decreasing = TRUE)
df_dem = data.frame(word = names(words), freq = words)

uxc.colors = c("#4c4c4c", "#619e8d", "#95485d")
uxc.background = "#fefefe"
library(extrafont)
fonts()

wordcloud2(df_all_perc,
           color = rep_len(uxc.colors, nrow(df)),
           backgroundColor = uxc.background,
           fontFamily = "DM Sans",
           #size = 2.5,
           #minSize =5,
           rotateRatio = 0)

#wordcloud repulicans & China ads
wc_rep <- df1 %>% filter(party ==2 & issue65==1)

medium.corpus = Corpus(VectorSource(wc_rep$text)) 

removethings = function(text){
  text = gsub(pattern = 'message', '', text)
  text = gsub(pattern = 'approve', '', text)
  text = gsub(pattern = 'China', '', text)
  text = gsub(pattern = 'Chinese', '', text)
  text = gsub(pattern = "\n", '', text)
  print(text)
  return(text)
}
medium.corpus = medium.corpus %>%
  tm_map(content_transformer(removethings)) %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(stemDocument)

tdm <- TermDocumentMatrix(medium.corpus) %>%
  as.matrix()
words <- sort(rowSums(tdm), decreasing = TRUE)
df_rep = data.frame(word = names(words), freq = words)

uxc.colors = c("#4c4c4c", "#619e8d", "#95485d")
uxc.background = "#fefefe"
library(extrafont)
fonts()

wordcloud2(df_rep,
           color = rep_len(uxc.colors, nrow(df)),
           backgroundColor = uxc.background,
           fontFamily = "DM Sans",
           #size = 2.5,
           #minSize =5,
           rotateRatio = 0)

#wordcloud all China ads #### (NOTE: THIS IS THE OLD WORDCLOUD)
wc_all <- df1 %>% filter(issue65==1)

medium.corpus = Corpus(VectorSource(wc_all$text)) 

removethings = function(text){
  text = gsub(pattern = 'message', '', text)
  text = gsub(pattern = 'approve', '', text)
  text = gsub(pattern = 'China', '', text)
  text = gsub(pattern = 'Chinese', '', text)
  text = gsub(pattern = "\n", '', text)
  return(text)
}

medium.corpus = medium.corpus %>%
  tm_map(content_transformer(removethings)) %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(stemDocument)

tdm <- TermDocumentMatrix(medium.corpus) %>%
  as.matrix()
words <- sort(rowSums(tdm), decreasing = TRUE)
df_all = data.frame(word = names(words), freq = words)

uxc.colors = c("#4c4c4c", "#619e8d", "#95485d")
uxc.background = "#fefefe"
library(extrafont)
fonts()

wordcloud2(df_all,
           color = rep_len(uxc.colors, nrow(df)),
           backgroundColor = uxc.background,
           fontFamily = "DM Sans",
           #size = 2.5,
           #minSize =5,
           rotateRatio = 0)

## WORDCLOUD ALL CHINA ADS (NEW)


df_dem <- df_dem %>% mutate(freq_perc = 100*(df_dem$freq/sum(df_dem$freq))) #create percent for freq
df_rep <- df_rep %>% mutate(freq_perc = 100*(df_rep$freq/sum(df_rep$freq))) #create percent for freq
df_all_new <- rbind(df_dem, df_rep) #combine rows 

library(tidyverse)

w <- df_all_new$word[duplicated(df_all_new$word)] # we have a total of 738 words that are both mentioned in dem and rep 

df_all_new_perc <- df_all_new %>% group_by(word) %>% summarize(freq_perc = sum(freq_perc)) 
#we take care of duplicate rows by adding freq_perc

df_all_new_perc <- df_all_new_perc[order(-df_all_new_perc$freq_perc),] #sort freq perc from highest to lowest

df_all_new_perc$freq_perc <- df_all_new_perc$freq_perc/2 #divide freq-perc by 2 

wordcloud2(df_all_new_perc,
           color = rep_len(uxc.colors, nrow(df)),
           backgroundColor = uxc.background,
           fontFamily = "DM Sans",
           #size = 2.5,
           #minSize =5,
           rotateRatio = 0)

