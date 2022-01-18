rm(list=ls())

library(stm)
library(tm)
library(SnowballC)
library(Matrix)

#BASIC PREPROCESSING 
load("text_analysis_Weslyan_Ad.Rdata")

# drop parties other than Dem or Rep 
df1 <- df[df$party == 1 | df$party == 2,]
unique(df1$party) # Now we only have Dem or Rep 

# drop other NA for issue65 
df1 <- df1[df1$issue65 == 0 | df1$issue65 == 1,]
unique(df1$issue65) #Now we don't have NA 

sum(is.na(df1$text)) #How many NA 
sum(!is.na(df1$text))#How many non-NA  

df1 <- df1 %>% drop_na(text) #drop NA 

sum(is.na(df1$text)) #How many NA 
sum(!is.na(df1$text))#How many non-NA  

for (i in 1:nrow(df1)){
  text <- c(df1$text[i])
  text = gsub(pattern = 'message', '', text) # remove message which is irrelevant to our analysis
  text = gsub(pattern = 'approve', '', text) # remove approve which is irrelevent to our analysis 
  df1$text[i] <- text 
}

# we create a new variable "China_party" that indicates as follows 
df1 <- df1 %>% mutate(china_party =
                        case_when(issue65 == 0 & party == 1 ~ "Non-China, Democrat", 
                                  issue65 == 1 & party == 2 ~ "China, Republican",
                                  issue65 == 0 & party == 2 ~ "Non-China, Republican",
                                  issue65 == 1 & party == 1 ~ "China, Democrat")
                      
)

unique(df1$china_party)


# we change names of the variables just like the plots for visualizing purposes 
df1 <- df1 %>% mutate(china =
                        case_when(issue65 ==1 ~ "China Ads", 
                                  issue65 ==0 ~ "Non-China Ads"),
                      party =
                        case_when(party==1 ~ "Democrat",
                                  party==2 ~ "Republican"))
unique(df1$china)
unique(df1$party)


############## STM CHINA VS. NON CHINA ################# 
text <- as.character(df1$text)
china <- as.character(df1$china)
party <- as.character(df1$party)

# #Function for automated text processing
data_use <- cbind(china,party)
part1 <- textProcessor(text, metadata = as.matrix(data_use))



#Extracting the relevant components 
vocab <- part1$vocab
docs <- part1$documents
meta <- part1$meta


#Getting the documents ready for analysis
out <- prepDocuments(docs, vocab, meta)
vocab2 <- out$vocab
docs2 <- out$documents
meta2 <- out$meta

## we're going to include china 
##to measure prevalence of topics across china variable
##to do this, we specify a formula with no dependent variable.

topmod_fit <- stm(documents = docs2, vocab = vocab2, K = 8, 
                   prevalence = ~ china, max.em.its = 75, data = meta2, seed = 1215228)
labelTopics(topmod_fit)

##we can now plot the prevalence effects using 
##some of the tools provided in the package.
##Specifically, we set the topic that we want to examine---
##in this case three---and then the variable we're interested
prep <- estimateEffect(c(3) ~ china, topmod_fit, metadata = meta2)
plot.estimateEffect(prep, 'china', method = 'pointestimate')

##note that this provides the relative attention to topics over the baseline
##we can also examine several topics at once.  Let's examine how attention to topics differs across topics 3 and 4
prep <- estimateEffect(c(1:4) ~ china, topmod_fit, metadata= meta)
plot.estimateEffect(prep, 'china', method = 'pointestimate', xlim = c(-.1, .3), main = "Topic 1:4")
prep <- estimateEffect(c(4:8) ~ china, topmod_fit, metadata= meta)
plot.estimateEffect(prep, 'china', method = 'pointestimate', xlim = c(-.1, .3), main = "Topic 4:8")


############## STM: Dem China, Rep China, Dem Non-China, Rep Non-China #################


# preprocessing 
text <- as.character(df1$text)
china_party <- as.character(df1$china_party)

# #Function for automated text processing
data_use <- cbind(china_party)
part1 <- textProcessor(text, metadata = as.matrix(data_use))


#Extracting the relevant components 
vocab <- part1$vocab
docs <- part1$documents
meta <- part1$meta


#Getting the documents ready for analysis
out <- prepDocuments(docs, vocab, meta)
vocab2 <- out$vocab
docs2 <- out$documents
meta2 <- out$meta

## we're going to include china 
##to measure prevalence of topics across china variable
##to do this, we specify a formula with no dependent variable.

topmod_fit <- stm(documents = docs2, vocab = vocab2, K = 8, 
                  prevalence = ~ china_party, data = meta2, max.em.its = 75, seed = 1215228)
labelTopics(topmod_fit)

##we can now plot the prevalence effects using 
##some of the tools provided in the package.
##Specifically, we set the topic that we want to examine---
##in this case three---and then the variable we're interested
prep <- estimateEffect(c(1:3) ~ china_party, topmod_fit, metadata= meta)
plot.estimateEffect(prep, 'china_party', method = 'pointestimate', xlim = c(-.1, .3), main = "Topic 1:3")

prep <- estimateEffect(c(4:5) ~ china_party, topmod_fit, metadata= meta)
plot.estimateEffect(prep, 'china_party', method = 'pointestimate', xlim = c(-.1, .3), main = "Topic 4 and 5")

prep <- estimateEffect(c(6:8) ~ china_party, topmod_fit, metadata= meta)
plot.estimateEffect(prep, 'china_party', method = 'pointestimate', xlim = c(-.1, .3), width = 40, main = "Topic 6:8")
