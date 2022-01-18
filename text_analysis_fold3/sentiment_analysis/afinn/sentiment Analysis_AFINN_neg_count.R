# install.packages("tidytext")
# install.packages("textdata")

rm(list=ls())

library(dplyr)
library(tidyr)
library(stargazer)
setwd("C:/Users/grant/Desktop/Desktop/Sentiment Analysis/")
load("text_analysis_Weslyan_Ad.Rdata")
unique(df$party)

#allow dplyr to show all rows 
options(dplyr.print_max = 1e9)

# drop parties other than Dem or Rep 
df1 <- df[df$party == 1 | df$party == 2,]
unique(df1$party) # Now we only have Dem or Rep 

sum(is.na(df1$text)) #How many NA 
sum(!is.na(df1$text))#How many non-NA  

df1 <- df1 %>% drop_na(text) #drop NA 

sum(is.na(df1$text)) #How many NA 
sum(!is.na(df1$text))#How many non-NA  


library(tidytext)
sent <- get_sentiments("afinn")

#change word value to -1 for -5 to -1 and 0 for 0 to 5 
sent <- sent %>% mutate(count_value =
                        case_when(value == -5 ~ 1, 
                                  value == -4 ~ 1,
                                  value == -3 ~ 1,
                                  value == -2 ~ 1,
                                  value == -1 ~ 1,
                                  value == 1 ~ 0,
                                  value == 2 ~ 0,
                                  value == 3 ~ 0,
                                  value == 4 ~ 0,
                                  value == 5 ~ 0,)
)


# word order does not matter, "bag of words assumption" 

library(stringr)
ratio <- c() # ratio = positive words / positive words + negative words
sent_count <- c()
score <- c()
AFINN_neg_count <- c()

for (i in 1:nrow(df1)){
  text <- c(df1$text[i])
  text <- gsub(pattern = "\\W", replacement =  " ", x = text) # remove all non-alpha numeric characters 
  
  text = gsub(pattern = 'message', '', text) # remove message which is irrelevant to our analysis
  text = gsub(pattern = 'approve', '', text) # remove approve which is irrelevent to our analysis 
  
  text <- gsub(pattern = "\\s+", replacement = " ", x = text) # replace multiple space with single space
  
  text = gsub(pattern = "\n", '', text) #remove unnecessary text from ibm video to text output 
  
  text <- str_split(string = text, pattern = "\\s") # split text into vectors of individual tokens
  
  where <- which(text[[1]] %in% sent$word) 
  
  for (n in 1:length(where)){
    if (sum(where) != 0){
      score[n] <- sent$count_value[sent$word %in% text[[1]][where[n]]]
    } 
    else {
      score <- 0
    }
  }
  AFINN_neg_count[i] <- sum(score)
}



#add word counts to dataframe 
df1 <- cbind(AFINN_neg_count,df1)

# texts that do not contain any pos or neg words are computed as NaN so we change 0 count
df1$AFINN_neg_count[is.nan(df1$AFINN_neg_count)] <- 0



df1$electype[df1$race=="GOV" | df1$race=="Governor" | df1$race=="GOVERNOR"] <- "gov"
df1$electype[df1$race=="HOUSE" | df1$race=="US House" | df1$race=="US HOUSE"] <- "house"
df1$electype[df1$race=="USSEN" | df1$race=="US Senate" | df1$race=="US SENATE"] <- "senate"
df1$electype[df1$race=="PRESIDENT"] <- "pres"


df1$year[df1$airdate>="2005-01-01" & df1$airdate<="2006-12-12"] <- "2006"
df1$year[df1$airdate>="2009-01-01" & df1$airdate<="2010-12-12"] <- "2010"
df1$year[df1$airdate>="2011-01-01" & df1$airdate<="2012-12-12"] <- "2012"
df1$year[df1$airdate>="2013-01-01" & df1$airdate<="2014-12-12"] <- "2014"
df1$year[df1$airdate>="2015-01-01" & df1$airdate<="2016-12-12"] <- "2016"
df1$year[df1$airdate>="2017-01-01" & df1$airdate<="2018-12-12"] <- "2018"


# sentiment scores ~ emotion scores


save(df1, file = "sentiment_df_AFINN_neg_count.Rdata")


