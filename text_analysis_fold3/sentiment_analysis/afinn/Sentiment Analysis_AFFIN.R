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



# word order does not matter, "bag of words assumption" 

library(stringr)
ratio <- c() # ratio = positive words / positive words + negative words
sent_count <- c()
score <- c()
difference <- c()

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
      score[n] <- sent$value[sent$word %in% text[[1]][where[n]]]
    } 
    else {
      score <- 0
    }
  }
  difference[i] <- sum(score)
}



#add word counts to dataframe 
df1 <- cbind(difference,df1)

# texts that do not contain any pos or neg words are computed as NaN so we change to .5 (for ratio) or 0 (for difference)
df1$difference[is.nan(df1$difference)] <- 0



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

df1$emotion <- as.numeric(as.character(df1$anger)) + as.numeric(as.character(df1$fear)) + as.numeric(as.character(df1$sadness))

cor.test(df1$emotion, df1$difference) #significantly correlated 

#################################################################################################
# Differences between average sentiment scores (across all election types)

# China vs. non-China ads (China ads are more negative than non-China ads)
df1 %>% group_by(issue65) %>% dplyr::summarise(Sent_Difference= mean(difference, na.rm = FALSE))

summary(m1 <- lm(difference ~ issue65, data=df1))

hist(df1$difference)
table(df1$issue65) # of China ad = 331, non-China ad=22449

# China non-trade vs. China trade ads (no difference)
df1 %>% group_by(issue65, issue20) %>% dplyr::summarise(Sent_Difference = mean(difference, na.rm = FALSE))

summary(m1 <- lm(difference ~ issue20, data=df1, subset=issue65==1))

# China democrat ads vs. China Republican Ads (Republican's China ads are more negative)
df1 %>% group_by(issue65,party) %>%
  #filter(party == 2) %>%
  dplyr::summarise(Sent_Difference = mean(difference, na.rm = FALSE))

summary(m1 <- lm(difference ~ party, data=df1, subset=issue65==1))


#################################################################################################
# Differences between average sentiment scores (by each election type)


## China vs. non-China ads

df1 %>% group_by(electype, issue65) %>% dplyr::summarise(Sent_Difference= mean(difference, na.rm = FALSE))

summary(m1 <- lm(difference ~ issue65, data=df1, subset=electype=="gov")) # p=0.053
summary(m1 <- lm(difference ~ issue65, data=df1, subset=electype=="house")) # p=0.053
summary(m1 <- lm(difference ~ issue65, data=df1, subset=electype=="senate"))
summary(m1 <- lm(difference ~ issue65, data=df1, subset=electype=="pres"))

# China democrat ads vs. China Republican Ads

df1 %>% group_by(electype, issue65,party) %>%
  #filter(party == 2) %>%
  dplyr::summarise(Sent_Difference = mean(difference, na.rm = FALSE))

#################################################################################################
# Differences between average sentiment scores (by each year)

df1 %>% group_by(electype, year, issue65,party) %>%
  #filter(party == 2) %>%
  dplyr::summarise(Sent_Difference = mean(difference, na.rm = FALSE))


save(df1, file = "sentiment_df_AFINN.Rdata")


