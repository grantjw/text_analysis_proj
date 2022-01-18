rm(list=ls())

library(dplyr)
library(tidyr)
library(stargazer)
setwd("C:/Users/grant/Desktop/Desktop/Sentiment Analysis_New_issue65/")
load("text_analysis_Weslyan_Ad.Rdata")
 
###CLEAN DATA###
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

### "issue65_new" instead of "issue_65" ### 
### We include a new variable "issue65_new" that re-codes 118 rows as issue65==1 based on the text transcribe 
df1$issue65_new <- df1$issue65

library(stringr) 
a <- grep(pattern = "China|Chinese", x = df1$text)

b <- which(df1$issue65==1)
`%notin%` <- Negate(`%in%`)

c <- c()
for(i in 1:a){
  if(a[i] %notin% b){
    c[i] <- print(a[i])
  }
}
summary(!is.na(c)) # we can tell that there are 118 values that are not na. 118 values are the indices that are not coded as issue65==1 
c <- c[!is.na(c)]  # take out NA values 
print(c) # these are the indices we want to re-code as issue65==1 
c <- c[1:118]

for(i in 1:118){
  df1$issue65_new[c[i]] <- 1   
}


#Sentiment Analysis
pos <- read.delim("positivewords.txt", header = F, stringsAsFactors = F)[,1]
neg <- read.delim("negativewords.txt", header = F, stringsAsFactors = F)[,1]


# word order does not matter, "bag of words assumption" 

library(stringr)
pos_count <- c()
neg_count <- c()
ratio <- c() # ratio = positive words / positive words + negative words
difference <- c() # difference = positive words - negative words 

for (i in 1:nrow(df1)){
  text <- c(df1$text[i])
  text <- gsub(pattern = "\\W", replacement =  " ", x = text) # remove all non-alpha numeric characters 
  
  text = gsub(pattern = 'message', '', text) # remove message which is irrelevant to our analysis
  text = gsub(pattern = 'approve', '', text) # remove approve which is irrelevent to our analysis 
  text = gsub(pattern = '%HESITATION', '', text) #remove hestitation which is irrelevent to our analysis 
  
  text <- gsub(pattern = "\\s+", replacement = " ", x = text) # replace multiple space with single space
  
  text = gsub(pattern = "\n", '', text) #remove unnecessary text from ibm video to text output 
  
  text <- str_split(string = text, pattern = "\\s") # split text into vectors of individual tokens
  
  print(text)
  # ii)
  pos_count[i] <- (sum(1*as.numeric(text[[1]] %in% pos)))
  # ii) 
  neg_count[i] <- (sum(1*as.numeric(text[[1]] %in% neg)))
  
  ratio[i] <- pos_count[i] / (neg_count[i] + pos_count[i])
  difference[i] <- pos_count[i] - neg_count[i]
  
}

#add word counts to dataframe 
df1 <- cbind(pos_count,df1)
df1 <- cbind(neg_count,df1)
df1 <- cbind(ratio,df1)
df1 <- cbind(difference,df1)

# texts that do not contain any pos or neg words are computed as NaN so we change to .5 (for ratio) or 0 (for difference)
df1$ratio[is.nan(df1$ratio)] <- .5 
df1$difference[is.nan(df1$difference)] <- 0

# sentiment scores ~ emotion scores

df1$emotion <- as.numeric(as.character(df1$anger)) + as.numeric(as.character(df1$fear)) + as.numeric(as.character(df1$sadness))

cor.test(df1$emotion, df1$ratio) # significantly correlated
cor.test(df1$emotion, df1$difference) # significantly correlated
cor.test(df1$emotion, df1$neg_count) # significantly correlated

#################################################################################################
# Differences between average sentiment scores (across all election types)

# China vs. non-China ads (China ads are more negative than non-China ads)
df1 %>% group_by(issue65_new) %>% dplyr::summarise(Sent_Ratio= mean(ratio, na.rm = FALSE))
df1 %>% group_by(issue65_new) %>% dplyr::summarise(Sent_Difference= mean(difference, na.rm = FALSE))

summary(m1 <- lm(ratio ~ issue65_new, data=df1))
summary(m1 <- lm(difference ~ issue65_new, data=df1))

hist(df1$ratio)
hist(df1$difference)
table(df1$issue65_new) # of China ad = 331, non-China ad=22449

# China non-trade vs. China trade ads (no difference)
df1 %>% group_by(issue65_new, issue20) %>% dplyr::summarise(Sent_Ratio = mean(ratio, na.rm = FALSE))
df1 %>% group_by(issue65_new, issue20) %>% dplyr::summarise(Sent_Difference = mean(difference, na.rm = FALSE))

summary(m1 <- lm(ratio ~ issue20, data=df1, subset=issue65_new==1))
summary(m1 <- lm(difference ~ issue20, data=df1, subset=issue65_new==1))

# China democrat ads vs. China Republican Ads (Republican's China ads are more negative)
df1 %>% group_by(issue65_new,party) %>%
  #filter(party == 2) %>%
  dplyr::summarise(Sent_Ratio = mean(ratio, na.rm = FALSE))

df1 %>% group_by(issue65_new,party) %>%
  #filter(party == 2) %>%
  dplyr::summarise(Sent_Difference = mean(difference, na.rm = FALSE))

summary(m1 <- lm(ratio ~ party, data=df1, subset=issue65_new==1))
summary(m1 <- lm(difference ~ party, data=df1, subset=issue65_new==1))


#################################################################################################
# Differences between average sentiment scores (by each election type)

table(df1$race)

df1$electype[df1$race=="GOV" | df1$race=="Governor" | df1$race=="GOVERNOR"] <- "gov"
df1$electype[df1$race=="HOUSE" | df1$race=="US House" | df1$race=="US HOUSE"] <- "house"
df1$electype[df1$race=="USSEN" | df1$race=="US Senate" | df1$race=="US SENATE"] <- "senate"
df1$electype[df1$race=="PRESIDENT"] <- "pres"

table(df1$electype)

## China vs. non-China ads

df1 %>% group_by(electype, issue65_new) %>% dplyr::summarise(Sent_Ratio= mean(ratio, na.rm = FALSE))
df1 %>% group_by(electype, issue65_new) %>% dplyr::summarise(Sent_Difference= mean(difference, na.rm = FALSE))

summary(m1 <- lm(ratio ~ issue65_new, data=df1, subset=electype=="gov"))
summary(m1 <- lm(difference ~ issue65_new, data=df1, subset=electype=="gov")) # p=0.053
summary(m1 <- lm(ratio ~ issue65_new, data=df1, subset=electype=="house")) # p=0.009
summary(m1 <- lm(difference ~ issue65_new, data=df1, subset=electype=="house")) # p=0.053
summary(m1 <- lm(ratio ~ issue65_new, data=df1, subset=electype=="senate"))
summary(m1 <- lm(difference ~ issue65_new, data=df1, subset=electype=="senate"))
summary(m1 <- lm(ratio ~ issue65_new, data=df1, subset=electype=="pres"))
summary(m1 <- lm(difference ~ issue65_new, data=df1, subset=electype=="pres"))

# China democrat ads vs. China Republican Ads

df1 %>% group_by(electype, issue65_new,party) %>%
  #filter(party == 2) %>%
  dplyr::summarise(Sent_Ratio = mean(ratio, na.rm = FALSE))


df1 %>% group_by(electype, issue65_new,party) %>%
  #filter(party == 2) %>%
  dplyr::summarise(Sent_Difference = mean(difference, na.rm = FALSE))

#################################################################################################
# Differences between average sentiment scores (by each year)

df1$year[df1$airdate>="2005-01-01" & df1$airdate<="2006-12-12"] <- "2006"
df1$year[df1$airdate>="2009-01-01" & df1$airdate<="2010-12-12"] <- "2010"
df1$year[df1$airdate>="2011-01-01" & df1$airdate<="2012-12-12"] <- "2012"
df1$year[df1$airdate>="2013-01-01" & df1$airdate<="2014-12-12"] <- "2014"
df1$year[df1$airdate>="2015-01-01" & df1$airdate<="2016-12-12"] <- "2016"
df1$year[df1$airdate>="2017-01-01" & df1$airdate<="2018-12-12"] <- "2018"

table(df1$year)

df1 %>% group_by(electype, year, issue65_new, party) %>%
  #filter(party == 2) %>%
  dplyr::summarise(Sent_Ratio = mean(ratio, na.rm = FALSE))


df1 %>% group_by(electype, year, issue65_new,party) %>%
  #filter(party == 2) %>%
  dplyr::summarise(Sent_Difference = mean(difference, na.rm = FALSE))

save(df1, file = "sentiment_df_HuLiu_new_issue65.Rdata") #save the data to plot later 

