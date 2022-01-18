rm(list=ls())
setwd("C:/Users/grant/Desktop/Desktop/Assignment 4/")

load("sentiment_df_AFINN.Rdata")
df_AFINN <- df1 

load("sentiment_df_Hu & Liu.Rdata")
df_HuLiu <- df1 

load("sentiment_df_AFINN_neg_count.Rdata")
df_AFINN_neg_count <- df1

names(df_AFINN)[names(df_AFINN) == "difference"] <- "dif_AFINN"
names(df_HuLiu)[names(df_HuLiu) == "difference"] <- "dif_HiuLiu"

#merge three data as_AFINN_HuLiu

df_AFINN_HuLiu <- cbind(df_AFINN, df_HuLiu$dif_HiuLiu, df_HuLiu$neg_count, df_AFINN_neg_count$AFINN_neg_count) 


names(df_AFINN_HuLiu)[names(df_AFINN_HuLiu) == "df_HuLiu$dif_HiuLiu"] <- "dif_HiuLiu"
names(df_AFINN_HuLiu)[names(df_AFINN_HuLiu) == "df_HuLiu$neg_count"] <- "HuLiu_neg_count"
names(df_AFINN_HuLiu)[names(df_AFINN_HuLiu) == "df_AFINN_neg_count$AFINN_neg_count"] <- "AFINN_neg_count"

# install.packages("tidycensus")
library(tidycensus)
library(dplyr)
library(tidyr)

fips <- fips_codes
fips_state <- fips[,c(1,2)]

library(dplyr)

df_AFINN_HuLiu$fip_state_code <- fips_state$state_code[match(df_AFINN_HuLiu$categorystate, fips_state$state)]

#1)
state_stfips <- read.csv("state - stfips.csv")
main_df <- read.csv("ctyfips - stfips - zip - cd - dma.csv")
cd_zip <- read.csv("cd - zip.csv")

# strip white space for state_stfips (e.g.) "TX " --> "TX"
library(stringr)

for (i in 1:nrow(state_stfips)){
  state <- c(state_stfips$state[i])
  state <- gsub(pattern = "\\s+", replacement = "", x = state)
  state_stfips$state[i] <- state
}

#merge st_fips to main & cd_zip to main 
main_df$stfips <- state_stfips$stfips[match(main_df$state, state_stfips$state)]
main_df$cd <- cd_zip$cd[match(main_df$zip, cd_zip$zip)]

###We include a new variable "issue65_new" that re-codes 118 rows as issue65==1 based on the text transcribe 
df_AFINN_HuLiu$issue65_new <- df_AFINN_HuLiu$issue65

library(stringr) 
a <- grep(pattern = "China|Chinese", x = df_AFINN_HuLiu$text)

b <- which(df_AFINN_HuLiu$issue65==1)
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
  df_AFINN_HuLiu$issue65_new[c[i]] <- 1   
}

df_AFINN_HuLiu$text[2569]

################## 1) AFINN_negcount_2006 to AFINN_negcount_2018 ################## 
df_AFINN_HuLiu$AFINN_neg_count[df_AFINN_HuLiu$issue65_new!= 1] <- 0 
df_AFINN_HuLiu$AFINN_neg_count[is.na(df_AFINN_HuLiu$issue65_new)] <- 0 

avg_AFINN_negcount <- df_AFINN_HuLiu %>% group_by(dma,year) %>% dplyr::summarise(AFINN_neg_count = mean(AFINN_neg_count, na.rm = FALSE))
avg_AFINN_negcount <- pivot_wider(avg_AFINN_negcount, names_from = year, values_from = AFINN_neg_count, names_prefix = "avg_AFINN_negcount_")

main_df$avg_AFINN_negcount_2006 <- avg_AFINN_negcount$avg_AFINN_negcount_2006[match(main_df$dma, avg_AFINN_negcount$dma)]
main_df$avg_AFINN_negcount_2010 <- avg_AFINN_negcount$avg_AFINN_negcount_2010[match(main_df$dma, avg_AFINN_negcount$dma)]
main_df$avg_AFINN_negcount_2012 <- avg_AFINN_negcount$avg_AFINN_negcount_2012[match(main_df$dma, avg_AFINN_negcount$dma)]
main_df$avg_AFINN_negcount_2014 <- avg_AFINN_negcount$avg_AFINN_negcount_2014[match(main_df$dma, avg_AFINN_negcount$dma)]
main_df$avg_AFINN_negcount_2016 <- avg_AFINN_negcount$avg_AFINN_negcount_2016[match(main_df$dma, avg_AFINN_negcount$dma)]
main_df$avg_AFINN_negcount_2018 <- avg_AFINN_negcount$avg_AFINN_negcount_2018[match(main_df$dma, avg_AFINN_negcount$dma)]



################# 2) HuLiu_negcount_2006 to HuLiu_negcount_2018 ################# 
df_AFINN_HuLiu$HuLiu_neg_count[df_AFINN_HuLiu$issue65_new!= 1] <- 0 
df_AFINN_HuLiu$HuLiu_neg_count[is.na(df_AFINN_HuLiu$issue65_new)] <- 0 

avg_HuLiu_negcount <- df_AFINN_HuLiu %>% group_by(dma,year) %>% dplyr::summarise(avg_HuLiu_negcount = mean(HuLiu_neg_count, na.rm = FALSE))

avg_HuLiu_negcount <- pivot_wider(avg_HuLiu_negcount, names_from = year, values_from = avg_HuLiu_negcount, names_prefix = "avg_HuLiu_negcount_")

main_df$avg_HuLiu_negcount_2006 <- avg_HuLiu_negcount$avg_HuLiu_negcount_2006[match(main_df$dma, avg_HuLiu_negcount$dma)]
main_df$avg_HuLiu_negcount_2010 <- avg_HuLiu_negcount$avg_HuLiu_negcount_2010[match(main_df$dma, avg_HuLiu_negcount$dma)]
main_df$avg_HuLiu_negcount_2012 <- avg_HuLiu_negcount$avg_HuLiu_negcount_2012[match(main_df$dma, avg_HuLiu_negcount$dma)]
main_df$avg_HuLiu_negcount_2014 <- avg_HuLiu_negcount$avg_HuLiu_negcount_2014[match(main_df$dma, avg_HuLiu_negcount$dma)]
main_df$avg_HuLiu_negcount_2016 <- avg_HuLiu_negcount$avg_HuLiu_negcount_2016[match(main_df$dma, avg_HuLiu_negcount$dma)]
main_df$avg_HuLiu_negcount_2018 <- avg_HuLiu_negcount$avg_HuLiu_negcount_2018[match(main_df$dma, avg_HuLiu_negcount$dma)]


################# 4) count_neg_emotion_2006 to count_neg_emotion_2018 ##################
df_AFINN_HuLiu$emotion[df_AFINN_HuLiu$issue65_new == 0] <- 0 
df_AFINN_HuLiu$emotion[is.na(df_AFINN_HuLiu$issue65_new)] <- 0 

# average total negative scores for negative emotions (anger, fear, sadness) for all china ads for each DMA 
count_neg_emotion <- df_AFINN_HuLiu %>% group_by(dma,year) %>% dplyr::summarise(avg_count_neg_emotion = mean(emotion, na.rm =FALSE))
count_neg_emotion <- pivot_wider(count_neg_emotion, names_from = year, values_from = avg_count_neg_emotion, names_prefix = "avg_count_neg_emotion")

main_df$avg_count_neg_emotion_2006 <- count_neg_emotion$avg_count_neg_emotion2006[match(main_df$dma, count_neg_emotion$dma)]
main_df$avg_count_neg_emotion_2010 <- count_neg_emotion$avg_count_neg_emotion2010[match(main_df$dma, count_neg_emotion$dma)]
main_df$avg_count_neg_emotion_2012 <- count_neg_emotion$avg_count_neg_emotion2012[match(main_df$dma, count_neg_emotion$dma)]
main_df$avg_count_neg_emotion_2014 <- count_neg_emotion$avg_count_neg_emotion2014[match(main_df$dma, count_neg_emotion$dma)]
main_df$avg_count_neg_emotion_2016 <- count_neg_emotion$avg_count_neg_emotion2016[match(main_df$dma, count_neg_emotion$dma)]
main_df$avg_count_neg_emotion_2018 <- count_neg_emotion$avg_count_neg_emotion2018[match(main_df$dma, count_neg_emotion$dma)]



############### 5) avg_AFINN_2006 to avg_AFINN_2018 ############### 
df_AFINN_HuLiu$dif_AFINN[df_AFINN_HuLiu$issue65_new == 0] <- 0 
df_AFINN_HuLiu$dif_AFINN[is.na(df_AFINN_HuLiu$issue65_new)] <- 0 

AFINN_df_dma <- df_AFINN_HuLiu %>% group_by(dma,year) %>% dplyr::summarise(avg_AFINN = mean(dif_AFINN, na.rm = FALSE))
AFINN_df_dma <- pivot_wider(AFINN_df_dma, names_from = year, values_from = avg_AFINN, names_prefix = "avg_AFINN_")

main_df$avg_AFINN_2006 <- AFINN_df_dma$avg_AFINN_2006[match(main_df$dma, AFINN_df_dma$dma)]
main_df$avg_AFINN_2010 <- AFINN_df_dma$avg_AFINN_2010[match(main_df$dma, AFINN_df_dma$dma)]
main_df$avg_AFINN_2012 <- AFINN_df_dma$avg_AFINN_2012[match(main_df$dma, AFINN_df_dma$dma)]
main_df$avg_AFINN_2014 <- AFINN_df_dma$avg_AFINN_2014[match(main_df$dma, AFINN_df_dma$dma)]
main_df$avg_AFINN_2016 <- AFINN_df_dma$avg_AFINN_2016[match(main_df$dma, AFINN_df_dma$dma)]
main_df$avg_AFINN_2018 <- AFINN_df_dma$avg_AFINN_2018[match(main_df$dma, AFINN_df_dma$dma)]


############## 6) avg_HuLiu_2006 to avg_HuLiu_2018 ##############
df_AFINN_HuLiu$dif_HiuLiu[df_AFINN_HuLiu$issue65_new == 0] <- 0 
df_AFINN_HuLiu$dif_HiuLiu[is.na(df_AFINN_HuLiu$issue65_new)] <- 0 

HuLiu_df_dma <- df_AFINN_HuLiu %>% group_by(dma,year) %>% dplyr::summarise(avg_HuLiu = mean(dif_HiuLiu, na.rm = FALSE))
HuLiu_df_dma <- pivot_wider(HuLiu_df_dma, names_from = year, values_from = avg_HuLiu, names_prefix = "avg_HuLiu_")

main_df$avg_HuLiu_2006 <- HuLiu_df_dma$avg_HuLiu_2006[match(main_df$dma, HuLiu_df_dma$dma)]
main_df$avg_HuLiu_2010 <- HuLiu_df_dma$avg_HuLiu_2010[match(main_df$dma, HuLiu_df_dma$dma)]
main_df$avg_HuLiu_2012 <- HuLiu_df_dma$avg_HuLiu_2012[match(main_df$dma, HuLiu_df_dma$dma)]
main_df$avg_HuLiu_2014 <- HuLiu_df_dma$avg_HuLiu_2014[match(main_df$dma, HuLiu_df_dma$dma)]
main_df$avg_HuLiu_2016 <- HuLiu_df_dma$avg_HuLiu_2016[match(main_df$dma, HuLiu_df_dma$dma)]
main_df$avg_HuLiu_2018 <- HuLiu_df_dma$avg_HuLiu_2018[match(main_df$dma, HuLiu_df_dma$dma)]

############# 7) count_china_2006 to count_china_2018 ############

count_china <- df_AFINN_HuLiu  %>% group_by(dma,year) %>% tally(issue65_new ==1)

count_china <- pivot_wider(count_china, names_from = year, values_from = n, names_prefix = "count_")

main_df$count_china_2006 <- count_china$count_2006[match(main_df$dma, count_china$dma)]
main_df$count_china_2010 <- count_china$count_2010[match(main_df$dma, count_china$dma)]
main_df$count_china_2012 <- count_china$count_2012[match(main_df$dma, count_china$dma)]
main_df$count_china_2014 <- count_china$count_2014[match(main_df$dma, count_china$dma)]
main_df$count_china_2016 <- count_china$count_2016[match(main_df$dma, count_china$dma)]
main_df$count_china_2018 <- count_china$count_2018[match(main_df$dma, count_china$dma)]


# We get the total for all 
library(dplyr)

main_df <- main_df %>%
  mutate(AFINN_negcount_total = select(.,avg_AFINN_negcount_2006,
                                       avg_AFINN_negcount_2010,
                                       avg_AFINN_negcount_2012,
                                       avg_AFINN_negcount_2014,
                                       avg_AFINN_negcount_2016,
                                       avg_AFINN_negcount_2018
  ) %>% rowSums(na.rm = TRUE))

main_df <- main_df %>%
  mutate(HuLiu_negcount_total = select(.,avg_HuLiu_negcount_2006,
                                       avg_HuLiu_negcount_2010,
                                       avg_HuLiu_negcount_2012,
                                       avg_HuLiu_negcount_2014,
                                       avg_HuLiu_negcount_2016,
                                       avg_HuLiu_negcount_2018
  ) %>% rowSums(na.rm = TRUE))



main_df <- main_df %>%
  mutate(count_neg_emotion_total = select(.,avg_count_neg_emotion_2006,
                                          avg_count_neg_emotion_2010,
                                          avg_count_neg_emotion_2012,
                                          avg_count_neg_emotion_2014,
                                          avg_count_neg_emotion_2016,
                                          avg_count_neg_emotion_2018
  ) %>% rowSums(na.rm = TRUE))

main_df <- main_df %>%
  mutate(count_china_total = select(.,count_china_2006,
                                    count_china_2010,
                                    count_china_2012,
                                    count_china_2014,
                                    count_china_2016,
                                    count_china_2018,
  ) %>% rowSums(na.rm = TRUE))

main_df <- main_df %>%
  mutate(avg_AFINN_total = select(.,avg_AFINN_2006,
                                  avg_AFINN_2010,
                                  avg_AFINN_2012,
                                  avg_AFINN_2014,
                                  avg_AFINN_2016,
                                  avg_AFINN_2018,
  ) %>% rowSums(na.rm = TRUE))

main_df <- main_df %>%
  mutate(avg_HuLiu_total = select(.,avg_HuLiu_2006,
                                  avg_HuLiu_2010,
                                  avg_HuLiu_2012,
                                  avg_HuLiu_2014,
                                  avg_HuLiu_2016,
                                  avg_HuLiu_2018,
  ) %>% rowSums(na.rm = TRUE))

save(main_df, file = "merge_df_New_issue65.Rdata")
