df_Dem <- data.frame(matrix(ncol = 6, nrow = 11))
x <- c("2006", "2010", "2012", "2014", "2016", "2018")
y <- c("Total", "China", "Trade", "Anger", "Anger_Trade", "Fear", "Fear_Trade", "Sadness", "Sadness_Trade", "Anger + Fear + Sad","Anger + Fear + Sad_Trade")

colnames(df_Dem) <- x
rownames(df_Dem) <- y

df_Rep <- data.frame(matrix(ncol = 6, nrow = 11))
x <- c("2006", "2010", "2012", "2014", "2016", "2018")
y <- c("Total", "China", "Trade", "Anger", "Anger_Trade", "Fear", "Fear_Trade", "Sadness", "Sadness_Trade", "Anger + Fear + Sad","Anger + Fear + Sad_Trade")

colnames(df_Rep) <- x
rownames(df_Rep) <- y


library(haven)
library(dplyr)
memory.limit(100000)
gov_2006 <- Gov_2006_f 
gov_2010 <- Gov_2010_f 
gov_2012 <- Gov_2012_f 
gov_2014 <- Gov_2014_f 
gov_2016 <- Gov_2016_f 
gov_2018 <- Gov_2018_f 

Gov_2010_f$party[c("Democrat")] <- c(1)

#Total 

#2006 
a <- gov_2006  %>% group_by(party, issue65) %>%
                   filter(issue65 %in% c(0,1)) %>%
                   filter(party == 1) %>% tally()
                   df_Dem["Total","2006"] <- sum(a$n)

a <- gov_2006  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 2) %>% tally()
df_Rep["Total","2006"] <- sum(a$n)

#2010 
a <- gov_2010  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 1) %>% tally()
df_Dem["Total","2010"] <- sum(a$n)

a <- gov_2010  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 2) %>% tally()
df_Rep["Total","2010"] <- sum(a$n)

#2012 
a <- gov_2012 %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 1) %>% tally()
df_Dem["Total","2012"] <- sum(a$n)

a <- gov_2012 %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 2) %>% tally()
df_Rep["Total","2012"] <- sum(a$n)

#2014 
a <- gov_2014  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 1) %>% tally()
df_Dem["Total","2014"] <- sum(a$n)

a <- gov_2014  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 2) %>% tally()
df_Rep["Total","2014"] <- sum(a$n)

#2016 
a <- gov_2016  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 1) %>% tally()
df_Dem["Total","2016"] <- sum(a$n)

a <- gov_2016  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 2) %>% tally()
df_Rep["Total","2016"] <- sum(a$n)

#2018 
a <- gov_2018  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 1) %>% tally()
df_Dem["Total","2018"] <- sum(a$n)

a <- gov_2018  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(0,1)) %>%
  filter(party == 2) %>% tally()
df_Rep["Total","2018"] <- sum(a$n)

#####China#### 

#2006 
a <- gov_2006  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>% tally()
df_Dem["China","2006"] <- sum(a$n)

a <- gov_2006  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>% tally()
df_Rep["China","2006"] <- sum(a$n)

#2010 
a <- gov_2010  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>% tally()
df_Dem["China","2010"] <- sum(a$n)

a <- gov_2010  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>% tally()
df_Rep["China","2010"] <- sum(a$n)

#2012 
a <- gov_2012 %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>% tally()
df_Dem["China","2012"] <- sum(a$n)

a <- gov_2012 %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>% tally()
df_Rep["China","2012"] <- sum(a$n)

#2014 
a <- gov_2014  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>% tally()
df_Dem["China","2014"] <- sum(a$n)

a <- gov_2014  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>% tally()
df_Rep["China","2014"] <- sum(a$n)

#2016 
a <- gov_2016  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>% tally()
df_Dem["China","2016"] <- sum(a$n)

a <- gov_2016  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>% tally()
df_Rep["China","2016"] <- sum(a$n)

#2018 
a <- gov_2018  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>% tally()
df_Dem["China","2018"] <- sum(a$n)

a <- gov_2018  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>% tally()
df_Rep["China","2018"] <- sum(a$n)


######TRADE########
#2006 
a <- gov_2006  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>% 
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Dem["Trade","2006"] <- sum(a$n)

a <- gov_2006  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>%
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Rep["Trade","2006"] <- sum(a$n)

#2010 
a <- gov_2010  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>% 
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Dem["Trade","2010"] <- sum(a$n)

a <- gov_2010  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>%
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Rep["Trade","2010"] <- sum(a$n)

#2012 
a <- gov_2012 %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>%
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Dem["Trade","2012"] <- sum(a$n)

a <- gov_2012 %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>%
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Rep["Trade","2012"] <- sum(a$n)

#2014 
a <- gov_2014  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>%
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Dem["Trade","2014"] <- sum(a$n)

a <- gov_2014  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>%
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Rep["Trade","2014"] <- sum(a$n)

#2016 
a <- gov_2016  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>%
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Dem["Trade","2016"] <- sum(a$n)

a <- gov_2016  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>%
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Rep["Trade","2016"] <- sum(a$n)

#2018 
a <- gov_2018  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 1) %>%
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Dem["Trade","2018"] <- sum(a$n)

a <- gov_2018  %>% group_by(party, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(party == 2) %>%
  filter(issue20 %in% c(1)) %>% 
  tally()
df_Rep["Trade","2018"] <- sum(a$n)

################################################ANGER############################################
################################################ANGER############################################
################################################ANGER############################################

#2006 
a <- gov_2006  %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger","2006"] <- sum(a$n)

a <- gov_2006  %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger","2006"] <- sum(a$n)

#2010 
a <- gov_2010  %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger","2010"] <- sum(a$n)

a <- gov_2010  %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger","2010"] <- sum(a$n)

#2012 
a <- gov_2012 %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger","2012"] <- sum(a$n)

a <- gov_2012 %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger","2012"] <- sum(a$n)

#2014 
a <- gov_2014  %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger","2014"] <- sum(a$n)

a <- gov_2014  %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger","2014"] <- sum(a$n)

#2016 
a <- gov_2016  %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger","2016"] <- sum(a$n)

a <- gov_2016  %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger","2016"] <- sum(a$n)

#2018 
a <- gov_2018  %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger","2018"] <- sum(a$n)

a <- gov_2018  %>% group_by(party, anger, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger","2018"] <- sum(a$n)

############################################ANGER TRADE###################################################################
############################################ANGER TRADE###################################################################
############################################ANGER TRADE###################################################################
#2006 
a <- gov_2006  %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger_Trade","2006"] <- sum(a$n)

a <- gov_2006  %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger_Trade","2006"] <- sum(a$n)

#2010 
a <- gov_2010  %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger_Trade","2010"] <- sum(a$n)

a <- gov_2010  %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger_Trade","2010"] <- sum(a$n)

#2012 
a <- gov_2012 %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger_Trade","2012"] <- sum(a$n)

a <- gov_2012 %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger_Trade","2012"] <- sum(a$n)

#2014 
a <- gov_2014  %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger_Trade","2014"] <- sum(a$n)

a <- gov_2014  %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger_Trade","2014"] <- sum(a$n)

#2016 
a <- gov_2016  %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger_Trade","2016"] <- sum(a$n)

a <- gov_2016  %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger_Trade","2016"] <- sum(a$n)

#2018 
a <- gov_2018  %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Anger_Trade","2018"] <- sum(a$n)

a <- gov_2018  %>% group_by(party, anger, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(anger %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Anger_Trade","2018"] <- sum(a$n)

################################################fear############################################
################################################fear############################################
################################################fear############################################

#2006 
a <- gov_2006  %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear","2006"] <- sum(a$n)

a <- gov_2006  %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear","2006"] <- sum(a$n)

#2010 
a <- gov_2010  %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear","2010"] <- sum(a$n)

a <- gov_2010  %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear","2010"] <- sum(a$n)

#2012 
a <- gov_2012 %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear","2012"] <- sum(a$n)

a <- gov_2012 %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear","2012"] <- sum(a$n)

#2014 
a <- gov_2014  %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear","2014"] <- sum(a$n)

a <- gov_2014  %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear","2014"] <- sum(a$n)

#2016 
a <- gov_2016  %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear","2016"] <- sum(a$n)

a <- gov_2016  %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear","2016"] <- sum(a$n)

#2018 
a <- gov_2018  %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear","2018"] <- sum(a$n)

a <- gov_2018  %>% group_by(party, fear, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear","2018"] <- sum(a$n)

############################################fear TRADE###################################################################
############################################fear TRADE###################################################################
############################################fear TRADE###################################################################
#2006 
a <- gov_2006  %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear_Trade","2006"] <- sum(a$n)

a <- gov_2006  %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear_Trade","2006"] <- sum(a$n)

#2010 
a <- gov_2010  %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear_Trade","2010"] <- sum(a$n)

a <- gov_2010  %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear_Trade","2010"] <- sum(a$n)

#2012 
a <- gov_2012 %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear_Trade","2012"] <- sum(a$n)

a <- gov_2012 %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear_Trade","2012"] <- sum(a$n)

#2014 
a <- gov_2014  %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear_Trade","2014"] <- sum(a$n)

a <- gov_2014  %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear_Trade","2014"] <- sum(a$n)

#2016 
a <- gov_2016  %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear_Trade","2016"] <- sum(a$n)

a <- gov_2016  %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear_Trade","2016"] <- sum(a$n)

#2018 
a <- gov_2018  %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Fear_Trade","2018"] <- sum(a$n)

a <- gov_2018  %>% group_by(party, fear, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(fear %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Fear_Trade","2018"] <- sum(a$n)

#######################################SADNESS##########################################
#######################################SADNESS##########################################
#######################################SADNESS##########################################

#2006 
a <- gov_2006  %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness","2006"] <- sum(a$n)

a <- gov_2006  %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness","2006"] <- sum(a$n)

#2010 
a <- gov_2010  %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness","2010"] <- sum(a$n)

a <- gov_2010  %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness","2010"] <- sum(a$n)

#2012 
a <- gov_2012 %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness","2012"] <- sum(a$n)

a <- gov_2012 %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness","2012"] <- sum(a$n)

#2014 
a <- gov_2014  %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness","2014"] <- sum(a$n)

a <- gov_2014  %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness","2014"] <- sum(a$n)

#2016 
a <- gov_2016  %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness","2016"] <- sum(a$n)

a <- gov_2016  %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness","2016"] <- sum(a$n)

#2018 
a <- gov_2018  %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness","2018"] <- sum(a$n)

a <- gov_2018  %>% group_by(party, sadness, issue65) %>%
  filter(issue65 %in% c(1)) %>%
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness","2018"] <- sum(a$n)

############################################Sadness TRADE###################################################################
############################################Sadness TRADE###################################################################
############################################Sadness TRADE###################################################################
#2006 
a <- gov_2006  %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness_Trade","2006"] <- sum(a$n)

a <- gov_2006  %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness_Trade","2006"] <- sum(a$n)

#2010 
a <- gov_2010  %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness_Trade","2010"] <- sum(a$n)

a <- gov_2010  %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness_Trade","2010"] <- sum(a$n)

#2012 
a <- gov_2012 %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness_Trade","2012"] <- sum(a$n)

a <- gov_2012 %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness_Trade","2012"] <- sum(a$n)

#2014 
a <- gov_2014  %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness_Trade","2014"] <- sum(a$n)

a <- gov_2014  %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness_Trade","2014"] <- sum(a$n)

#2016 
a <- gov_2016  %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness_Trade","2016"] <- sum(a$n)

a <- gov_2016  %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness_Trade","2016"] <- sum(a$n)

#2018 
a <- gov_2018  %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 1) %>% tally()
df_Dem["Sadness_Trade","2018"] <- sum(a$n)

a <- gov_2018  %>% group_by(party, sadness, issue65, issue20) %>%
  filter(issue65 %in% c(1) & issue20 %in% c(1))  %>% 
  filter(sadness %in% c(1,2)) %>%
  filter(party == 2) %>% tally()
df_Rep["Sadness_Trade","2018"] <- sum(a$n)


###############################ANGER/FEAR/SAD##########################
###############################ANGER/FEAR/SAD##########################
###############################ANGER/FEAR/SAD##########################

# count either anger or fear or sadness 

# gov_2006 
a <- gov_2006 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad","2006"] <- sum(a$n)

a <- gov_2006 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
sum(a$n)
df_Rep["Anger + Fear + Sad","2006"] <- sum(a$n)


# gov_2010 
a <- gov_2010 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad","2010"] <- sum(a$n)

a <- gov_2010 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
sum(a$n)
df_Rep["Anger + Fear + Sad","2010"] <- sum(a$n)

# gov_2012 
a <- gov_2012 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad","2012"] <- sum(a$n)

a <- gov_2012 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
sum(a$n)
df_Rep["Anger + Fear + Sad","2012"] <- sum(a$n)


# gov_2014 
a <- gov_2014 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad","2014"] <- sum(a$n)

a <- gov_2014 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
sum(a$n)
df_Rep["Anger + Fear + Sad","2014"] <- sum(a$n)


# gov_2016 
a <- gov_2016 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad","2016"] <- sum(a$n)

a <- gov_2016 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
sum(a$n)
df_Rep["Anger + Fear + Sad","2016"] <- sum(a$n)

# gov_2018  
a <- gov_2018 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad","2018"] <- sum(a$n)

a <- gov_2018 %>% 
  filter(issue65 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
sum(a$n)
df_Rep["Anger + Fear + Sad","2018"] <- sum(a$n)


###############################ANGER/FEAR/SAD _ Trade##########################
###############################ANGER/FEAR/SAD _ Trade ##########################
###############################ANGER/FEAR/SAD _ Trade##########################

# gov_2006 
a <- gov_2006 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad_Trade","2006"] <- sum(a$n)

a <- gov_2006 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
sum(a$n)
df_Rep["Anger + Fear + Sad_Trade","2006"] <- sum(a$n)


# gov_2010 
a <- gov_2010 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad_Trade","2010"] <- sum(a$n)

a <- gov_2010 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
sum(a$n)
df_Rep["Anger + Fear + Sad_Trade","2010"] <- sum(a$n)

# gov_2012 
a <- gov_2012 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad_Trade","2012"] <- sum(a$n)

a <- gov_2012 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 

df_Rep["Anger + Fear + Sad_Trade","2012"] <- sum(a$n)


# gov_2014 
a <- gov_2014 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad_Trade","2014"] <- sum(a$n)

a <- gov_2014 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Rep["Anger + Fear + Sad_Trade","2014"] <- sum(a$n)


# gov_2016 
a <- gov_2016 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad_Trade","2016"] <- sum(a$n)

a <- gov_2016 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Rep["Anger + Fear + Sad_Trade","2016"] <- sum(a$n)

# gov_2018  
a <- gov_2018 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 1)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Dem["Anger + Fear + Sad_Trade","2018"] <- sum(a$n)

a <- gov_2018 %>% 
  filter(issue65 %in% c(1)) %>% filter(issue20 %in% c(1)) %>% 
  group_by(party, anger,fear, sadness, issue65) %>% tally()

a <- a %>% filter(party == 2)  %>% filter(anger %in% c(1,2)| 
                                            fear %in% c(1,2)| 
                                            sadness %in% c(1,2)) 
df_Rep["Anger + Fear + Sad_Trade","2018"] <- sum(a$n)
