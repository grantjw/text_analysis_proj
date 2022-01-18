library(readstata13)
library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)
#install.packages("reshape2")

rm(list = ls())

#2006
downballot_2006 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-downballot-2006-v1.0.dta")
federal_2006 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-federal-2006-v1.0.dta")
gov_2006 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2006-v1.0.dta")

# (1) create dataframe for graph 
a <- federal_2006 %>% group_by(airdate,issue65) %>% tally()
a$n[a$issue65 == 0] <- 0
a$n[is.na(a$issue65)] <- 0 

# (1.1) the changing proportion of China-related ads (a) over "time" 

f <- ggplot(a, aes(airdate,n))
    f + geom_line() + 
     #scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06"))) +
    labs(title = "Proportion of China-related Ads Over time", 
                                   x = "Date: Jan to Dec 2006", y= "# of China Related Issues")

# (1.2) the changing proportion of China-related ads (a) over "time" (RELEVANT DATES ONLY) 
ggplot(a, aes(airdate,n)) +
  geom_line() + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06"))) +
  labs(title = "Proportion of China-related Ads Over time", 
           x = "Date: Jul to December 2006", y= "# of China Related Issues")

# (2.1) create dataframe 
b <- federal_2006 %>% group_by(airdate, categorystate, issue65) %>% tally()
b$n[b$issue65 == 0] <- 0
b$n[is.na(b$issue65)] <- 0 

# (2.1) proportion of China-related ads by "region" 
ggplot(federal_2006, aes(categorystate,issue65)) + geom_col(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~issue65, scales = "free_x")

# (2.2) Proportion of China-related ads by "region" over "date" INDIVIDUAL graph for ALL STATES 
ggplot(b, aes(x=airdate,y=n, color = categorystate)) + geom_line() +
  facet_wrap(~categorystate, scales = "free")

# (2.3) Proportion of China-related ads by "region" over "date" COMPILED graph (FOR RELEANT STATES)
b %>%
  filter(categorystate %in% c("IN","MI","NY","OH","PA","TN","WI")) %>%
  ggplot(aes(x=airdate,y=n, color = categorystate)) + geom_line() #+ 
  #facet_wrap(~categorystate, scales = "free")

# (2.4) Proportion of China-related ads by "region" over "date" INDIVIDUAL graph (FOR RELEVANT STATES ONLY)
b %>%
  filter(categorystate %in% c("IN","MI","NY","OH","PA","TN","WI")) %>%
  ggplot(aes(x=airdate,y=n, color = categorystate)) + geom_line() + 
  facet_wrap(~categorystate, scales = "free")



# (3) create dataframe 
c <- federal_2006 %>% group_by(airdate, party, issue65) %>% tally()
c$n[c$issue65 == 0] <- 0
c$n[is.na(c$issue65)] <- 0 

c$party <- haven::as_factor(c$party)

# (3.1) Proportion of China-related ads (c) by party ID
g3.2 <- ggplot(c, aes(party,n))
g3.2 + geom_col()

# (3.2) Proportion of China-related ads (C) by "Party ID" over "date" (COMPILED)
ggplot(c, aes(x=airdate, y=n, color=party)) + geom_line()

# (3.3) Proportion of China-related ads (C) by "Party ID" over "date" (INDIVIDUAL)
ggplot(c, aes(x=airdate, y=n, color=party)) + geom_line() +
  facet_wrap(~party, scales = "free_x")

# (3.3) Proportion of China-related ads (C) by "Party ID" over "date" (INDIVIDUAL, RELEVANT DATES) 
ggplot(c, aes(x=airdate, y=n, color=party)) + geom_line() +
  facet_wrap(~party) + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

# (4.1) create dataframe for graph
d <- federal_2006 %>% group_by(airdate, fear, enthusiasm, anger, pride, humor, sadness, issue65) %>% tally() 
d$n[d$issue65 == 0] <- 0
d$n[is.na(d$issue65)] <- 0 

d$fear <- haven::as_factor(d$fear)
d$enthusiasm <- haven::as_factor(d$enthusiasm)
d$anger <- haven::as_factor(d$anger)
d$pride <- haven::as_factor(d$pride)
d$humor <- haven::as_factor(d$humor)
d$sadness <- haven::as_factor(d$sadness)

# (4.1) the changing proportion of 6 emotions on China related ads (a) over time (COMPILED)
ggplot(d, aes(x=airdate, y=n, color=fear)) + geom_line()
ggplot(d, aes(x=airdate, y=n, color=enthusiasm)) + geom_line()
ggplot(d, aes(x=airdate, y=n, color=anger)) + geom_line()
ggplot(d, aes(x=airdate, y=n, color=pride)) + geom_line()
ggplot(d, aes(x=airdate, y=n, color=humor)) + geom_line()
ggplot(d, aes(x=airdate, y=n, color=sadness)) + geom_line()


# (4.2) the changing proportion of 6 emotions on China related ads (a) over time (INDIVIDUAL)
ggplot(d, aes(x=airdate, y=n, color=fear)) + geom_line() +
  facet_wrap(~fear, scales = "free") 
ggplot(d, aes(x=airdate, y=n, color=enthusiasm)) + geom_line() +
  facet_wrap(~enthusiasm, scales = "free") 
ggplot(d, aes(x=airdate, y=n, color=anger)) + geom_line() +
  facet_wrap(~anger, scales = "free") 
ggplot(d, aes(x=airdate, y=n, color=pride)) + geom_line() +
  facet_wrap(~pride, scales = "free") 
ggplot(d, aes(x=airdate, y=n, color=humor)) + geom_line() +
  facet_wrap(~humor, scales = "free") 
ggplot(d, aes(x=airdate, y=n, color=sadness)) + geom_line() +
  facet_wrap(~sadness, scales = "free") 

# (4.3) the changing proportion of 6 emotions on China related ads (a) over time (INDIVIDUAL, RESCALED X)
ggplot(d, aes(x=airdate, y=n, color=fear)) + geom_line() +
  facet_wrap(~fear, scales = "free") +
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

ggplot(d, aes(x=airdate, y=n, color=enthusiasm)) + geom_line() +
  facet_wrap(~enthusiasm, scales = "free") +
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

ggplot(d, aes(x=airdate, y=n, color=anger)) + geom_line() +
  facet_wrap(~anger, scales = "free") +
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

ggplot(d, aes(x=airdate, y=n, color=pride)) + geom_line() +
  facet_wrap(~pride, scales = "free") +
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

ggplot(d, aes(x=airdate, y=n, color=humor)) + geom_line() +
  facet_wrap(~humor, scales = "free") +
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

ggplot(d, aes(x=airdate, y=n, color=sadness)) + geom_line() +
  facet_wrap(~sadness, scales = "free") +
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

# (5.1) create dataset for graph 
e <- federal_2006 %>% group_by(airdate, categorystate, fear, enthusiasm, anger, pride, humor, sadness, issue65) %>% tally() 
e$n[e$issue65 == 0] <- 0
e$n[is.na(e$issue65)] <- 0 

c$party <- haven::as_factor(c$party)
e$fear <- haven::as_factor(e$fear)
e$enthusiasm <- haven::as_factor(e$enthusiasm)
e$anger <- haven::as_factor(e$anger)
e$pride <- haven::as_factor(e$pride)
e$humor <- haven::as_factor(e$humor)
e$sadness <- haven::as_factor(e$sadness)

# (5.1) the changing proportion of 6 emotion China related ads (b) by region (RELEVANT STATES ONLY) (SCALED DATES)

#fear
e %>%
  filter(categorystate %in% c("IN","MI","NY","OH","PA","TN","WI")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = fear)) +
  geom_line() +
  facet_grid(rows = vars(fear), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))
#enthusiasm
e %>%
  filter(categorystate %in% c("IN","MI","NY","OH","PA","TN","WI")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = enthusiasm)) +
  geom_line() +
  facet_grid(rows = vars(enthusiasm), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))
#anger
e %>%
  filter(categorystate %in% c("IN","MI","NY","OH","PA","TN","WI")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = anger)) +
  geom_line() +
  facet_grid(rows = vars(anger), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))
#pride
e %>%
  filter(categorystate %in% c("IN","MI","NY","OH","PA","TN","WI")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = pride)) +
  geom_line() +
  facet_grid(rows = vars(pride), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))
#humor
e %>%
  filter(categorystate %in% c("IN","MI","NY","OH","PA","TN","WI")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = humor)) +
  geom_line() +
  facet_grid(rows = vars(humor), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))
#sadness
e %>%
  filter(categorystate %in% c("IN","MI","NY","OH","PA","TN","WI")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = sadness)) +
  geom_line() +
  facet_grid(rows = vars(sadness), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

# (6) create dataset for the graph
e <- federal_2006 %>% group_by(airdate, party, fear, enthusiasm, anger, pride, humor, sadness, issue65) %>% tally() 
e$n[e$issue65 == 0] <- 0
e$n[is.na(e$issue65)] <- 0 

e$party <- haven::as_factor(e$party)
e$fear <- haven::as_factor(e$fear)
e$enthusiasm <- haven::as_factor(e$enthusiasm)
e$anger <- haven::as_factor(e$anger)
e$pride <- haven::as_factor(e$pride)
e$humor <- haven::as_factor(e$humor)
e$sadness <- haven::as_factor(e$sadness)

# (6.1) the changing proportion of 6 emotions China related ads (c) by Party ID of the candidate

#fear
ggplot(data =e, mapping = aes(x = airdate, y = n, color = fear)) +
  geom_line() +
  facet_grid(rows = vars(fear), cols = vars(party), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

#enthusiasm
ggplot(data =e, mapping = aes(x = airdate, y = n, color = enthusiasm)) +
  geom_line() +
  facet_grid(rows = vars(enthusiasm), cols = vars(party), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

#anger
ggplot(data =e, mapping = aes(x = airdate, y = n, color = anger)) +
  geom_line() +
  facet_grid(rows = vars(anger), cols = vars(party), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

#pride
ggplot(data =e, mapping = aes(x = airdate, y = n, color = pride)) +
  geom_line() +
  facet_grid(rows = vars(pride), cols = vars(party), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

#humor
ggplot(data =e, mapping = aes(x = airdate, y = n, color = humor)) +
  geom_line() +
  facet_grid(rows = vars(humor), cols = vars(party), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

#sadness
ggplot(data =e, mapping = aes(x = airdate, y = n, color = sadness)) +
  geom_line() +
  facet_grid(rows = vars(sadness), cols = vars(party), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))



#2010
downballot_2010 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-downballot-2010-v1.1.dta")
gov_2010 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2010-v1.2.dta")
federal_2010 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-federal-2010-v1.3.dta")

#2012
pres_2012 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-pres-2012-v1.2.dta")
downballot_2012 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-downballot-2012-v1.1.dta")
gov_2012 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2012-v1.1.dta")
house_2012 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-house-2012-v1.1.dta")
senate_2012 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-senate-2012-v1.1.dta")

#2014
downballot_2014 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-downballot-2014-v1.1.dta")
house_2014 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-house-2014-v1.0.dta")
gov_2014 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2014-v1.1.dta")
senate_2014 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-senate-2014-v1.0.dta")

#2016 
downballot_2016 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-downballot-2016-v1.0.dta")
gov_2016 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2016-v1.0.dta")
house_2016 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-house-2016-v1.0.dta")
senate_2016 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-senate-2016-v1.0.dta")
pres_2016 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-pres-2016-v1.1.dta")

#2018 
house_2018 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-house-2018-v1.0.dta")
senate_2018 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-senate-2018-v1.0.dta")
downballot_2018 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-downballot-2018-v1.0.dta")
gov_2018 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2018-v1.0.dta")

