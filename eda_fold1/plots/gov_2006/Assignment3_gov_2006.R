library(readstata13)
library(haven)
library(dplyr)
library(ggplot2)
library(reshape2)
#install.packages("reshape2")

rm(list = ls())

dat <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-federal-2010-v1.3.dta")

# (1) create dataframe for graph 
a <- dat %>% group_by(airdate,issue65) %>% tally()
a$n[a$issue65 == 0] <- 0
a$n[is.na(a$issue65)] <- 0 

# (1.1) the changing proportion of China-related ads (a) over "time" 

f <- ggplot(a, aes(airdate,n))
f + geom_line() + 
  #scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06"))) +
  labs(title = "Proportion of China-related Ads Over time", 
       x = "Date: Jan to Dec 2006", y= "# of China Related Issues")


# (2.1) create dataframe 
b <- dat %>% group_by(airdate, state, issue65) %>% tally()
b$n[b$issue65 == 0] <- 0
b$n[is.na(b$issue65)] <- 0 

# (2.1) proportion of China-related ads by "region" 
ggplot(b, aes(state,issue65)) + geom_col(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~issue65, scales = "free_x")

# (2.2) Proportion of China-related ads by "region" over "date" INDIVIDUAL graph for ALL STATES 
ggplot(b, aes(x=airdate,y=n, color = state)) + geom_line() +
  facet_wrap(~state, scales = "free")

relstate <- unique(b$state[b$issue65 ==1]) # relevant states 
# (2.3) Proportion of China-related ads by "region" over "date" COMPILED graph (FOR RELEANT STATES)
b %>%
  filter(state %in% c(relstate)) %>%
  ggplot(aes(x=airdate,y=n, color = state)) + geom_line() #+ 
#facet_wrap(~categorystate, scales = "free")

# (2.4) Proportion of China-related ads by "region" over "date" INDIVIDUAL graph (FOR RELEVANT STATES ONLY)
b %>%
  filter(state %in% c(relstate)) %>%
  ggplot(aes(x=airdate,y=n, color = state)) + geom_line() + 
  facet_wrap(~state, scales = "free")


# (3) create dataframe 
c <- dat %>% group_by(airdate, party, issue65) %>% tally()
c$n[c$issue65 == 0] <- 0
c$n[is.na(c$issue65)] <- 0 

c$party <- haven::as_factor(c$party)
c$party <- as.character(c$party)
relparty <- unique(c$party[c$issue65==1])

# (3.1) Proportion of China-related ads (c) by party ID
g3.2 <- ggplot(c, aes(party,n))
g3.2 + geom_col()

# (3.2) Proportion of China-related ads (C) by "Party ID" over "date" (COMPILED)
ggplot(c, aes(x=airdate, y=n, color=party)) + geom_line()

# (3.3) Proportion of China-related ads (C) by "Party ID" over "date" (INDIVIDUAL) (RELEVANT PARTY)
c %>%
  filter(party %in% c(relparty)) %>%
  ggplot(aes(x=airdate, y=n, color=party)) + geom_line() +
  facet_wrap(~party, scales = "free")

# (3.4) Proportion of China-related ads (C) by "Party ID" over "date" (INDIVIDUAL, RELEVANT DATES) 
# ggplot(c, aes(x=airdate, y=n, color=party)) + geom_line() +
#  facet_wrap(~party) #+ 
#  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

# (4.1) create dataframe for graph
d <- dat %>% group_by(airdate, fear, enthusiasm, anger, pride, humor, sadness, issue65) %>% tally() 
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
e <- dat %>% group_by(airdate, categorystate, fear, enthusiasm, anger, pride, humor, sadness, issue65) %>% tally() 
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
  filter(categorystate %in% c("MI","NV")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = fear)) +
  geom_line() +
  facet_grid(rows = vars(fear), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))
#enthusiasm
e %>%
  filter(categorystate %in% c("MI","NV")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = enthusiasm)) +
  geom_line() +
  facet_grid(rows = vars(enthusiasm), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))
#anger
e %>%
  filter(categorystate %in% c("MI","NV")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = anger)) +
  geom_line() +
  facet_grid(rows = vars(anger), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))
#pride
e %>%
  filter(categorystate %in% c("MI","NV")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = pride)) +
  geom_line() +
  facet_grid(rows = vars(pride), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))
#humor
e %>%
  filter(categorystate %in% c("MI","NV")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = humor)) +
  geom_line() +
  facet_grid(rows = vars(humor), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))
#sadness
e %>%
  filter(categorystate %in% c("MI","NV")) %>%
  ggplot(mapping = aes(x = airdate, y = n, color = sadness)) +
  geom_line() +
  facet_grid(rows = vars(sadness), cols = vars(categorystate), scales = "free") + 
  scale_x_date(limits = as.Date(c("2006-07-01","2006-11-06")))

# (6) create dataset for the graph
e <- dat %>% group_by(airdate, party, fear, enthusiasm, anger, pride, humor, sadness, issue65) %>% tally() 
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