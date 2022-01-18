library(haven)
library(dplyr)

gov_2006 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2006-v1.0.dta")
gov_2010 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2010-v1.2.dta")
gov_2012 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2012-v1.1.dta")
gov_2014 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2014-v1.1.dta")
gov_2016 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2016-v1.0.dta")
gov_2018 <- read_dta("C:/Users/grant/Desktop/Desktop/1/wmp-gov-2018-v1.0.dta")

# part 1, descriptive stats for #China #Total each parties 
a <- gov_2006 %>% group_by(party, issue65) %>% tally()
b <- gov_2010 %>% group_by(party, issue65) %>% tally()
c <- gov_2012 %>% group_by(party, issue65) %>% tally()
d <- gov_2014 %>% group_by(affiliation, issue65) %>% tally()
e <- gov_2016 %>% group_by(party, issue65) %>% tally()
f <- gov_2018 %>% group_by(party, issue65) %>% tally()

# part 2, descriptive stats for Anger, Fear, Sadness for ALL parties

#Anger 
a <- gov_2006 %>% group_by(anger, issue65) %>% tally()
b <- gov_2010 %>% group_by(anger, issue65) %>% tally()
c <- gov_2012 %>% group_by(anger, issue65) %>% tally()
d <- gov_2014 %>% group_by(anger, issue65) %>% tally()
e <- gov_2016 %>% group_by(anger, issue65) %>% tally()
f <- gov_2018 %>% group_by(anger, issue65) %>% tally()

#Fear
a <- gov_2006 %>% group_by(fear, issue65) %>% tally()
b <- gov_2010 %>% group_by(fear, issue65) %>% tally()
c <- gov_2012 %>% group_by(fear, issue65) %>% tally()
d <- gov_2014 %>% group_by(fear, issue65) %>% tally()
e <- gov_2016 %>% group_by(fear, issue65) %>% tally()
f <- gov_2018 %>% group_by(fear, issue65) %>% tally()

#Sadness 
a <- gov_2006 %>% group_by(sadness, issue65) %>% tally()
b <- gov_2010 %>% group_by(sadness, issue65) %>% tally()
c <- gov_2012 %>% group_by(sadness, issue65) %>% tally()
d <- gov_2014 %>% group_by(sadness, issue65) %>% tally()
e <- gov_2016 %>% group_by(sadness, issue65) %>% tally()
f <- gov_2018 %>% group_by(sadness, issue65) %>% tally()

# part 3, descriptive stats for Anger, Fear, Sadness for EACH parties 
memory.limit(size= 100000)
# WE CAN USE THE HASTAGGED CODE BELOW IF WE WANTED A FULLER VIEW OF EMOTION VARIABLES, 
# note: we need to change sadness to fear or anger as the code hastaged below is for the sadness variable 
 
# a <- gov_2006 %>% group_by(party, sadness, issue65) %>% tally()
# b <- gov_2010 %>% group_by(party, sadness, issue65) %>% tally()
# c <- gov_2012 %>% group_by(party, sadness, issue65) %>% tally()
# d <- gov_2014 %>% group_by(affiliation, sadness, issue65) %>% tally()
# e <- gov_2016 %>% group_by(party, sadness, issue65) %>% tally()
# f <- gov_2018 %>% group_by(party, sadness, issue65) %>% tally()

#Anger 
a <- gov_2006 %>% filter(anger %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,anger, issue65) %>% tally()
b <- gov_2010 %>% filter(anger %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,anger, issue65) %>% tally()
c <- gov_2012 %>% filter(anger %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,anger, issue65) %>% tally()
d <- gov_2014 %>% filter(anger %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(affiliation,anger, issue65) %>% tally()
e <- gov_2016 %>% filter(anger %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,anger, issue65) %>% tally()
f <- gov_2018 %>% filter(anger %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,anger, issue65) %>% tally()

#Fear
a <- gov_2006 %>% filter(fear %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,fear, issue65) %>% tally()
b <- gov_2010 %>% filter(fear %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,fear, issue65) %>% tally()
c <- gov_2012 %>% filter(fear %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,fear, issue65) %>% tally()
d <- gov_2014 %>% filter(fear %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(affiliation,fear, issue65) %>% tally()
e <- gov_2016 %>% filter(fear %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,fear, issue65) %>% tally()
f <- gov_2018 %>% filter(fear %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,fear, issue65) %>% tally()

#Sadness
a <- gov_2006 %>% filter(sadness %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,sadness, issue65) %>% tally()
b <- gov_2010 %>% filter(sadness %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,sadness, issue65) %>% tally()
c <- gov_2012 %>% filter(sadness %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,sadness, issue65) %>% tally()
d <- gov_2014 %>% filter(sadness %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(affiliation,sadness, issue65) %>% tally()
e <- gov_2016 %>% filter(sadness %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,sadness, issue65) %>% tally()
f <- gov_2018 %>% filter(sadness %in% c(1,2)) %>%
  filter(issue65 %in% c(1)) %>%
  group_by(party,sadness, issue65) %>% tally()




