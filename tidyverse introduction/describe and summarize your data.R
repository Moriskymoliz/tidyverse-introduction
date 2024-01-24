# describe and summarize data-----
#range / spread
# centrality
# variance
# summarize your data
# create table 
library(tidyverse)
data()
view(msleep)
# describe the spread, centrality and variance of data-----
min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)
mean(msleep$awake)
median(msleep$awake)
var(msleep$awake)
#summarized selected variable-----
summary(msleep)
summary(msleep$sleep_total)

msleep %>% 
  select(sleep_total,brainwt) %>% 
  summary()
#create a summary table-----
msleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(lower=min(sleep_total),
            average=mean(sleep_total),
            upper=max(sleep_total),
            difference=max(sleep_total)-min(sleep_total)) %>% 
  arrange(average) %>% 
  view()
#creating a contigency table----
library(MASS)
attach(Cars93)
glimpse(Cars93)
table(Origin)
table(AirBags,Origin)
addmargins(table(AirBags,Origin),1)# to add row total
addmargins(table(AirBags,Origin),2)# to add column total
prop.table(table(AirBags,Origin))*100# everything add up to 100%
prop.table(table(AirBags,Origin),2)*100 # column add up to 100%
prop.table(table(AirBags,Origin),1)*100# rows add up to 100%
round(prop.table(table(AirBags,Origin),1)*100,digits = 2)# to put into 2digit

Cars93 %>% 
  group_by(Origin,AirBags) %>% 
  summarise(number=n()) %>% 
  pivot_wider(names_from = Origin,
              values_from = number) %>% 
  view()

