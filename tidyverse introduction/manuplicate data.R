# load package
library(tidyverse)
data()
#view data
?msleep
glimpse(msleep)
view(msleep)
#-------rename a variable-----
d1<-msleep %>% 
  rename('conserv'='conservation') %>% #start with new name
  glimpse()
#----reorder variable----
d2<-msleep %>% 
  select(name,vore,brainwt,bodywt) %>% 
  glimpse()
#changing a variable type-----
d4<-msleep
class(d4$vore)
d4$vore<-as.factor(d4$vore)
glimpse(d4)
#or
d5<-d4 %>% 
  mutate(vore=as.character(vore)) %>% 
  glimpse()
#select variable to work with
msleep %>% 
  select(2:4,awake,starts_with('sleep'),
         contains('wt')) %>% 
  names()
# filter and arrange data-----
unique(msleep$order)
d6<-msleep %>% 
  filter((order=="Carnivora" | order=="Primates")&
           sleep_total>8) %>% # meet criteria 
  select(name,order,sleep_total) %>% 
  arrange(-sleep_total) %>% 
  view()
# or
d6<-msleep %>% 
    filter(order %in% c("Carnivora","Primates")&
           sleep_total>8) %>% # meet criteria 
  select(name,order,sleep_total) %>% 
  arrange(order) %>% 
  view()
# change observations (mutate)-----
msleep %>% 
  mutate(brainwt_in_grams=brainwt*1000) %>% 
  view()
# conditional change (if_else)-----
##logical vector based on a condition
msleep$bodywt
table(msleep$bodywt>0.01)

size_of_brain<-msleep %>% 
  select(name,brainwt) %>% 
  drop_na(brainwt) %>% 
  mutate(brain_size=if_else(brainwt>0.01,
                            "ulitoa wapi","mjinga")) %>% 
  view()
# recoding data and rename a variable------
## change observations of "ulitoa wapi' and 'mjinga' into
size_of_brain<-msleep %>% 
  select(name,brainwt) %>% 
  drop_na(brainwt) %>% 
  mutate(brain_size=if_else(brainwt>0.01,
                            "ulitoa wapi","mjinga")) %>% 
  mutate(brain_size=recode(brain_size,
                           "ulitoa wapi"=1,
                           "mjinga"=2 )) %>%
  arrange(brain_size) %>% 
  view()
#reshape the data fro long to wide or wide to long-------
install.packages("gapminder")
library(gapminder)
view(gapminder)
data<-select(gapminder,country,year,lifeExp)
view(data)
wide_data<-data %>%
  pivot_wider(names_from = year,values_from = lifeExp) %>% 
  view()
long_data<-wide_data %>% 
  pivot_longer(2:13,
               names_to = "year",values_to = "lifeExp") %>% 
  view()
#reordering levels of factor using forcats 
library(forcats)
library(tidyverse)
install.packages("patchwork")
library(patchwork)# used for data visualization
?forcats
data()
view(gss_cat)
glimpse(gss_cat)
# order of categorical variables matters-------
unique(gss_cat$race)
gss_cat %>% 
  pull(race) %>% 
  unique()
count(gss_cat,race,sort = T)
gss_cat %>% 
  count(race)
gss_cat %>% 
  pull(race) %>% 
  levels()
gss_cat %>% 
  select(race) %>% 
  table() %>% 
  view()

gss_cat %>% 
  mutate(race=fct_drop(race)) %>% #factor drop
  pull(race) %>% 
  levels()
# order a factor levers by the value of another numerical variable------
gss_cat %>% 
  drop_na(tvhours) %>% 
  group_by(relig) %>% 
  summarise(mean_tv=mean(tvhours)) %>% 
  ggplot(aes(mean_tv,relig))+
  geom_point(size=4)

gss_cat %>% 
  drop_na(tvhours) %>% 
  group_by(relig) %>% 
  summarise(mean_tv=mean(tvhours)) %>% 
  mutate(relig=fct_reorder(relig,mean_tv)) %>% 
  ggplot(aes(mean_tv,relig))+
  geom_point(size=4)
# reverse factor levels
gss_cat %>% 
  drop_na(age) %>% 
  filter(rincome!="Not applicable") %>% 
  group_by(rincome) %>% 
  summarise(mean_age=mean(age)) %>% 
  ggplot(aes(mean_age,rincome))+
  geom_point(size=4)

gss_cat %>% 
  drop_na(age) %>% 
  filter(rincome!="Not applicable") %>% 
  group_by(rincome) %>% 
  summarise(mean_age=mean(age)) %>% 
  mutate(rincome=fct_rev(rincome)) %>% 
  ggplot(aes(mean_age,rincome))+
  geom_point(size=4)
# order factor levels by frequency of the value of that variable------
gss_cat %>% 
  count(marital)
gss_cat %>% 
  ggplot(aes(marital))+
  geom_bar()
gss_cat %>% 
  mutate(marital=fct_infreq(marital)) %>% 
  count(marital)
gss_cat %>% 
  mutate(marital=fct_infreq(marital)) %>% 
  mutate(marital=fct_rev(marital)) %>%
  count(marital)
gss_cat %>% 
  mutate(marital=fct_infreq(marital)) %>% 
  mutate(marital=fct_rev(marital)) %>%
  ggplot(aes(marital))+
  geom_bar(fill='steelblue',alpha=0.5)+
  theme_bw()
# recoding factor-----
attach(gss_cat)
unique(partyid)
gss_cat %>% 
  mutate(partyid=fct_recode(partyid,
                            "repubication, strong"="Strong republican",
                            "repubication, weak"="Not str republican",
                            "Independent, near rep"="Ind,near rep",
                            "Independent, near dem"="Ind,near dem",
                            "Democrat, weak"="Not str democrat",
                            "Democrat, strong"="Strong democrat",
                            "other"="No answer",
                            "other"="Don't know",
                            "other"="Other party")) %>% 
  count(partyid)
# collapsing factor----
gss_cat %>% 
  mutate(partyid=fct_collapse(partyid,
                              other=c("No answer","Don't know","Other party"),
                              rep=c("Strong republican","Not str republican"),
                              ind=c("Ind,near rep","Independent","Ind,near dem"),
                              dem=c("Not str democrat","Strong democrat"))) %>% 
  count(partyid)
# lumping into 'other'-----
gss_cat %>% 
  count(relig,sort = T)

gss_cat %>% 
  mutate(relig=fct_lump(relig, n=2)) %>% 
  count(relig)
# reordering a factor y by its value corresponding the largest x value
gss_cat %>% 
  filter(!is.na(age)) %>% 
  filter(marital %in% c("Never married",
                        "Married",
                        "Widowed")) %>% 
  count(age,marital) %>% 
  group_by(age) %>% 
  mutate(percentage=(n/sum(n))*100) %>% 
  ggplot(aes(age,percentage,color=marital))+
  geom_line(linewidth=2, na.rm = TRUE)+
  theme_classic()
# how to save ggplot------
glimpse(mpg)
mpg %>% 
  ggplot(aes(x=cty,fill=drv))+
  geom_density(alpha=0.3)+
  theme_bw()
ggsave("mpg plot.png",
      width=10,
      height=7,
      units = "cm",
      dpi = 300)
# group and summarize---------
library(tidyverse)
view(msleep)
names(msleep)

msleep %>%
  drop_na(sleep_rem,vore) %>% 
  group_by(vore) %>% 
  summarise("average total sleep"=mean(sleep_total),
            "maximum rem sleep"=max(sleep_rem)) %>% 
  view()
unique(msleep$vore)
# separate and unite-------
library(tidyverse)
library(gapminder)
view(gapminder)

# how to separate years into century and year--
gapminder1<-gapminder %>% 
  separate(col = year,
           into = c("century","year"),
           sep = 2) %>% 
  view()
d<-data.frame(name=c("maurice odhiambo",
                     "daniel ombonya",
                     "grace amwiru"))
d1<-d %>% 
  separate(col = name,
           into = c("first name","last name"),
           sep = " ") %>% 
  view()
# combine century and year into date----
gapminder1 %>% 
  unite(col = date,
        century,year,
        sep = "") %>% 
  view()
d1 %>% 
  unite(col = "full names",
        "first name","last name",
        sep = " ") %>% 
  view()











