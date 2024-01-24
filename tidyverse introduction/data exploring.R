# explore data------
library(tidyverse)
data()
?starwars
#dimensions-----
# it tell us the number of rows and columns in a dataset
dim(starwars)
#structure---- 
str(starwars)
glimpse(starwars)
view(starwars)
head(starwars)
tail(starwars)
attach(starwars)
names(starwars)
length(starwars)
## categorical-----
class(hair_color)
length(hair_color)#show number of observations
unique(hair_color)
view(sort(table(hair_color),decreasing = TRUE))
barplot(sort(table(eye_color),decreasing = TRUE))
d1<-starwars %>% 
  select(hair_color) %>% 
  count(hair_color) %>% 
  arrange(desc(n)) %>% 
  view()
view(starwars[is.na(hair_color),])
#numerical-----
class(height)
length(height)
summary(height)
boxplot(height)
hist(height)
#---------cleaning data------
#checking if each variable is categorized as type of variable
# to select great variable to work with
# findind and dealing with missing data
# finding amd dealing with duplicates 
# recode values 
library(tidyverse)
library(mice)
data()
view(starwars)
#----- variable types----
d1<-starwars
glimpse(d1)
class(d1$gender)
unique(d1$gender)
# how to change from character to factor variable
d1$gender<-as.factor(d1$gender)
class(d1$gender)
levels(d1$gender)
d1$gender<-factor((d1$gender),
                  levels = c("masculine","feminine"))
levels(d1$gender)
#----select variable-----
names(d1)
d2<-d1%>%
  select(name,height,ends_with('color'))%>%
  names()
#-----filter observations-----
unique(d1$hair_color)
d3<-d1 %>% 
  select(name,height,ends_with('color'))%>%
  filter(hair_color %in% c("blonde","brown" )&
           height<180) %>% 
  view()
# %in% means within hair color
#----missing values-----
#understand where the missing value is
d4<-d1 %>% 
  select(name,gender,hair_color,height) %>% 
  filter(!complete.cases(.)) %>% 
  view()
md.pattern(d4)
# we delect the missing variable where appropriate
d5<-d1 %>% 
  select(name,gender,hair_color,height) %>% 
  filter(!complete.cases(.)) %>% 
  drop_na(height) %>% 
  view()
d6<-d1 %>% 
  select(name,gender,hair_color,height) %>% 
  filter(!complete.cases(.)) %>% 
  mutate(hair_color=replace_na(hair_color,'none')) %>% #create or overwrite existing variable.
  view()
#----duplicate----
Names<-c('peter','john','andrew','peter')
age<-c(11,44,55,11)  
friends<-data.frame(Names,age)
friends[!duplicated(friends),]
duplicated(friends)
friends %>% distinct()
#-----recoding variables------
d7<-d1 %>% 
  select(name,gender) %>% 
  mutate(gender=recode(gender,
                       "masculine"=1,"feminine"=2)) %>% 
  view()
#----filter----
library(tidyverse)
view(msleep)
glimpse(msleep)
my_data<-msleep %>% 
  select(name,sleep_total) %>% 
  filter(sleep_total>18) %>% 
  view()
my_data1<-msleep %>% 
  select(name,order,bodywt,sleep_total) %>% 
  filter(order=="Primates",bodywt>20) %>% 
  view()
my_data2<-msleep %>% 
  select(name,order,bodywt,sleep_total) %>% 
  filter(order=="Primates"| bodywt>20) %>%  # return is either is true
  view()
my_data3<-msleep %>% 
  select(name,order,bodywt,sleep_total) %>% 
  filter(order=="Primates"& bodywt>20) %>%  # return if all are true
  view()
my_data2<-msleep %>% 
  select(name,sleep_total) %>% 
  filter(name=="Cow"|
           name=="Dog"|
           name=="Goat") %>%  
  view()
my_data2<-msleep %>% 
  select(name,sleep_total) %>% 
  filter(name %in% c("Cow","Dog","Goat")) %>%  
  view()
my_data2<-msleep %>% 
  select(name,sleep_total) %>% 
  filter(between(sleep_total,16,18)) %>%  
  view()
my_data2<-msleep %>% 
  select(name,sleep_total) %>% 
  filter(near(sleep_total,17,tol=0.5)) %>%  
  view()
#------recoding----
library(tidyverse)
view(starwars)
attach(starwars)
unique(gender) 
sw<-starwars %>% 
  select(name,height,mass,gender) %>% 
  rename(weight=mass) %>% 
  na.omit() %>% 
  mutate(height=height/100) %>%  # to change height into meters
  #filter(gender=="masculine"|gender=="feminine") %>% 
  filter(gender %in% c("masculine","feminine")) %>% 
  mutate(gender=recode(gender,masculine="m",feminine="f")) %>% 
  mutate(size=height>1 & weight>75,
         size=if_else(size == TRUE, "big","small")) %>% 
  view()



