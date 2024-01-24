library(tidyverse)
library(mice)
View(starwars)
str(starwars)
d1<-starwars%>%
  select(name, gender, hair_color, height)%>%
  filter(!complete.cases(.))%>%
  view()
md.pattern(d2)
md.pairs(d1)
d2<-starwars%>%
  select(name, gender, hair_color, height)%>%
  na.omit()%>%
  view()
d3<-starwars%>%
  select(name, gender, hair_color, height)%>%
  drop_na(height)%>%
  mutate(gender=replace_na(gender,'none'))%>%
  view()
d4<-unique(starwars$hair_color);d4
table(d4)
d5<-starwars%>%
  select(name, gender, hair_color, height)%>%
  mutate(hair_color=na_if(hair_color,'unknown'))%>%
  filter(is.na(hair_color))%>%
  view()
mean(starwars$height,na.rm=TRUE)
  