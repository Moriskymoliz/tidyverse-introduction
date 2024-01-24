#data 
#mapping
#geometric representation
#statistics
#facet
#coordination
#labels
#theme
library(tidyverse)
data()
# prako 1-------
BOD
ggplot(data = BOD,
       mapping = aes(x=Time,
                     y=demand))+
  geom_point(size=0.5)+
  geom_line(colour="red")
ggplot(BOD, aes(Time,demand))+
  geom_point(size=3)+
  geom_line(colour="red")
# prako 2-------
CO2
CO2 %>% 
  ggplot(aes(conc,uptake,colour=Treatment))+
  geom_point(alpha=0.5)+
  geom_smooth(method = lm,se=F)+
  facet_wrap(~Type)+
  labs(title = "concentration of co2")+
  theme_bw()
# prako 3-------
names(CO2)
CO2 %>% 
  ggplot(aes(Treatment,uptake))+
  geom_boxplot()+
  geom_point(aes(size=conc,
                 colour=Plant))+
  facet_wrap(~Type)+
  coord_flip()+
  theme_bw()
# prako 4------
view(mpg)
view(table(mpg$year))
mpg %>% 
  ggplot(aes(displ,cty,colour=year,size=trans))+
  geom_point()
# prako 5------
msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(sleep_total))+
  geom_density()+
  facet_wrap(~vore)+
  theme_bw()
# prako 6-------
msleep %>% 
  drop_na(vore) %>% 
  #filter(vore=="herbi"| vore=="omni") %>% 
  filter(vore %in% c("herbi","omni")) %>% 
  ggplot(aes(sleep_total,fill=vore))+
  geom_density(alpha=0.2)+
  theme_bw()
ggsave("msleep.png",height = 7,width = 10,units = "cm",dpi = 300)
# scatter 
msleep %>% 
  drop_na(bodywt,brainwt) %>%
  ggplot(aes(bodywt,brainwt))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x="body weight",
       y="brain weight",
       title = "brain and body weight")
d<-lm(bodywt~brainwt,data=msleep)
summary(d)
plot(msleep$bodywt~msleep$brainwt)
abline(d)
library(readxl)
data1<-read_excel("C:\\Users\\aucha\\Desktop\\Appliances Data.xlsx",
                  sheet = 'Sheet1')
view(data1)
data<-read.csv("H.csv")
view(data)



