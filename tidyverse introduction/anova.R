## anova-----
library(tidyverse)
view(msleep)

my_data<-msleep %>% 
  select(vore,sleep_rem) %>% 
  drop_na()
mod1<-aov(sleep_rem~vore,data = my_data)
summary(mod1)
# p value is less than 5
# reject h0(that means are equal) its is statistically significant

# using pipes
msleep %>% 
  select(vore,sleep_rem) %>% 
  drop_na() %>% 
  aov(sleep_rem~vore,data=.) %>% 
  summary()
# to fill values-----
library(tidyverse)
?fill
data1<-data_name %>% 
  fill(name_of_column_to_fill)
#to fill all columns
data1<-data_name %>% 
  fill(1:6)
# linear regression------
# it show relationship 
# x is independent variable y is dependent variable
library(tidyverse)
data()
head(cars,10)

cars %>% 
  lm(dist~speed,data=.) %>% 
  summary()
# or
mod<-lm(dist~speed,data=cars);mod
summary(mod)

attributes(mod)
mod$residuals
hist(mod$residuals)# to show how residual are symmetrical
#scatter
plot(dist~speed,data=cars,pch="*",col="red")
abline(mod,col="blue")
# make prediction
new_speeds<-data.frame(speed=c(10,15,20))
predict(mod,new_speeds) %>% 
  round(2) # to round off

# or
cars %>% 
  lm(dist~speed,data=.) %>% 
  predict(data.frame(speed=C(10,15,20))) 
  round()


