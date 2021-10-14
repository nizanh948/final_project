# Nizan Howard
# Ista 320
# 10/13/21
# Final Project assignment

#The data set I'm working on for this project is a Bakery Sales Data set accessed from Kaggle that includes;
# loading libraries and cleaning the data set.
library(effects)
library(tidyverse)
library(lubridate)
# read file in and clean-in it 
bakery<- read_csv("Bakery.csv")

bakery <- bakery %>%
  separate(col = DateTime, into= c("Date", "Time"), sep=" ")
clean_bakery <- bakery %>%
  separate(col = Date, into = c("Month","Day","Year"))
clean_bakery <- clean_bakery%>%
  mutate(Date = parse_date(paste(Month, Day, Year), "%m %d %Y"))



#First question:
#What were the top ten most frequently purchased items from this bakery in November 2016?
#first graph that is the top 10 items sold at the shop to get a sense of the data 

top_ten <- clean_bakery %>%
  filter(Month == "11" & Year == "2016" )%>% 
  count(Items)%>%
  top_n(10)

top_ten%>%
  ggplot(aes(x = n, 
             y = reorder(Items, n)))+
  geom_col()



# Second Question:
#How did the top 3 items sell in comparison throughout December of 2016?
#The second graph shows the distribution of amount of tea,coffee, or Bread Purchased in the month 
# of December of 2016. 

day_items<- clean_bakery %>%
  filter(Year == "2016" & Month == 12) %>%
  filter(Items == "Coffee"| Items == "Bread"| Items == "Tea")%>%
  group_by(Month, Day, Items, Date)%>%
  count(Items)

day_items %>%
  ggplot(aes(x = Day,
       y = n,
       color = Items))+
  geom_point()


#Third Question
#How did the distribution of coffee sell throughout 2016? "
# The third graph utilizes a line graph. This is a line plot showcasing the time of the distribution of coffee purchased throughout 2016. 

day_coffee <- clean_bakery %>%
  filter(Year == "2016"  & Items == "Coffee")%>%
  group_by(Items, Date, Day)%>%
  count(Items)

day_coffee%>%
  ggplot(aes(x = Date,
             y = n,
             label = n))+
  geom_point()+
  geom_line()


#Statistic Visualization 
# Due to the graphs and understanding the bases of data I would like to see if 
# The Day type, which is the  weekend or week day has an effect on the items distributed 

# Question What top 2 items affected the year of sales for this coffee shop?

model1 <- clean_bakery %>%
  group_by(Items, Year)%>%
  count(Items)%>%
  top_n(2)%>%
  mutate(Year = as.numeric(Year))%>%
  lm(formula = Year~Items)
summary(model1)

effect("Items", model1)%>%
  data.frame()%>%
  ggplot(aes(y = reorder(Items, fit),
             x =fit,
             label = round(fit, digits = 2)))+
  geom_errorbar(aes(xmin = lower, 
                    xmax = upper),
                width = .1)+
  geom_label()







