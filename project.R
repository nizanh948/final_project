# Nizan Howard
# Ista 320
# 10/13/21
# Final Project assignment

"
The data set I'm working on for this project is a Bakery Sales Data set accessed from Kaggle that includes;

 -ID ( index identification number)

-Transactions( the different purchased items from the bakery)

-Date-Time ( Month Year, Day, hours mins and sec) of the purchases the Day-Time of purchase( Morning Afternoon, or evening)

-Week Type( the weekend or the weekday). 

I had diffculty with data cleaning soI changed  my focus for this project to utilizing the transaction frequency of items
of this bakery, specifically the most frequent purchase, and see the frequency across a range of times.
I only used (Date time and Items ).
"

# loading libraries and cleaning the data set.
library(effects)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
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
#first graph that is the top 10 items sold at the shop to get a sense of the data. 

top_ten <- clean_bakery %>%
  filter(Month == "11" & Year == "2016" )%>% 
  count(Items)%>%
  top_n(10)%>%
  mutate(count = n)

top_ten%>%
  ggplot(aes(x = count, 
             y = reorder(Items, count)))+
  geom_col(fill =  brewer.pal(10,"Paired"))+
  labs(title = "Top ten Bakery Items Puchased",
       subtitle = "Date from Novermber 2016",
       caption = "data from kaggle Bakery ")




# Second Question:
#How did the top 3 items sell in comparison throughout December of 2016?
#The second graph shows the distribution of amount of tea,coffee, or Bread Purchased in the month 
# of December of 2016. 

day_items<- clean_bakery %>%
  filter(Year == "2016" & Month == 12) %>%
  filter(Items == "Coffee"| Items == "Bread"| Items == "Tea")%>%
  group_by(Month, Day, Items, Date)%>%
  count(Items)%>%
  mutate(count = n)

day_items %>%
  ggplot(aes(x = Day,
       y = count,
       color = Items))+
  geom_point()+
  labs(title = "Top 3 Bakery Items Purchased Distribution",
       subtitle = "Date Days of December of 2016",
       caption = "data from kaggle Bakery ")

#Third Question
#How did the distribution of coffee sell throughout 2016? "
# The third graph utilizes a line graph. This is a line plot showcasing the time of the distribution of coffee purchased throughout 2016. 

day_coffee <- clean_bakery %>%
  filter(Year == "2016"  & Items == "Coffee")%>%
  group_by(Items, Date, Day)%>%
  count(Items)%>%
  mutate(count = n)

day_coffee%>%
  ggplot(aes(x = Date,
             y = count,
             label = count))+
  geom_point()+
  geom_line()+
  labs(title = "Coffee Purchases Throughout2016",
       subtitle = "Coffe by date throughout the year 2016",
       caption = "data from kaggle Bakery ")+
  scale_color_brewer(palette = "Dark2")
  


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

"
The data shows that every item technically affected the year of purches, but my goal was to see if any item in partically 
had shown the effect of sales of  a particular year of the bakery show such as 2017 vesus 2016 or in the middle of the year. 
"






