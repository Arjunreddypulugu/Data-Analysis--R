#Arjun Reddy Pulugu, Oct 17 2023, AlY6000: Intro to Analytics, MPS Analytics
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #
clears packages
options(scipen = 100) # disables scientific notation for entire R
session

library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(ggeasy)
library(ggthemes)

df = read_csv("/Users/arjunreddypulugu/Desktop/videogames_sales.csv")

dim(df)
glimpse(df)
sum(is.na(df$User_Count))
as.integer(mean(df$User_Count, na.rm = T))
mean(df$Critic_Score, na.rm = T)


 df <- df %>% 
  mutate(User_Count = ifelse(is.na(User_Count), as.integer(mean(User_Count, na.rm = T)), User_Count)) %>% 
   mutate(Critic_Score = ifelse(is.na(Critic_Score), as.integer(mean(Critic_Score, na.rm = T)), Critic_Score))

 install.packages("DescTools")
library(DescTools)

 df <- df %>% 
   mutate (Year_of_Release = ifelse(is.na(Year_of_Release), mode(Year_of_Release), Year_of_Release)) %>% 
   mutate( Genre = ifelse(is.na(Genre), mode(Genre), Genre)) %>% 
   mutate(Publisher = ifelse(is.na(Publisher), mode(Publisher), Publisher))
 
sum(is.na(df)) 

summary(df$Global_Sales)
unique(df$Genre)
unique(df$Publisher)
unique(df$Year_of_Release)
summary(df$User_Count)
summary(df$Critic_Score)
summary(df$NA_Sales)

#No of users by year
df %>% 
  ggplot(aes(Year_of_Release, User_Count))+
  geom_bar(stat = "identity")+
  labs(title = " No of users by Year", x= "Year", y = "Number of users")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust =0.5))

# Global sales by Genre
df %>% 
  ggplot(aes(Genre, Global_Sales))+
  geom_bar(stat = "identity", fill = "#FADA11")+
  labs(title = "Global Sales by Genre", x ="Genre", y = "Golbal Sales")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Distribution of critic score
df %>% 
  ggplot(aes(Critic_Score))+
  geom_histogram(fill = "#086CB6" , binwidth = 12)+
  labs(title = "Distribution of critic score", x= "Critic Score")+
  theme_minimal()

#Relattion between NA sales and Global sales
df %>% 
  ggplot(aes(Global_Sales, NA_Sales))+
  geom_point(size =1, color ="#424C09")+
  geom_line(color = "#D8FF00")+
  labs(title = "Relation between NA and Global sales", x= "Global sales", y= "NA sales")+
  theme_minimal()

#Relation between EU sales and Global sales
df %>% 
  ggplot(aes(Global_Sales, EU_Sales))+
  geom_point(size=1, color = "#030C42")+
  geom_smooth(method = lm , se= F)+
  labs(title = "Relation between EU and Global sales", x= "Global sales",  y = " EU sales")+
  theme_minimal()

df_pub <-df %>% 
  group_by(Publisher) %>% 
  summarise(Sales =sum(Global_Sales), Users =sum(User_Count))


df_pub <- df_pub %>% 
  arrange(desc(Sales)) %>% 
  arrange(desc(Users))

df_pub_topby_users <- df_pub %>% 
  filter(Users> 60000)


df_pub_topby_users %>% 
  ggplot(aes(Publisher, Users))+
  geom_bar(fill = "#59C1A6", stat = "identity" )+
  labs(title = "No of users of top publishers", x= "Publisher", y = "Number of users")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust=1))

df_pub_topby_sales <- df_pub %>% 
  arrange(desc(Sales)) %>% 
  filter(Sales > 200)
 

df_pub_topby_sales %>% 
  ggplot(aes(reorder(Publisher, - Sales), Sales))+
  geom_bar(stat = "identity", fill = "#D42D00")+
  labs(title ="Sales of top publishers", x= "Publisher", y= "Sales")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30, hjust=1))

df_pub %>% 
  filter(Users > 60000) %>% 
  ggplot(aes(Users, Sales))+
  geom_point(aes(color = Publisher), size =8)+
  geom_smooth(se= F, method = lm)+
  labs(title = "User-Sales relation of different publishers", x= "Users", y = "sales")+
  theme_minimal()

df %>% 
  ggplot(aes(Year_of_Release, Global_Sales))+
  geom_bar(aes(fill = Genre), stat = "identity")+
  labs(title = " Global sales trend of different Genre by Year", x= "Year", y = "Sales by Genre")+
  theme_minimal()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust =0.5))


df_genre <- df %>% 
  group_by(Genre) %>% 
   summarise(Global_salesG=sum(Global_Sales), NA_salesG=sum(NA_Sales), EU_salesG=sum(EU_Sales),JP_salesG= sum(JP_Sales), UsersG =sum(User_Count))


df_genre %>% 
  ggplot(aes(UsersG, Global_salesG))+
  geom_point(aes(color = Genre), size=3)+
  geom_smooth(se=F, method = lm)+
  labs( x= "No of users", y = "Global Sales")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust =1))

df_genre %>% 
  arrange(desc(NA_salesG))
  ggplot(aes(Genre,NA_salesG))+
  geom_bar(stat = "identity", fill= "#18096C")+
  labs(title = "Sales of different Genres in North America", x= "Genre", y ="Sales")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
df %>% 
  filter(Year_of_Release > 2000) %>% 
    ggplot(aes(Year_of_Release, Global_Sales, Genre))+
    geom_bar(aes(fill = Genre), stat = "identity")+
  facet_wrap(~Genre)+
    labs(title = " Global sales trend of different Genre by Year (2001-2020)", x= "Year", y = "Sales by Genre")+
    theme_minimal()+
    coord_flip()+
    theme(axis.text.x = element_text(angle = 90, hjust =0.5))

df %>% 
  filter(Year_of_Release < 2001 ) %>% 
  ggplot(aes(Year_of_Release, Global_Sales, Genre))+
  geom_bar(aes(fill = Genre), stat = "identity")+
  facet_wrap(~Genre)+
  labs(title = " Global sales trend of different Genre by Year (1980-2000)", x= "Year", y = "Sales by Genre")+
  theme_minimal()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust =0.5))

df_genre %>% 
  ggplot(aes(reorder(Genre, - EU_salesG), EU_salesG))+
  geom_bar(stat = "identity",fill = "#189A9E")+
  labs(title = "Sales of different genres in Europe", x= "Genre", y= "sales")+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 90, hjust =1))

df_genre %>% 
  ggplot(aes(reorder(Genre, - NA_salesG), NA_salesG))+
  geom_bar(stat = "identity",fill = "#59BD14")+
  labs(title = "Sales of different genres in North America", x= "Genre", y= "sales")+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 90, hjust =1))

df_genre %>% 
  ggplot(aes(reorder(Genre, - JP_salesG), JP_salesG))+
  geom_bar(stat = "identity",fill = "#E7A90E")+
  labs(title = "Sales of different genres in Japan", x= "Genre", y= "sales")+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 90, hjust =1))

df_genre %>% 
  ggplot(aes(reorder(Genre, - Global_salesG), Global_salesG))+
  geom_bar(stat = "identity",fill = "#520B7A")+
  labs(title = "Sales of different genres Globally", x= "Genre", y= "sales")+
  theme_clean()+
  theme(axis.text.x = element_text(angle = 90, hjust =1))

acc = read.csv("/Users/arjunreddypulugu/Desktop/top5.csv")
view(acc)

acc %>% ggplot(aes(Category, score_Total))+
  geom_bar(aes(fill= score_Total),stat = "identity")+
  coord_polar(theta = "x")











































  
  
  
  
  
  
  
  
  
  






  
















 
 
 
 
 




