# Arjun Reddy Pulugu, 11 Oct 2023, ALY 6000
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #
clears packages
options(scipen = 100) # disables scientific notation for entire R session

daf <- read.csv("/Users/arjunreddypulugu/Desktop/books.csv")
library(janitor)
library(lubridate)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(ggeasy)

#cleaning the dataset

#1
clean_names(daf)
#2
daf$firstPublishDate <- mdy(daf$firstPublishDate)
#3
daf$year <- year(daf$firstPublishDate)
#4
daf <- daf %>% 
      filter(year >= 1990 & year<= 2020)
#5
daf <- daf %>% 
     select(-publishDate, -edition, -characters, -price, -genres, -isbn, -setting)
#6
daf <- daf %>% 
  filter( pages <1200)

#Data Analysis
#1
glimpse(daf)
#2
summary(daf)
#3
 daf %>% 
   ggplot(aes(rating))+
   geom_histogram(binwidth = 0.25, fill = 'red')+
   labs(title = "Histogram of Book Ratings", x ="Rating", y="Number of Books")+
   theme_bw()

 #4
 daf %>% 
   ggplot(aes(pages))+
   geom_boxplot(fill = "magenta")+
   labs(title = "Box plot of Page Counts", x= "Pages")+
   theme_economist()
 
 #5
 daf_summary <- daf %>% 
   group_by(publisher) %>% 
   summarise(numbooks = n()) %>% 
   ungroup()
 daf_summary
daf_summary <- daf_summary %>% 
  arrange(desc(numbooks))
daf_summary <- daf_summary %>% 
  mutate(cumbooks = cumsum(numbooks))
daf_summary <- daf_summary %>% 
  mutate(rel_freq = (numbooks/sum(numbooks)))
daf_summary <- daf_summary %>% 
  mutate(cum_freq = cumsum(rel_freq))
daf_summary <- daf_summary %>% na.omit()

daf_summary <- daf_summary %>%  filter(numbooks>= 250)

#6
daf_summary %>% 
  ggplot(aes(reorder(publisher, -numbooks), numbooks)) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_line(aes(publisher, cumbooks), size = 1) +
  geom_point(aes(publisher, cumbooks), color = "blue", size = 3, show.legend = FALSE) + 
  labs(title = "Pareto and Ogive of Publisher Book Counts (1990-2020)", x = "Publisher", y = "Number of Books") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#7
daf %>% 
  ggplot(aes(pages, rating))+
  geom_point(aes(color = year))+
  labs(title = "Scatter plot of Pages vs Rating", x= "Pages", y = "Rating")+
  theme_tufte()

#8
daf_nbooks <- daf %>% 
  group_by(year) %>% 
  summarise(numb_books=n(), avg_rating = mean(rating, na.rm = T)) %>% 
  ungroup()

#9
daf_nbooks %>% 
  ggplot(aes(year, numb_books))+
  geom_point(aes(color = avg_rating, size = avg_rating))+
  geom_line()+
  labs(title = "Total no of books rated per yeaR")+
    theme_excel_new()
#10
sample_mean <- function(x) {
  n <- length(x)
  if (n == 0) {
    return(NULL)  # to avoid division by zero for empty vectors
  } else {
    return(sum(x) / n)
  }
}

pop_var <- function(x) {
  n <- length(x)
  if (n == 0) {
    return(NULL)  # to avoid division by zero for empty vectors
  } else {
    mean_x <- sample_mean(x)
    return(sum((x - mean_x)^2) / n)
  }
}

sd_var <- function(x) {
  variance <- pop_var(x)
  if (is.null(variance)) {
    return(NULL)
  } else {
    return(sqrt(variance))
  }
}

#11
sample_mean(daf$rating)
pop_var(daf$rating)
sd_var(daf$rating)

#12
sample1 <- daf[sample(nrow(daf),100, replace= F),]
sample1
sample_mean(sample1$rating)
pop_var(sample1$rating)
sd_var(sample1$rating)

sample2 <- daf[sample(nrow(daf), 100, replace=F) , ]
sample_mean(sample2$rating)
pop_var(sample2$rating)
sd_var(sample2$rating)

sample3 <- daf[sample(nrow(daf), 100, replace =F), ]
sample_mean(sample3$rating)
pop_var(sample3$rating)
sd_var(sample3$rating)

daf %>% 
  filter(pages<800) %>% 
  ggplot(aes(pages))+
  geom_histogram(binwidth = 25, fill = "#FF4488")+
  labs(title = "frequency distribution of number of pages", x= "no of pages per book", y="frequency")+
  theme_minimal()
unique(daf$author)

daf %>% 
  ggplot(aes(rating))+
  geom_boxplot(fill = "#82F43D")+
  labs(title = "Distribution of ratings using boxplot", x= "Ratings", y= "Frequency")+
  coord_flip()+
  theme_minimal()

 






















  
   
   
   







