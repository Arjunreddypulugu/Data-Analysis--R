# Arjun Reddy pulugu, Oct 24 2023, ALY6000: Introduction to Analytics

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) 
#clears packages
options(scipen = 100) # disables scientific notation for entire R
session

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggeasy)
library(janitor)

#1
df = read_csv("/Users/arjunreddypulugu/Desktop/ball-dataset.csv")

dim(df)

#2
freq_color <- df %>% 
  group_by(color) %>% 
  summarise(counts = n())


#3
freq_label <- df %>% 
  group_by(label) %>% 
  summarise(counts = n())


colorss <- c(blue = "blue", green = "green", red = "red", yellow = "yellow")

#4
freq_color %>% 
  ggplot(aes(color, counts, fill = color))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = colorss)+
  labs(title = "Color Counts of balls", x= "color", y = "Counts")+
  theme_clean()

#5
freq_label %>% 
  ggplot(aes(label, counts, fill= label))+
  geom_bar(stat = "identity")+
  guides(label = "none")+
  labs(title = "Label Counts of Balls", x= "Labels", y= "counts")+
  theme_clean()

#6
prob6_result <- freq_color[freq_color$color == 'green', "counts"]/ sum(freq_color$counts)
prob6_result  

#7
countred <- freq_color$counts[freq_color$color == "red"]
countblue <- freq_color$counts[freq_color$color == "blue"]
prob7_result <- (countred+ countblue)/ sum(freq_color$counts)
prob7_result

#8
labA <- freq_label$counts[freq_label == "A"]
labC <- freq_label$counts[freq_label == "C"]
prob8_result <- (labA + labC)/ sum(freq_label$counts)
prob8_result

#9
mask = (df$color == "yellow" & df$label == "D")
prob9_result <- nrow(df[mask,])/ nrow(df)
prob9_result

#10
mask1 = (df$color == "yellow" | df$label == "D")
prob10_result <- nrow(df[mask1,])/ nrow(df)
prob10_result

#11
prob_blue <- freq_color$counts[freq_color == "blue"] / sum(freq_color$counts)
prob_red <- freq_color$counts[freq_color == "red"] / (sum(freq_color$counts) - 1)
prob11_result <- prob_blue*prob_red
prob11_result

#12
probg1 <- freq_color$counts[freq_color$color == 'green'] / sum(freq_color$counts)
probg2 <- (freq_color$counts[freq_color$color == 'green']-1) / (sum(freq_color$counts)-1)
probg3 <- (freq_color$counts[freq_color$color == 'green']-2) / (sum(freq_color$counts)-2)
probg4 <- (freq_color$counts[freq_color$color == 'green']-3) / (sum(freq_color$counts)-3)
prob12_result <- probg1*probg2*probg3*probg4
prob12_result

#13
#Assuming that the Red ball picked doesnt have a "B" label
probr <- freq_color$counts[freq_color$color == "red"]/ sum(freq_color$counts)
probB <- freq_label$counts[freq_label$label == "B"] / (sum(freq_label$counts)-1)
prob13_result <- probr*probB
prob13_result

#14
factorial <- function(n){
  if (n ==0) {return(0)}
  else if (n<0) {return(-1)}
  else{
    num <- 1
    for (i in 1:n) {num = num*i}
    return(num)
  }
}

factorial(3)
factorial(-10)
factorial(0)

#18
coin_outcomes <- data.frame(
  first = c("H", "H", "H", "H", "H", "H", "H", "H", "T", "T", "T", "T", "T", "T", "T", "T"),
  second = c("H", "H", "H", "H", "T", "T", "T", "T", "H", "H", "H", "H", "T", "T", "T", "T"),
  third = c("H", "H", "T", "T", "H", "H", "T", "T", "H", "H", "T", "T", "H", "H", "T", "T"),
  fourth = c("H", "T", "H", "T", "H", "T", "H", "T", "H", "T", "H", "T", "H", "T", "H", "T")
)


#19
coin_outcomes$probability <- apply(coin_outcomes,1,function(row){
  prod(ifelse(row== "H", 0.6,0.4))
})

#20
coin_outcomes$no_of_heads <- apply(coin_outcomes,1,function(row){
  sum(ifelse(row=="H",1,0))
})

num_heads_prob <- coin_outcomes %>% 
  select(no_of_heads, probability)


num_heads_prob <- num_heads_prob %>%
  group_by(no_of_heads) %>% 
  summarise(probab = sum(probability))
  
#21
prob21_result <- num_heads_prob %>% 
  filter(no_of_heads==3) %>% 
  select(probab)
prob21_result  

#22
prob22_result <- (num_heads_prob %>% filter(no_of_heads==2) %>% select(probab))+
(num_heads_prob %>% filter(no_of_heads==4) %>% select(probab))
prob22_result

#23
prob23_result <- 1 - (num_heads_prob %>% filter(no_of_heads==4) %>% select(probab))
prob23_result

#24
num_heads_prob %>% 
  ggplot(aes(no_of_heads, probab))+
  geom_bar(stat = "identity", color = "#02FCF9")+
  labs(title = "Probability distribution of heads for 4 flips", x= "Number of Heads", y="Probability")+
  theme_minimal()

#25
prob25_result <- (0.75)^5 * (0.5)^5
prob25_result

#26
prob26_result <- 1 - ((0.25)^5 * (0.5)^5)
prob26_result

#27
prob27_result <- choose(5,3) * choose(5,2)
prob27_result


























































































  









  






















