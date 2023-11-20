#Arjun Reddy Pulugu, Oct 26 2023, ALY6000: Intro to Analytics.
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R
session

install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)


#1
prob1_result <- dbinom(5,7,0.65)
prob1_result

#2
prob2_result <- data.frame(Wins = c(0,1,2,3,4,5,6,7))
view(prob2_result)

prob2_result$probability <- apply(prob2_result,1, function(n){
  return(dbinom(n,7,0.65))
})

#3
prob3_result <- pbinom(4, 7, 0.65)
prob3_result

#4
prob4_result <- pbinom(5,7,0.65) - pbinom(2,7,0.65)
prob4_result

#5
prob5_result <- 1- pbinom(4, 7, 0.65)
prob5_result

#6
prob6_result <- 7* 0.65 #total no of games * probability of a win
prob6_result

#7
prob7_result <- 7* 0.65 * 0.35
prob7_result

#8
set.seed(10)
 random =rbinom(1000, 7, 0.65 )

#9
prob9_result <- mean(random)
prob9_result

#10
prob10_result <- var(random)
prob10_result

#11
prob11_result <- dpois(6,7)
prob11_result

#12
prob12_result <- ppois(40, 56)
prob12_result

#13
prob13_result <- 1 - ppois(274, 5*8*7)
prob13_result

#14
prob14_result <- 1 - ppois(274, 4*8*7)
prob14_result

#15
prob15_result <- qpois(0.9, 56)
prob15_result

#16
set.seed(15)
random_pois <- rpois(1000, 7*8)
random_pois

#17
prob17_result <- mean(random_pois)
prob17_result

#18
prob18_result <- var(random_pois)
prob18_result

#19
prob19_result <- (pnorm(2200,2000, 100 ) - pnorm(1800, 2000, 100))*100
prob19_result

#20
prob20_result <- 1 - pnorm(2500, 2000, 100)
prob20_result

#21
prob21_result <- round(qnorm(0.1, 2000, 100))
prob21_result

#22
set.seed(25)
random_norm <- rnorm(10000, 2000, 100 )

#23
prob23_result <- mean(random_norm)
prob23_result

#24
prob24_result <- sd(random_norm)
prob24_result

#25
samples_mean <-c()
num_samples <- 1000
for (i in 1: num_samples) {
  sampledata <- sample(random_norm, 100, replace = T)
  samples_mean[i] <- mean(sampledata)
}
samples_mean

#26
hist(samples_mean)

#27
prob27_result <- mean(samples_mean)
prob27_result

#28
install.packages("palmerpenguins")
library(palmerpenguins)
view(penguins)

penguins %>% 
  select(species, flipper_length_mm) %>% 
  filter(species == "Adelie") %>% 
  na.omit() %>% 
  ggplot(aes(flipper_length_mm))+
  geom_histogram(fill= "blue", binwidth = 2)

#29
penguins %>% 
  select(species, flipper_length_mm, bill_depth_mm) %>% 
  filter(species == "Gentoo") %>% 
  na.omit() %>% 
  ggplot(aes(flipper_length_mm, bill_depth_mm))+
  geom_point()+
  theme_bw()



















