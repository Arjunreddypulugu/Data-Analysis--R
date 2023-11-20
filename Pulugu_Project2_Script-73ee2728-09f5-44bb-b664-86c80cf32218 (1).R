install.packages("tidyverse")
library(tidyverse)
view(iris)
 #1
iris_df <- iris
view(iris_df)
#2
dim(iris_df) #displays the number or rows and columns
#3
filter(iris_df, Sepal.Length <= 6.4) #selects only rows that satisfy the condtion
#4
iris_new <- filter(iris_df, Sepal.Length <= 6.4)
view(iris_new)
count(iris_new)
#5
mutate(iris_new, longer_sepal = Sepal.Length + 0.7 )
#6
print(iris_df)
#7
range(iris_df$Petal.Length) #gives the max and min values of given varibale 
#8
count(filter(iris_df, Petal.Length < 4.35))
#These represent half of the total no of observations
#9
unique(iris_df$Species)
#Setosa , versicolor and virginica are the three
#10
setosa <- filter(iris_df, Species == "setosa")
max(setosa$Sepal.Width) #4.4
versicolor <- filter(iris_df, Species == "versicolor")
max(versicolor$Sepal.Width) #3.4
virginica <- filter(iris_df, Species == "virginica")
max(virginica$Sepal.Width) #3.8
#10
max(subset(iris_df$Sepal.Width, iris_df$Species == "setosa"))
max(subset(iris_df$Sepal.Width, iris_df$Species == "versicolor"))
max(subset(iris_df$Sepal.Width, iris_df$Species == "virginica"))  
#11
df_setosa <- filter(iris_df, Species == "setosa")
df_setosa
df_versicolor <- filter(iris_df, Species == "versicolor")
df_versicolor
df_virginica <- filter(iris_df, Species == "virginica")
df_virginica
#12
arrange(df_setosa,Sepal.Width)
arrange(df_versicolor,Sepal.Width)
arrange(df_virginica, Sepal.Width)
#13
df1 <-mutate(iris_df, sepal_width_category1 = ifelse(Sepal.Width >3, "long", "short"))
df1
#14
df2 <-mutate(iris_df, sepal_width_category2 = case_when(Sepal.Width >3 ~ "long",
                                                  Sepal.Width <3 ~ "short",
                                                  Sepal.Width ==3 ~ "short"))
#15
iris_df2 <- cbind(iris_df, df1$sepal_width_category1, df2$sepal_width_category2)
iris_df2 

write.csv(iris_df2, "/Users/arjunreddypulugu/Desktop/Pulugu_Project2_iris_df2.csv", row.names = TRUE)



