
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggeasy")
install.packages("ggthemes")


library(tidyverse)
df = read_csv("/Users/arjunreddypulugu/Documents/R2 folder/Global Economy Indicators.csv")
view(df)
df <- df %>% 
  select(Country, Year, `IMF based exchange rate`, Population, `Per capita GNI`, `Exports of goods and services`, `Imports of goods and services`, `Total Value Added`, `Gross Domestic Product (GDP)`,)
view(df)
dim(df)
colnames(df) <- c('Country', 'Year', 'Curr_Xrate','Population','Percapita_GNI', 'Exports', 'Imports', 'Value_added', 'GDP' )

df <- df %>% 
  mutate(Exports = ifelse(is.na(Exports), mean(Exports, na.rm = T), Exports),
         Imports = ifelse(is.na(Imports), mean(Imports, na.rm = T), Imports))

sum(colSums(is.na(df)))

df_1970 <- df %>% 
  filter(Year== 1970) %>% 
  arrange(desc(Population))

df_1970[1:10, ] %>% 
  ggplot(aes(reorder(Country,-Population ), GDP ))+
  geom_bar(stat='identity',fill = 'blue')+
  theme_classic()+
  labs(title="GDP of 10 most populated countries (1970)", x='Country', y='GDP')+
  theme(axis.text.x = element_text(hjust=1, angle = 90))

df_2021 <- df %>% 
  filter(Year== 2021) %>% 
  arrange(desc(Population))

df_2021[1:10, ] %>% 
  ggplot(aes(reorder(Country,-Population ), GDP ))+
  geom_bar(stat='identity',fill = 'blue')+
  theme_classic()+
  labs(title="GDP of 10 most populated countries (2021)", x='Country', y='GDP')+
  theme(axis.text.x = element_text(hjust=1, angle = 90))

df_India <- df %>% 
  select(Country, Year, Curr_Xrate) %>% 
  filter(Country== 'India') %>% 
  ggplot(aes(Year, Curr_Xrate))+
  geom_point(color= 'green', size=2)+
  geom_line(color = 'yellow')+
  theme_classic()+
  labs(title = "Indian currency exchange rate over the years", x='Year', y='Exchange rate of INR')+
  theme(axis.text.x = element_text(hjust=1, angle = 90))
  
print(df_India)

pop_IndoChina <- df %>% 
  filter(Country %in% c('China', 'India')) %>% 
  ggplot(aes(Year, Population, color= Country))+
  geom_point(aes(color= Country), size=1)+
  geom_line(color= '#025887')+
  labs(title = 'Population- India vs China', x= 'Year', y='Population')+
  theme_classic()+
  theme(axis.text.x = element_text(hjust = 1, angle = 90))

print(pop_IndoChina)  
  
GDP_IndoUS <- df %>% 
  filter(Country %in% c('United States', 'India')) %>% 
  ggplot(aes(Year, GDP, color= Country))+
  geom_point(aes(color= Country), size=1)+
  geom_line(color= '#025887')+
  labs(title = 'GDP - India vs US', x= 'Year', y='GDP')+
  theme_classic()+
  theme(axis.text.x = element_text(hjust = 1, angle = 90))

print(GDP_IndoUS)  

df %>% 
  filter(Country == 'India') %>% 
  ggplot(aes(x= Year))+
  geom_line(aes(y = Exports, color= "Exports"))+
  geom_line(aes(y= Imports, color = "Imports"))+
  labs(title='Trend of Indias Exports and imports', x='Year', y='')+
  theme_classic()+
  theme(axis.text.x = element_text(hjust=1, angle=90))

df %>% 
  ggplot(aes(GDP, Exports))+
  geom_point(aes(color=Year))+
  labs(title="GDP vs Exports")+
  theme_classic()+
  theme(axis.text.x = element_text(hjust=1, angle=90))

df %>% 
  filter(Country == 'Afghanistan') %>% 
  ggplot(aes(Year, Percapita_GNI))+
  geom_point(color = 'red', size=1)+
  geom_line(color = 'black')+
  labs(title="Percapta GNI of Afghanistan", y='GNI Percapita')+
  theme_classic()

df_num <- df %>%select(-Country, -Year)  
kmeans_result <- kmeans(df_num, centers =3)  
clusters <- kmeans_result$cluster

df_num %>% 
  ggplot(aes(Population, GDP, color= factor(clusters)))+
  geom_point()

df %>% 
  select(Year, Population) %>% 
  group_by(Year) %>% 
  summarize(GlobalPopulation = sum(Population)) %>% 
  ggplot(aes(Year, GlobalPopulation))+
  geom_point(color='black')+
  geom_line(color='pink')+
  labs(title = 'Global Population trend over the years', x='Year', y='Population')+
  theme_classic()+
  theme(axis.text.x = element_text(hjust=1, angle = 90))

df %>% 
  select(Year, GDP) %>% 
  group_by(Year) %>% 
  summarize(GlobalGDP = sum(GDP)) %>% 
  ggplot(aes(Year, GlobalGDP))+
  geom_point(color='black')+
  geom_line(color='pink')+
  labs(title = 'Global GDP trend over the years', x='Year', y='GDP')+
  theme_classic()+
  theme(axis.text.x = element_text(hjust=1, angle = 90))

df_value <- df %>% 
  select(Country, Value_added) %>% 
  group_by(Country) %>% 
  summarise(Total_Value_added = sum(Value_added)) %>% 
  arrange(desc(Total_Value_added))

df_value[1:10,] %>% 
  ggplot(aes(reorder(Country, -Total_Value_added), Total_Value_added)) + 
  geom_bar(stat= "identity", fill= "#24DD0B")+
  labs(title = 'Total value added by top economies', x= 'Country', y='Total Value added')+
  theme_classic()+
  theme(axis.text.x = element_text(hjust = 1, angle = 90))









