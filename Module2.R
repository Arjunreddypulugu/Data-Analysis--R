#Arjun R Pulugu, ALY6010, MPS Analytics

install.packages('tidyverse')
install.packages('ggplot2')
install.packages('ggeasy')
install.packages('ggthemes')

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggeasy)

df <- read_csv('/Users/arjunreddypulugu/Documents/R2 folder/Global Economy Indicators.csv')
view(df)

sum(is.na(df))
dim(df)
df <-na.omit(df)

df_sum <- summary(df)
view(df_sum)

install.packages('psych')
library(psych)

df_psych <- psych::describe(df %>% select(-CountryID, -Country, -Year, -Currency))
view(df_psych)

df1<- df %>% 
  select(Country, Year, `IMF based exchange rate`, Population, `Per capita GNI`, `Exports of goods and services`, `Imports of goods and services`, `Total Value Added`, `Gross Domestic Product (GDP)` ) 
  
colnames(df1) <- c('country', 'year', 'x_rate', 'population', 'percap_gni', 'exports', 'imports', 'value_added', 'gdp')
view(df1)

df1 %>% 
  filter(country == 'China') %>% 
  ggplot(aes(x= year))+
  geom_line(aes(y = exports, color= "exports"))+
  geom_line(aes(y= imports, color = "imports"))+
  labs(title='Trend of Chinas Exports and imports', x='Year', y='')+
  theme_classic()+
  theme(axis.text.x = element_text(hjust=1, angle=90))

df1 %>% 
  filter(country == 'Israel') %>% 
  ggplot(aes(year, percap_gni))+
  geom_point()+
  geom_line(color = 'magenta')+
  labs(title="Percapta GNI of Israel", y='GNI Percapita')+
  theme_classic()

df1 %>% 
  filter(country == 'India') %>% 
  ggplot(aes(population, exports))+
  geom_point()+
  geom_smooth(se = FALSE)+
  labs(title = 'Growing exports with Population (INDIA)', x='Population', y='Exports')+
  theme_classic()

df1 %>% 
  ggplot(aes(value_added, exports))+
  geom_point(aes(year))+
  geom_smooth()+
  labs(title = 'Variance of value added with exports', x='Value added', y='exports')+
  theme_classic()

df1 %>% 
  select(year, population) %>% 
  group_by(year) %>% 
  summarise(population = sum(population)) %>% 
  ggplot(aes(year, population))+
  geom_jitter()+
  geom_smooth()+
  labs(title = 'Global population over the years', x='year', y ='population')+
  theme_classic()

df_exports <- df1 %>% 
  select(country, exports) %>% 
  group_by(country) %>% 
  summarise(exports = sum(exports)) %>% 
  arrange(desc(exports))

df_exports[1:10, ] %>% 
  ggplot(aes(reorder(country, -exports), exports))+
  geom_bar(stat = 'identity', fill='skyblue')+
  labs(title = 'Top net exporters', x='country', y='exports')+
  theme_classic()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

df_imports <- df1 %>% 
  select(country, imports) %>% 
  group_by(country) %>% 
  summarise(imports = sum(imports)) %>% 
  arrange(desc(imports))

df_imports[1:10, ] %>% 
  ggplot(aes(reorder(country, -imports), imports))+
  geom_bar(stat = 'identity', fill='#12E683')+
  labs(title = 'Top net importers', x='country', y='imports')+
  theme_classic()+
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

df1 %>% 
  select(country, exports) %>% 
  filter(country %in% c('Japan', 'United States', 'China', 'India')) %>% 
  ggplot(aes(country, exports, fill= country))+
  geom_violin()+
  labs(title= 'Distribution of exports', y='Exports')+
  theme_classic()

df1 %>% 
  select(country, gdp) %>% 
  filter(country %in% c('Japan', 'United States', 'China', 'India')) %>% 
  ggplot(aes(country, gdp, fill= country))+
  geom_violin()+
  labs(title= 'Distribution of gdp', y='gdp')+
  theme_classic()

df1 %>% 
  select(country, population) %>% 
  filter(country %in% c('China', 'India')) %>% 
  ggplot(aes(country, population, fill= country))+
  geom_violin()+
  labs(title= 'Distribution of population', y='population')+
  theme_classic()

df1 %>% 
  select(year, country, x_rate) %>% 
  filter(country %in% c("India", 'Pakistan')) %>% 
  ggplot(aes(year, x_rate, fill=country, color=country))+
  geom_line()+
  labs(title = 'Exchange rate progression', x='Year', y='Forex')+
  theme_classic()




