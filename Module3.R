#Arjun R Pulugu, ALY6010: Probability theory and statistics, MPS Analytics

install.packages('tidyverse')
library(tidyverse)
df = read_csv('/Users/arjunreddypulugu/Documents/R2 folder/Global Economy Indicators.csv')
view(df)


#Research Question 1:
#Null Hypothesis (H0: mu = x): The mean per capita GNI of United States is equal to the global mean per capita GNI.
#Alternative Hypothesis (H1: mu > x): The mean per capita GNI of United States is greater than the global mean per capita GNI.
result_1 <- t.test(df$`Per capita GNI`[df$Country == "United States"], mu = mean(df$`Per capita GNI`), alternative = "greater")
result_1
#Interpretation
# 1)The very small p-value (3.981e-12) suggests strong evidence against the null hypothesis. It implies that the observed mean "Per capita GNI" for the United States is significantly different from 8965.565.
# 2)The positive test statistic (8.8136) indicates that the sample mean is significantly higher than the hypothesized population mean.
# 3)The 95 percent confidence interval (27718.34 to Inf) provides a range of values where we can be 95 percent confident that the true mean lies. 
  #In this case, the interval is from 27718.34 to infinity, indicating a substantial difference between the observed mean and the hypothesized mean
# In Conclusion, based on the low p-value and the positive test statistic, we have evidence to reject the null hypothesis and conclude that the "Per capita GNI" for the United States is significantly greater than 8965.565.


#Research Question 2:
#Null Hypothesis (H0: mu = x): The mean exports of goods and services for the year 1970 is equal to the mean for the year 1971 across all countries.
#Alternative Hypothesis (H1: mu != x): The mean exports of goods and services for the year 1970 is different from the mean for the year 1971 across all countries.
result_2 <- t.test(df$`Exports of goods and services`[df$Year == 1970], df$`Exports of goods and services`[df$Year == 1971])
result_2
#Interpretation:
#Given the high p-value of 0.725 and the confidence interval containing 0, there is not enough evidence to reject the null hypothesis. 
#The test suggests that the difference in mean exports of goods and services between 1970 and 1971 is not statistically significant. 
#The means of the two years are estimated to be 2060835295 and 2302119435, respectively, and the confidence interval suggests that the true difference in means could be anywhere from -1589061348 to 1106493070.
#In summary, based on this analysis, we do not have sufficient evidence to conclude that there is a significant difference in mean exports of goods and services between 1970 and 1971 across all countries.


#Research Question 3:
#Null Hypothesis (H0: mu = x): The mean Population for the year 2021 is equal to the mean Population for the year 1970 across all countries.
#Alternative Hypothesis (H1:mu > x): The mean Population for the year 2021 is greater than the mean Population for the year 1970 across all countries.
result_3 <- t.test(df$`Population`[df$Year == 2021], df$`Population`[df$Year == 1970], alternative = "greater")
result_3
#interpretation
#The p-value is 0.06081, which is close to, but slightly above, the common significance level of 0.05
#This suggests weak evidence against the null hypothesis.
#In conclusion, based on this test, there is a suggestion that the population mean of the year 2021 might be greater than the population mean of the year 1970
#And based on the context of the study, we can reject the null hypothesis


