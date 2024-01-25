#Global Economy Indicators Analysis using R

## Introduction
The analysis focuses on global economic indicators, exploring trends, patterns, and relationships among various countries. The dataset used for this analysis contains information on exchange rates, population, GNI, exports, imports, GDP, and other economic indicators for multiple countries over several years. The goal is to gain insights into the economic dynamics, comparing countries, and understanding how key indicators have evolved over time.

## Tools, Languages, and Libraries Used
The analysis was conducted using the R programming language, leveraging several libraries for data manipulation, visualization, and statistical testing. The primary libraries used include:
- Tidyverse: For data cleaning, transformation, and visualization.
- ggplot2: For creating informative and visually appealing plots.
- DescTools: For statistical analysis and hypothesis testing.
- Psych: For descriptive statistics.

The entire analysis was performed in RStudio, providing an interactive and dynamic environment for data exploration.

### Data Preparation
The analysis began with the installation and loading of necessary R packages, followed by reading the dataset into a data frame. Initial exploration involved viewing the data structure, handling missing values, and renaming columns for better clarity.

### Descriptive Statistics and Visualization
Descriptive statistics were computed for key economic indicators, such as GDP, population, and exchange rates. Visualizations included bar charts comparing the GDP of the ten most populated countries in 1970 and 2021, a line chart depicting the exchange rate of India over the years, and scatter plots illustrating the relationship between population and GDP.

### Clustering Analysis
A clustering analysis using k-means was performed on numerical variables, categorizing countries into three clusters based on population and GDP. Visualizations showcased the clusters on a scatter plot.

### Time Trends
The analysis explored global population and GDP trends over the years, providing insights into how these indicators have changed on a global scale.

### Hypothesis Testing
Several hypothesis tests were conducted to answer specific research questions:
1. Testing if the mean per capita GNI of the United States is greater than the global mean.
2. Examining if the mean exports of goods and services for 1970 differ significantly from 1971 globally.
3. Testing if the mean population for 2021 is greater than the mean population for 1970 globally.

## Conclusion
The comprehensive analysis of global economic indicators revealed interesting patterns and trends. Visualizations provided a clear understanding of how different countries compare in terms of GDP, population, and other key economic factors. The clustering analysis identified groups of countries with similar population and GDP characteristics.

Hypothesis testing added a statistical dimension to the analysis, allowing us to make evidence-based conclusions regarding specific economic questions. Overall, the report provides valuable insights into global economic trends and serves as a foundation for further in-depth studies and policy considerations.

The tools and libraries used in this analysis demonstrated the power of the R programming language for data exploration, visualization, and statistical analysis. The combination of Tidyverse, ggplot2, DescTools, and Psych provided a robust and efficient environment for conducting a comprehensive examination of global economic indicators.

In conclusion, this report contributes to the understanding of global economic dynamics, laying the groundwork for informed decision-making and further research in the field of international economics.
