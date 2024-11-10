# STAT_451_Final_Project
## Eliot Ozaki, Glit Hanpanitkitkan, Ethan Kawahara, Andrew Sousa

### Our Question and Motivation:
##### How does a country's income (GDP per capita in USD) affect the enrollment rates of men and women, and how do they differ between each regions of the world from 1999 to 2005? 

##### The reason why we want to use data manipulation and data visualization to solve this question and topic is because throughout history, women have encountered greater obstacles of obtaining the same level of education as men. Many people widely believe that "third-world regions" tend to struggle with educational gender equality compared to "first-world regions" who typically tend to have higher individual and family incomes.

##### We plan to use multiple datasets with one being about the number of secondary education enrollment by country and by gender from 1999 to 2005. The key features of this dataset includes the gender and country or area for each year. Another dataset contains the GDP per capita in USD of different countries from 1960 to 2023. The key features of this dataset includes the GDP per capita of each country for every year. Because we are only looking from 1999 to 2005, we will filter to only look at the GDP per capita during the years we are interested in. The last dataset we are using is about census data of countries for each year. The key features of this dataset includes the total population and sex ratio for each country for every year which we will also look at only 1999-2005.

##### With the combination of these datasets we will have the GDP per capita of each country and region along with the number of secondary enrollments by gender for each country. With this information, we can also compare with the ratio of men to women in each country and look at the potential correlation in GDP per capita and the enrollment rates of between men and women. With enrollment rates, GDP per capita, and sex ratios available by country and year, these datasets provide a sufficient foundation to analyze how economic factors relate to gender differences in education. Filtering each dataset to the same time period (1999-2005) ensures consistency in our comparisons. By visualizing enrollment rates and GDP data across regions, we can reveal trends and disparities, making it possible to answer our question with clarity.

### Datasets we plan to use:
#### GDP per capita: https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
#### Enrollment in secondary education: https://data.un.org/Data.aspx?d=GenderStat&f=inID%3a64
#### Population information: https://www.census.gov/data-tools/demo/idb/#/table?COUNTRY_YEAR=2024&COUNTRY_YR_ANIM=2024https:%2F%2Fwww.census.gov%2Fdata-tools%2Fdemo%2Fidb%2F%23%2Ftable%3FCOUNTRY_YEAR%3D2024&menu=tableViz&TABLE_RANGE=1999,2005&TABLE_YEARS=1999,2000,2001,2002,2003,2004,2005,2024&TABLE_USE_RANGE=Y&TABLE_USE_YEARS=Y&TABLE_STEP=1&TABLE_ADD_YEARS=2024