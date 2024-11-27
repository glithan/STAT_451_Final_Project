library(tidyverse)
library(ggplot2)
library(dplyr)
library(countrycode)
library(plotly)
library(viridis)


gdp_data = read.csv("Datasets/GDP.PCAP.csv")
demog_data = read.csv("Datasets/UNdata_secondary_enrollment.csv")
gdpmeta_data = read.csv("Datasets/Metadata_Country_API_NY.GDP.PCAP.CD_DS2_en_csv_v2_9803.csv")
continents = read.csv("Datasets/continents2.csv")

gdp_data = gdp_data %>% select(1:2, 44:50)
# Renaming the columns
colnames(gdp_data) = c("CountryName", "CountryCode", "1999","2000", "2001", "2002", "2003", "2004", "2005")
print(sum(is.na(gdp_data)))

# Printing num.na in each column:
print(colSums(is.na(gdp_data)))
head(gdp_data)

tail(demog_data, 15)
## Dropping the last 6 rows
demog_data = demog_data[1:2338,]

## Changing values of '-' to 0
demog_data = demog_data %>% mutate_all(funs(ifelse(. == "-", 0, .)))

demog_data$Value = as.numeric(demog_data$Value)
print(colSums(is.na(demog_data)))

print(summary(demog_data))

demog_data = demog_data %>% select(-Value.Footnotes)

# spreading demog data by Year
demog_data = demog_data %>% spread(key = Year, value = Value)
print(sum(is.na(demog_data)))
head(demog_data)


for (i in 1:6) {
  demog_data$`1999` = ifelse(is.na(demog_data$`1999`), demog_data$`2000`, demog_data$`1999`)
  demog_data$`2000` = ifelse(is.na(demog_data$`2000`), demog_data$`2001`, demog_data$`2000`)
  demog_data$`2001` = ifelse(is.na(demog_data$`2001`), demog_data$`2002`, demog_data$`2001`)
  demog_data$`2002` = ifelse(is.na(demog_data$`2002`), demog_data$`2003`, demog_data$`2002`)
  demog_data$`2003` = ifelse(is.na(demog_data$`2003`), demog_data$`2004`, demog_data$`2003`)
  demog_data$`2004` = ifelse(is.na(demog_data$`2004`), demog_data$`2005`, demog_data$`2004`)
  
  demog_data$`2005` = ifelse(is.na(demog_data$`2005`), demog_data$`2004`, demog_data$`2005`)
  demog_data$`2004` = ifelse(is.na(demog_data$`2004`), demog_data$`2003`, demog_data$`2004`)
  demog_data$`2003` = ifelse(is.na(demog_data$`2003`), demog_data$`2002`, demog_data$`2003`)
  demog_data$`2002` = ifelse(is.na(demog_data$`2002`), demog_data$`2001`, demog_data$`2002`)
  demog_data$`2001` = ifelse(is.na(demog_data$`2001`), demog_data$`2000`, demog_data$`2001`)
  demog_data$`2000` = ifelse(is.na(demog_data$`2000`), demog_data$`1999`, demog_data$`2000`)
  
}


## Printing cols where all year values are NA
print(sum(is.na(demog_data)))
demog_data = demog_data %>% select(-Unit)


print(summary(gdpmeta_data))

print(colSums(is.na(gdpmeta_data)))

gdpmeta_data$Region = ifelse(gdpmeta_data$Region == "", "Other", gdpmeta_data$Region)
gdpmeta_data$IncomeGroup = ifelse(gdpmeta_data$IncomeGroup == "", "Other", gdpmeta_data$IncomeGroup)

## Renaming Country Code
colnames(gdpmeta_data)[1] = "CountryCode"

gdp_data = merge(gdp_data, gdpmeta_data, by = "CountryCode")

summary(gdp_data)

gdp_data <- gdp_data %>% 
  pivot_longer(cols = c(3:9), names_to = "Year", values_to = "GDP_per_capita") %>% 
  select(CountryCode, CountryName, Region, IncomeGroup, Year, GDP_per_capita)

demog_data_long <- demog_data %>% 
  pivot_longer(cols = c(4:10), names_to = "Year", values_to = "Enrollment")

demog_data_long$iso3c <- countrycode(demog_data_long$Country.or.Area, origin = "country.name", destination = "iso3c")

demog_data_long <- demog_data_long %>% 
  inner_join(gdp_data, by = c("iso3c" = "CountryCode", "Year")) %>% 
  rename("CountryCode" = iso3c) %>% 
  relocate(CountryCode, .before = 1)

demog_data_long <- demog_data_long %>% 
  mutate(Income_Low_High = case_when(IncomeGroup %in% c("Low income", "Lower middle income") ~ "Lower income",
                                     IncomeGroup %in% c("High income", "Upper middle income") ~ "Upper income",
                                     TRUE ~ NA))

# Drop redundant fields
demog_data_long <- demog_data_long %>% 
  select(c(CountryCode, Country.or.Area, Subgroup, Year, Enrollment,
           Region, IncomeGroup, Income_Low_High))

demog_data_long <- demog_data_long %>% 
  mutate(Enrollment = case_when(Enrollment == 0 ~ NA,
                                TRUE ~ Enrollment))

map_data <- map_data("world")
map_data$iso3c <- countrycode(map_data$region, origin = "country.name", destination = "iso3c")
demog_data_long <- map_data %>% 
  left_join(demog_data_long, by = c("iso3c" = "CountryCode"))

demog_data_long_avg_in_years <- demog_data_long %>% 
  group_by(iso3c, Country.or.Area, long, lat, Subgroup, group) %>% 
  summarize(avg_enrollment = mean(Enrollment, na.rm = TRUE))

# Separate male and female data into two data frames
female_data <- demog_data_long_avg_in_years %>% filter(Subgroup == "Female")
male_data <- demog_data_long_avg_in_years %>% filter(Subgroup == "Male")

# Rename avg_enrollment for clarity before joining
female_data <- female_data %>% rename(female_enrollment = avg_enrollment)
male_data <- male_data %>% rename(male_enrollment = avg_enrollment)

# Join male and female data on country and coordinates
enrollment_ratio_data <- female_data %>%
  inner_join(male_data, by = c("iso3c", "Country.or.Area", "long", "lat", "group")) %>%
  mutate(male_female_ratio = male_enrollment / female_enrollment)

map_data <- map_data %>% 
  full_join(enrollment_ratio_data, by = c("iso3c", "long", "lat"))

continents <- continents %>% 
  select(c(alpha.3, region))

map_data <- map_data %>% 
  full_join(continents, by = c("iso3c" = "alpha.3"))

# Uncomment to save full map dataset if needed
#write.csv(map_data, "map_data.csv")

