library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# Let's prepare gdp_data:
gdp_data = read.csv("Datasets/GDP.PCAP.csv")
gdpmeta_data = read.csv("Datasets/Metadata_Country_API_NY.GDP.PCAP.CD_DS2_en_csv_v2_9803.csv")

gdp_data = gdp_data %>% select(1:2, 44:50)
# Renaming the columns
colnames(gdp_data) = c("CountryName", "CountryCode", "1999","2000", "2001", "2002", "2003", "2004", "2005")

gdpmeta_data$Region = ifelse(gdpmeta_data$Region == "", "Other", gdpmeta_data$Region)
gdpmeta_data$IncomeGroup = ifelse(gdpmeta_data$IncomeGroup == "", "Other", gdpmeta_data$IncomeGroup)

## Renaming Country Code
colnames(gdpmeta_data)[1] = "CountryCode"
gdp_data = merge(gdp_data, gdpmeta_data, by = "CountryCode")
gdp_data

# Uncomment below to create/update file to datasets folder
# write.csv(gdp_data, "Datasets/gdp_data.csv")


# Let's prepare demog_data:
demog_data = read.csv("Datasets/UNdata_secondary_enrollment.csv")

## Dropping the last 6 rows
demog_data = demog_data[1:2338,]

## Changing values of '-' to 0
demog_data = demog_data %>% mutate_all(funs(ifelse(. == "-", 0, .)))

demog_data$Value = as.numeric(demog_data$Value)


demog_data = demog_data %>% select(-Value.Footnotes)

# spreading demog data by Year
demog_data = demog_data %>% spread(key = Year, value = Value)
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
demog_data = demog_data %>% select(-Unit)

# Uncomment below to create/update file to datasets folder
# write.csv(demog_data, "Datasets/demog_data.csv)


# Let's prepare demog_data_with_region:
demog_long <- demog_data %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "Year", 
               values_to = "Value") %>%
  filter(!is.na(Value))  # Remove rows with NA values if needed

demog_long <- demog_long %>% 
  mutate(Countrycode = countrycode(demog_long$Country.or.Area, origin = "country.name", destination = "iso3c")) %>% 
  mutate(Countrycode = case_when(Country.or.Area == "Serbia and Montenegro" ~ "SCG",
                                 Country.or.Area == "Netherlands Antilles" ~ "ANT",
                                 TRUE ~ Countrycode))
# Merge with region data from gdp_data
demog_data_with_region <- merge(demog_long, gdp_data[, c("CountryCode", "Region")], 
                                by.x = "Countrycode", by.y = "CountryCode", all.x = TRUE) %>% 
  mutate(Region = case_when(Countrycode == "AIA" ~ "Latin America & Caribbean",
                            Countrycode == "ANT" ~ "Europe & Central Asia",
                            Countrycode == "COK" ~ "East Asia & Pacific",
                            Countrycode == "MSR" ~ "Latin America & Caribbean",
                            Countrycode == "NIU" ~ "East Asia & Pacific",
                            Countrycode == "SCG" ~ "Europe & Central Asia",
                            Countrycode == "TKL" ~ "East Asia & Pacific",
                            TRUE ~ Region))

# Convert 'Year' to numeric
demog_data_with_region$Year <- as.numeric(demog_data_with_region$Year)

# Uncomment below to create/update file to datasets folder
# write.csv(demog_data_with_region, "Datasets/demog_data_with_region")