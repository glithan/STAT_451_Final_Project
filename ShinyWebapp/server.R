#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(RCurl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(fastmap)
library(htmltools)
library(tidyselect)
library(scales)
library(countrycode)
library(DT)

# Define server logic required to draw a histogram
function(input, output){
  # Load the data
  x = getURL("https://raw.githubusercontent.com/glithan/STAT_451_Final_Project/refs/heads/main/Datasets/gdp_data.csv")
  gdp_data = read.csv(text = x)
  x = getURL("https://raw.githubusercontent.com/glithan/STAT_451_Final_Project/refs/heads/main/Datasets/demog_data_with_region.csv")
  demog_data_with_region = read.csv(text = x)
  x = getURL("https://raw.githubusercontent.com/glithan/STAT_451_Final_Project/refs/heads/main/Datasets/demog_data.csv")
  demog_data = read.csv(text = x)
  gdp_data2 <- getURL("https://raw.githubusercontent.com/glithan/STAT_451_Final_Project/refs/heads/main/Datasets/GDP.PCAP.csv") 
  gdp_data2 <- read.csv(text=gdp_data2)
  demog_data2 <- getURL("https://raw.githubusercontent.com/glithan/STAT_451_Final_Project/refs/heads/main/Datasets/UNdata_secondary_enrollment.csv")
  demog_data2 <- read.csv(text = demog_data2)
  gdpmeta_data2 <- getURL("https://raw.githubusercontent.com/glithan/STAT_451_Final_Project/refs/heads/main/Datasets/Metadata_Country_API_NY.GDP.PCAP.CD_DS2_en_csv_v2_9803.csv")
  gdpmeta_data2 <- read.csv(text = gdpmeta_data2)
  
  
  colnames(gdp_data)[3:10] <- sub("^X", "", colnames(gdp_data)[3:10])
  colnames(demog_data)[4:10] <- sub("^X", "", colnames(demog_data)[4:10])
    
  gdp_long <- gdp_data %>%
    pivot_longer(cols = starts_with("19") | starts_with("20"),
                 names_to = "Year", 
                 values_to = "GDP") %>%
    filter(!is.na(Region) & Region != "World")
  
  # Convert 'Year' to numeric
  gdp_long$Year <- as.numeric(gdp_long$Year)
  
  
  # Calculate the average GDP for each region by year
  average_gdp_by_region_year <- gdp_long %>%
    group_by(Region, Year) %>%
    summarize(Average_GDP = mean(GDP, na.rm = TRUE))
  
  # Changing all "NA" regions in demog_data_with_region to "Other":
  demog_data_with_region$Region[is.na(demog_data_with_region$Region)] <- "Other"
  
  demog_mean_values <- demog_data_with_region %>%
    group_by(Region, Year) %>%
    summarize(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    arrange(Region, Year)  # Sort by region and year
  
  demog_mean_values_gender <- demog_data_with_region %>%
    group_by(Subgroup, Year) %>%
    summarize(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    arrange(Subgroup, Year)  # Sort by region and year
  
  # Calculate growth relative to the first year in each region
  demog_growth <- demog_mean_values %>%
    group_by(Region) %>%
    mutate(Growth = ((Mean_Value / first(Mean_Value)) * 100) - 100)  # Standardize to the first year
  
  # Calculate growth relative to the first year by gender
  demog_growth_gender <- demog_mean_values_gender %>%
    group_by(Subgroup) %>%
    mutate(Growth = ((Mean_Value / first(Mean_Value)) * 100) - 100)  # Standardize to the first year
  
  output$chosenPlot <- renderPlot({
    if(input$plotChoice == "gdp"){
      average_gdp_region_year <- average_gdp_by_region_year %>% 
        filter((Year >= min(input$yearRange)) & (Year <= max(input$yearRange))) %>% 
        filter(Region %in% input$selectRegions) 
      # Calculate gdp change by region
      gdp_change <- average_gdp_region_year %>%
        group_by(Region) %>%
        summarize(Change = last(Average_GDP) - first(Average_GDP), .groups = "drop")
      # Reorder region factor based on GDP change
      average_gdp_region_year <- average_gdp_region_year %>%
        mutate(Region = factor(Region, levels = gdp_change$Region[order(gdp_change$Change, decreasing = TRUE)]))
      
      custom_colors <- c(
        "East Asia & Pacific" = "#CC79A7",
        "Europe & Central Asia" = "#648fff",
        "Latin America & Caribbean" = "#D55E00",
        "Middle East & North Africa" = "#F0E442",
        "North America" = "#009E73",
        "Other" = "#56B4E9",
        "South Asia" = "#E69F00",
        "Sub-Saharan Africa" = "#7f7f7f"
      )
        ggplot(average_gdp_region_year, aes(x = Year, y = Average_GDP, color = Region)) +
        geom_line() +
        geom_point() +
        labs(title = "Average GDP Per Capita by Region Over Time", x = "Year", y = "Average GDP Per Capita (USD)") +
        scale_y_continuous(
          labels = scales::comma_format(prefix = "$"),
          limits = c(0, max(average_gdp_region_year$Average_GDP) * 1.2),
          breaks = scales::pretty_breaks(n = 5)  
        ) +
        scale_x_continuous(breaks = seq(1999, 2005, 1)) +
        scale_color_manual(values = custom_colors)
      
      
    }  
    else if(input$plotChoice == "growth"){
      if (input$demoChoice == 'reg'){
        # Calculate enrollment change for regions
        region_enrollment_change <- demog_growth %>%
          group_by(Region) %>%
          summarize(Change = last(Growth) - first(Growth), .groups = "drop")
        # Reorder the Region factor based on enrollment change
        demog_growth <- demog_growth %>%
          mutate(Region = factor(Region, levels = region_enrollment_change$Region[order(region_enrollment_change$Change, decreasing = TRUE)]))
        
        ggplot(demog_growth, aes(x = Year, y = Growth, color = Region)) +
          geom_line() +
          geom_point() +
          labs(title = "Growth in Secondary Education Enrollment by Region", x = "Year", y = "Enrollment Growth (%)") +
          scale_y_continuous(breaks = seq(-100, 100, 5)) +
          scale_x_continuous(breaks = seq(1999, 2005, 2))
      } else {
        # Calculate enrollment change for gender
        gender_enrollment_change <- demog_growth_gender %>%
          group_by(Subgroup) %>%
          summarize(Change = last(Growth) - first(Growth), .groups = "drop")
        
        # Reorder the Subgroup factor based on enrollment change
        demog_growth_gender <- demog_growth_gender %>%
          mutate(Subgroup = factor(Subgroup, levels = gender_enrollment_change$Subgroup[order(gender_enrollment_change$Change, decreasing = TRUE)]))
        ggplot(demog_growth_gender, aes(x = Year, y = Growth, color = Subgroup)) +
          geom_line() +
          geom_point() +
          labs(title = "Growth in Secondary Education Enrollment by Gender", x = "Year", y = "Enrollment Growth (%)") + 
          scale_y_continuous(breaks = seq(-100, 100, 5)) +
          scale_x_continuous(breaks = seq(1999, 2005, 2))
        
        }
    }
    else if(input$plotChoice == "gender"){
      
        # Selecting only X2000-X2005 from the GDP data
        gdp_data2 = gdp_data2 %>% select(1:2, 44:50)
        # Renaming the columns
        colnames(gdp_data2) = c("CountryName", "CountryCode", "1999","2000", "2001", "2002", "2003", "2004", "2005")
        
        tail(demog_data2, 15)
        ## Dropping the last 6 rows
        demog_data2 = demog_data2[1:2338,]
        
        ## Changing values of '-' to 0
        demog_data2 = demog_data2 %>% mutate_all(funs(ifelse(. == "-", 0, .)))
        
        demog_data2$Value = as.numeric(demog_data2$Value)
        
        demog_data2 = demog_data2 %>% select(-Value.Footnotes)
        
        # spreading demog data by Year
        demog_data2 = demog_data2 %>% spread(key = Year, value = Value)
        
        for (i in 1:6) {
          demog_data2$`1999` = ifelse(is.na(demog_data2$`1999`), demog_data2$`2000`, demog_data2$`1999`)
          demog_data2$`2000` = ifelse(is.na(demog_data2$`2000`), demog_data2$`2001`, demog_data2$`2000`)
          demog_data2$`2001` = ifelse(is.na(demog_data2$`2001`), demog_data2$`2002`, demog_data2$`2001`)
          demog_data2$`2002` = ifelse(is.na(demog_data2$`2002`), demog_data2$`2003`, demog_data2$`2002`)
          demog_data2$`2003` = ifelse(is.na(demog_data2$`2003`), demog_data2$`2004`, demog_data2$`2003`)
          demog_data2$`2004` = ifelse(is.na(demog_data2$`2004`), demog_data2$`2005`, demog_data2$`2004`)
          
          demog_data2$`2005` = ifelse(is.na(demog_data2$`2005`), demog_data2$`2004`, demog_data2$`2005`)
          demog_data2$`2004` = ifelse(is.na(demog_data2$`2004`), demog_data2$`2003`, demog_data2$`2004`)
          demog_data2$`2003` = ifelse(is.na(demog_data2$`2003`), demog_data2$`2002`, demog_data2$`2003`)
          demog_data2$`2002` = ifelse(is.na(demog_data2$`2002`), demog_data2$`2001`, demog_data2$`2002`)
          demog_data2$`2001` = ifelse(is.na(demog_data2$`2001`), demog_data2$`2000`, demog_data2$`2001`)
          demog_data2$`2000` = ifelse(is.na(demog_data2$`2000`), demog_data2$`1999`, demog_data2$`2000`)
          
        }
        
        
        ## Printing cols where all year values are NA
        demog_data2 = demog_data2 %>% select(-Unit)
        
        gdpmeta_data2$Region = ifelse(gdpmeta_data2$Region == "", "Other", gdpmeta_data2$Region)
        gdpmeta_data2$IncomeGroup = ifelse(gdpmeta_data2$IncomeGroup == "", "Other", gdpmeta_data2$IncomeGroup)
        
        ## Renaming Country Code
        colnames(gdpmeta_data2)[1] = "CountryCode"
        
        ## Merging gdp and gdpmeta datasets
        gdp_data2 = merge(gdp_data2, gdpmeta_data2, by = "CountryCode")
        
        demog_data_long2 <- demog_data2 %>% 
          pivot_longer(cols = c(4:10), names_to = "Year", values_to = "Enrollment")
        
        demog_data_long2$iso3c <- countrycode(demog_data_long2$Country.or.Area, origin = "country.name", destination = "iso3c")
        
        demog_data_long2 <- demog_data_long2 %>% 
          inner_join(gdp_data2, by = c("iso3c" = "CountryCode")) %>% 
          rename("CountryCode" = iso3c) %>% 
          relocate(CountryCode, .before = 1)
        
        demog_data_long2 <- demog_data_long2 %>% 
          mutate(Income_Low_High = case_when(IncomeGroup %in% c("Low income", "Lower middle income") ~ "Lower income",
                                             IncomeGroup %in% c("High income", "Upper middle income") ~ "Upper income",
                                             TRUE ~ NA))
        
        
        demog_data_long2 %>% 
          group_by(Subgroup, Year, Income_Low_High) %>% 
          summarize(Average_Enrollment = mean(Enrollment, na.rm = TRUE)) %>% 
          mutate(Group = paste(Subgroup, Income_Low_High, sep = " ")) %>%  # Combine variables into a single grouping
          filter(Income_Low_High != "Other") %>% 
          ggplot(aes(x = Year, y = Average_Enrollment, fill = Group)) +
          geom_col(position = position_dodge2(padding = 0.2), alpha = 0.7) +
          geom_line(aes(color = Group, group = Group), size = 0.7, show.legend = FALSE) + 
          labs(title = "How does a country's income level affect the enrollment rates of \nmen and women between from 1999 to 2005?",
               subtitle = "Average global enrollment over time by gender and income level",
               x = "Year",
               y = "Average Enrollment",
               fill = "Gender and Income Level",
               caption = "Source: UN Data enrollment in secondary education") +
          scale_y_continuous(
            expand = c(0, 0),  # Removes extra space at the bottom
            limits = c(0, 1600000),  # Manually set upper limit to a higher value (adjust this value if needed)
            breaks = seq(0, 2000000, by = 200000)  # Set the breaks according to your desired range
          ) +  
          scale_fill_manual(values = c("Female Lower income" = "lightpink", 
                                       "Female Upper income" = "magenta", 
                                       "Male Lower income" = "lightblue", 
                                       "Male Upper income" = "blue")) + 
          scale_color_manual(values = c("Female Lower income" = "lightpink", 
                                        "Female Upper income" = "magenta", 
                                        "Male Lower income" = "lightblue", 
                                        "Male Upper income" = "blue")) +
          theme_bw() +
          theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  # Adjust margins to minimize space around the plot
        
      }    
    })
  output$chosenTable <- renderDataTable({
    dataset <- switch(input$dataChoice,
                      "gdp" = gdp_data,
                      "demog" = demog_data,
                      "demog_region" = demog_data_with_region)
    datatable(
      dataset,
      options = list(
        dom = 'Bfrtip',    # Arrange the table elements
        pageLength = 10, # Set default rows per page
        scrollX = TRUE   # Enable horizontal scrolling for wide tables
      ),
      filter = "top"
    )
  })
  
}
