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

# Define server logic required to draw a histogram
function(input, output){
  # Load the data
  x = getURL("https://raw.githubusercontent.com/glithan/STAT_451_Final_Project/refs/heads/main/Datasets/gdp_data.csv")
  gdp_data = read.csv(text = x)
  x = getURL("https://raw.githubusercontent.com/glithan/STAT_451_Final_Project/refs/heads/main/Datasets/demog_data_with_region.csv")
  demog_data_with_region = read.csv(text = x)
  x = getURL("https://raw.githubusercontent.com/glithan/STAT_451_Final_Project/refs/heads/main/Datasets/demog_data.csv")
  demog_data = read.csv(text = x)
  
  
  colnames(gdp_data)[3:9] <- sub("^X", "", colnames(gdp_data)[3:9])
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
      ggplot(average_gdp_by_region_year, aes(x = Year, y = Average_GDP, color = Region)) +
        geom_line() +
        geom_point() +
        labs(title = "Average GDP Per Capita by Region Over Time", x = "Year", y = "Average GDP Per Capita (USD)") +
        scale_y_continuous(breaks = seq(0,60000, 5000), labels = scales::dollar_format()) +
        scale_x_continuous(breaks = seq(1999, 2005, 2))
      
      
    } else if(input$plotChoice == "growth"){
      if (input$demoChoice == 'reg'){
        ggplot(demog_growth, aes(x = Year, y = Growth, color = Region)) +
          geom_line() +
          geom_point() +
          labs(title = "Growth in Secondary Education Enrollment by Region", x = "Year", y = "Enrollment Growth (%)") +
          scale_y_continuous(breaks = seq(-100, 100, 5)) +
          scale_x_continuous(breaks = seq(1999, 2005, 2))
      } else {
        ggplot(demog_growth_gender, aes(x = Year, y = Growth, color = Subgroup)) +
          geom_line() +
          geom_point() +
          labs(title = "Growth in Secondary Education Enrollment by Region", x = "Year", y = "Enrollment Growth (%)") + 
          scale_y_continuous(breaks = seq(-100, 100, 5)) +
          scale_x_continuous(breaks = seq(1999, 2005, 2))
        
        }
      }
      
  })
}
