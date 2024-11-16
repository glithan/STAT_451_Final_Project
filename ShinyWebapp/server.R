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
  x = getURL("https://github.com/glithan/STAT_451_Final_Project/tree/main/Datasets/gdp_data.csv")
  gdp_data = read.csv(text = x)
  x = getURL("https://github.com/glithan/STAT_451_Final_Project/tree/main/Datasets/demog_data_with_region.csv")
  demog_data_with_region = read.csv(text = x)
  
  print(summary(demog_data_with_region))
  
  gdp_long <- gdp_data %>%
    pivot_longer(cols = colnames(gdp_data) %>% select(starts_with("19") | starts_with("20")),
                 names_to = "Year", 
                 values_to = "GDP") %>%
    filter(!is.na(Region) & Region != "World")
  
  # Convert 'Year' to numeric
  gdp_long$Year <- as.numeric(gdp_long$Year)
  
  # Calculate the average GDP for each region by year
  average_gdp_by_region_year <- gdp_long %>%
    group_by(Region, Year) %>%
    summarize(Average_GDP = mean(GDP, na.rm = TRUE))
  
  demog_mean_values <- demog_data_with_region %>%
    group_by(Region, Year) %>%
    summarize(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    arrange(Region, Year)  # Sort by region and year
  
  # Calculate growth relative to the first year in each region
  demog_growth <- demog_mean_values %>%
    group_by(Region) %>%
    mutate(Growth = ((Mean_Value / first(Mean_Value)) * 100) - 100)  # Standardize to the first year
  
  output$chosenPlot <- renderPlot({
    if(input$plotChoice == "gdp"){
      ggplot(average_gdp_by_region_year, aes(x = Year, y = Average_GDP, color = Region)) +
        geom_line() +
        geom_point() +
        labs(title = "Average GDP Per Capita by Region Over Time", x = "Year", y = "Average GDP") 
    } else if(input$plotChoice == "growth"){
      growth <- ggplot(demog_growth, aes(x = Year, y = Growth, color = Region)) +
        geom_line() +
        geom_point() +
        labs(title = "Growth in Secondary Education Enrollment by Region", x = "Year", y = "Enrollment Growth (%)")
    }
  })
}
