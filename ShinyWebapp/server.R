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
