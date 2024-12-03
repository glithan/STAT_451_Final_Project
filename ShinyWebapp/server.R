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
library(plotly)
library(viridis)

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
  map_data <- getURL("https://raw.githubusercontent.com/glithan/STAT_451_Final_Project/refs/heads/main/Datasets/map_data.csv")
  map_data <- read.csv(text = map_data)
  
  
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
  
  #Changing all 0 values in demog_data_with_region to NA
  demog_data_with_region$Value <- na_if(demog_data_with_region$Value, 0)
  
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
       scale_y_log10(
          labels = scales::comma_format(prefix = "$"),
          limits = c(5000, max(average_gdp_region_year$Average_GDP) * 1.2),
          breaks = scales::log_breaks(n = 5) #Adding the log scale breaks
        ) +
        scale_y_continuous(
          labels = scales::comma_format(prefix = "$"),
          limits = c(0, max(average_gdp_region_year$Average_GDP) * 1.2),
          breaks = scales::pretty_breaks(n = 5)  
        ) +
        scale_x_continuous(breaks = seq(1999, 2005, 1)) +
        scale_color_manual(values = custom_colors) +
        theme(
          plot.title = element_text(size = 22),
           axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 15)
        )
      
      
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
          scale_x_continuous(breaks = seq(1999, 2005, 2)) +
          theme(
            plot.title = element_text(size = 22),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 15),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 18)
          )
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
          scale_x_continuous(breaks = seq(1999, 2005, 2)) +
          theme(
            plot.title = element_text(size = 22),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 15)
          )
        
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
        
        
        plain_plot <- demog_data_long2 %>% 
          group_by(Subgroup, Year, Income_Low_High) %>% 
          summarize(Average_Enrollment = mean(Enrollment, na.rm = TRUE)) %>% 
          mutate(Group = paste(Subgroup, Income_Low_High, sep = " ")) %>%  # Combine variables into a single grouping
          filter(Income_Low_High != "Other") %>% 
          ggplot(aes(x = Year, y = Average_Enrollment, fill = Group)) +
          labs(title = "How does a country's income level affect the average enrollment of \nmen and women between from 1999 to 2005?",
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
          theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 22),
          plot.caption = element_text(size = 13),
          plot.subtitle = element_text(size = 13)
          )
        
        final_plot <- plain_plot
        
        if("Bar" %in% input$plotType) {
          final_plot <- final_plot + geom_col(position = position_dodge2(padding = 0.2), alpha = 0.7) 
        }
        if("Line" %in% input$plotType){
          final_plot <- final_plot + geom_line(aes(color = Group, group = Group), size = 0.7, show.legend = FALSE) 
        }

        
        final_plot
      }    
    })
  
  # Gather data sets to show in app
  gdp_data <- gdp_data %>% 
    select(-c(`X`, "SpecialNotes"))
  
 
  output$enrollmentMap <- renderPlotly({
    # Aggregate the data to ensure one entry per country
    aggregated_data <- map_data %>%
      group_by(iso3c) %>%
      summarise(
        male_female_ratio = mean(male_female_ratio),
        country = first(Country.or.Area)
      )
    aggregated_data$gray_value <- 1
    aggregated_data2 <- map_data %>% 
      group_by(iso3c) %>%
      summarise(
        male_female_ratio = mean(male_female_ratio, na.rm = TRUE),
        country = first(Country.or.Area)
      )
    longitudes <- seq(-180, 180, by = 10)  # Define longitudes for animation
    
    if(input$mapProjection == "globe"){
      plot_geo() %>% 
        # Add base layer gray for countries
        add_trace(
          data = aggregated_data,
          locations = ~iso3c,
          z = ~gray_value,
          locationmode = 'ISO-3',
          colorscale = list(c(0, "gray"), c(1, "gray")),
          showscale = FALSE,
          hoverinfo = "text",
          marker = list(line = list(width = 0))
        ) %>% 
        add_trace(
          data = aggregated_data2,
          z = ~ male_female_ratio,
          color = ~ male_female_ratio,
          colorscale = list(
            c(0, "blue"),
            c(0.5, "white"),
            c(1.0, "red")
          ),
          reversescale = TRUE,
          text = ~paste(
            country,
            ifelse(is.na(male_female_ratio), "",
                   paste("\n Average Male-to-Female\n Enrollment Ratio:", round(male_female_ratio, 2)))
          ),
          hoverinfo = "text",
          locations = ~iso3c,
          marker = list(line = list(color = "black", width = 0.5)),
          zmid = 1.0,
          colorbar = list(
            title = "Average Male-to-Female \nEnrollment Ratio",
            len = 0.8
          )
        ) %>% 
        layout(
          title = "Average Male-to-Female Enrollment Ratio by Country (1999-2005)",
          geo = list(
            showframe = FALSE,
            showcoastlines = TRUE,
            coastlinecolor = "black",
            showocean = TRUE,
            oceancolor = "lightblue",
            projection = list(type = "orthographic")
          )
        )
    }
    else{
      plot_geo() %>% 
        # Add base layer gray for countries
        add_trace(
          data = aggregated_data,
          locations = ~iso3c,
          z = ~gray_value,
          locationmode = 'ISO-3',
          colorscale = list(c(0, "gray"), c(1, "gray")),
          showscale = FALSE,
          hoverinfo = "text",
          marker = list(line = list(width = 0))
        ) %>% 
        add_trace(
          data = aggregated_data2,
          z = ~ male_female_ratio,
          color = ~ male_female_ratio,
          colorscale = list(
            c(0, "blue"),
            c(0.5, "white"),
            c(1.0, "red")
          ),
          reversescale = TRUE,
          text = ~paste(
            country,
            ifelse(is.na(male_female_ratio), "",
                   paste("\n Average Male-to-Female\n Enrollment Ratio:", round(male_female_ratio, 2)))
          ),
          hoverinfo = "text",
          locations = ~iso3c,
          marker = list(line = list(color = "black", width = 0.5)),
          zmid = 1.0,
          colorbar = list(
            title = "Average Male-to-Female \nEnrollment Ratio",
            len = 0.8
          )
        ) %>% 
        layout(
          title = "Average Male-to-Female Enrollment Ratio by Country (1999-2005)",
          geo = list(
            showframe = FALSE,
            showcoastlines = TRUE,
            coastlinecolor = "black",
            showocean = TRUE,
            oceancolor = "lightblue",
            projection = list(type = "equirectangular")
          )
        )
    }
    
  })
  
  aggregated_data2 <- gdp_long %>%
    group_by(CountryCode) %>%
    summarise(
      Avg_gdp_per_cap = mean(GDP, na.rm = TRUE),
      country = first(CountryName)
    )
  output$gdpMap <- renderPlotly({
    if(input$mapProjection == "globe"){
      plot_geo(aggregated_data2) %>%
        add_trace(
          z = ~log10(Avg_gdp_per_cap),  # Log-transform the data
          color = ~log10(Avg_gdp_per_cap),
          colorscale = "Viridis",
          text = ~paste(country, "<br>Avg GDP per Capita: $", round(Avg_gdp_per_cap, 2)), # Detailed hover text
          hoverinfo = "text",
          locations = ~CountryCode,
          marker = list(line = list(color = "black", width = 0.5)),
          colorbar = list(
            title = "Avg GDP Per Capita",
            tickvals = log10(c(100, 1000, 10000, 100000)),
            ticktext = c("$100", "$1k", "$10k", "$100k"),
            len = 0.8
          )
        ) %>%
        layout(
          title = list(
            text = "Average GDP Per Capita by Country (1999-2005)",
            x = 0.5
          ),
          geo = list(
            showframe = FALSE,
            showcoastlines = TRUE,
            coastlinecolor = "black",
            showocean = TRUE,
            oceancolor = "lightblue",
            projection = list(type = "orthographic")
          ),
          coloraxis_colorbar = list(
            title = "Avg GDP<br>Per Capita",
            tickvals = log10(c(100, 1000, 10000, 100000)),  # Tick positions on the log scale
            ticktext = c("$100", "$1k", "$10k", "$100k"),    # Human-readable labels
            len = 0.8
          )
        )
    }
    else{
      plot_geo(aggregated_data2) %>%
        add_trace(
          z = ~log10(Avg_gdp_per_cap),  # Log-transform the data
          color = ~log10(Avg_gdp_per_cap),
          colorscale = "Viridis",
          text = ~paste(country, "<br>Avg GDP per Capita: $", round(Avg_gdp_per_cap, 2)), # Detailed hover text
          hoverinfo = "text",
          locations = ~CountryCode,
          marker = list(line = list(color = "black", width = 0.5)),
          colorbar = list(
            title = "Avg GDP Per Capita",
            tickvals = log10(c(100, 1000, 10000, 100000)),
            ticktext = c("$100", "$1k", "$10k", "$100k"),
            len = 0.8
          )
        ) %>%
        layout(
          title = list(
            text = "Average GDP Per Capita by Country (1999-2005)",
            x = 0.5
          ),
          geo = list(
            showframe = FALSE,
            showcoastlines = TRUE,
            coastlinecolor = "black",
            showocean = TRUE,
            oceancolor = "lightblue",
            projection = list(type = "equirectangular")
          ),
          coloraxis_colorbar = list(
            title = "Avg GDP<br>Per Capita",
            tickvals = log10(c(100, 1000, 10000, 100000)),  # Tick positions on the log scale
            ticktext = c("$100", "$1k", "$10k", "$100k"),    # Human-readable labels
            len = 0.8
          )
        )
    }
  })
  
  output$chosenTable <- renderDataTable({
    dataset <- switch(input$dataChoice,
                      "gdp" = gdp_data,
                      "demog" = demog_data,
                      "demog_region" = demog_data_with_region,
                      "map_data" = map_data)
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
