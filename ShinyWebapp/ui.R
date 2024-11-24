#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

fluidPage(
  titlePanel("Choose a Plot"),
  
  # Main Tabset Panel
  tabsetPanel(
    # First Tab: Plots
    tabPanel(
      "Plots",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "plotChoice",
            "Choose a Plot",
            choices = c("GDP" = "gdp", "Growth" = "growth", "Gender" = "gender")
          ), 
          conditionalPanel(
            condition = "input.plotChoice === 'growth'",
            selectInput(
              "demoChoice",
              "Group by Region or Gender?",
              choices = c("Region" = "reg", "Gender" = "gender")
            )
          ), 
          conditionalPanel(
            condition = "input.plotChoice == 'gdp'",
            selectInput(
              "selectRegions",
              "Select Region(s): ",
              choices = c(
                "East Asia & Pacific", "Europe & Central Asia", 
                "Latin America & Caribbean", "Middle East & North Africa", 
                "North America", "South Asia", "Sub-Saharan Africa"
              ),
              selected = c("North America", "Europe & Central Asia"), 
              multiple = TRUE
            ),
            sliderInput(
              "yearRange",
              "Select Year Range",
              min = 1999, max = 2005, value = c(1999, 2005), step = 1, sep = ""
            )
          ),
          conditionalPanel(
            condition = "input.plotChoice == 'gender'",
            checkboxGroupInput(
              inputId = "plotType",
              label = "Choose Plot Type",
              choices = c("Bar", "Line"),
              selected = "Bar",
              
            ),
          )

        ),
        mainPanel(
          plotOutput("chosenPlot")
        )
      )
    ),
    # Second Tab: Data Tables
    tabPanel(
      "Datasets",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "dataChoice",
            "Choose a Dataset:",
            choices = c(
              "GDP Data" = "gdp", 
              "Demographic Data" = "demog", 
              "Demographic Data (With Region)" = "demog_region"
            )
          )
        ),
        mainPanel(
          dataTableOutput("chosenTable")
        )
      )
    )
  )
)
