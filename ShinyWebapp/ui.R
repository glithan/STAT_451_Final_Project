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
            checkboxGroupInput(
              "selectRegions",
              "Select Region(s): ",
              choices = c(
                "East Asia & Pacific", "Europe & Central Asia", 
                "Latin America & Caribbean", "Middle East & North Africa", 
                "North America", "South Asia", "Sub-Saharan Africa"
              ),
              selected = c("East Asia & Pacific", "Europe & Central Asia", 
                           "Latin America & Caribbean", "Middle East & North Africa", 
                           "North America", "South Asia", "Sub-Saharan Africa")
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
          plotOutput("chosenPlot", height = "1200px", width = "95%")
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
              "Demographic Data (With Region)" = "demog_region",
              "Mapping Data" = "map_data"
            )
          )
        ),
        mainPanel(
          dataTableOutput("chosenTable")
        )
      )
    ),
    tabPanel(
      "Geospatial Map",
      sidebarLayout(
        sidebarPanel(
          selectInput("mapProjection",
                      "Select Projection",
                      choices = c("Map" = "map", "Globe" = "globe"),
                      selected = "map"),
          h3("Trends Overview"),
          p("This map visualizes the average male-to-female ratio of enrollment across countries and regions from 1999 to 2005."),
          p("Countries with no data are shown in gray, while those with available data are color-coded based on the ratio."),
          p("Ratios closer to 1 indicate parity in enrollment between males and females, while values above or below 1 indicate disparities."),
          p("For the enrollment map, countries appearing more white indicate relatively 
            equal enrollment ratios between males and females while blue indicates greater male ratio and red/pink indicate greater female ratio."),
          p("Overall, there seems to be a correlation between the average male-to-female enrollment in education and the average GDP per capita
            from 1999-2005 as countries with very low GDP per capita tend to have a very high male-to-female ratio meaning that there is a much greater
            proportion of boys in education compared to girls. It can also be seen vice versa where high income countries have enrollment ratios close to one."),
          p("Furthermore, what is an even greater observation from the geospatial visualizations is that regions also share a trend. For example,
            Africa and the Middle East tend to have lower GDP per capita and also more unequal gender enrollment compared to other parts of the world
            especially North America and Europe.")
        ),
        mainPanel(
          fluidRow(
            h3("Enrollment Ratio")
          ),
          fluidRow(
            plotlyOutput("enrollmentMap", height = "500px")
          ),
          fluidRow(
            h3("GDP Per Capita")
          ),
          fluidRow(
            plotlyOutput("gdpMap", height = "500px")
          )
        )
      )
    )
  )
)
