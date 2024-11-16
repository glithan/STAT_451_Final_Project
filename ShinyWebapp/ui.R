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
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "plotChoice",
        "Choose a Plot",
        choices = c("GDP" = "gdp", "Growth" = "growth")
      ), conditionalPanel(
        condition = "input.plotChoice === 'growth'",
        selectInput(
          "demoChoice",
          "Group by Region or Gender?",
          choices = c("Region" = "reg", "Gender" = "gender")
        )
      )
    ),
    mainPanel(
      plotOutput("chosenPlot")
    )
  )
  
)