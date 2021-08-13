library(shiny)
library(shinydashboard)
library(leaflet)

cities_and_towns_ui <- tabItem(
  tabName = "cities_and_towns",
  
  fluidRow(
    box(
      title = "Cities and Towns",
      leafletOutput(outputId = "ma_map"),
      width = 9,
    ),
    
    box(
      title = "Map options:",
      width = 3,
      radioButtons(
        "selectedRace",
        label = "Race:",
        choices = c(
          "Asian" = "Asian",
          "Black" = "Black",
          "Hispanic" = "Hispanic",
          "Native" = "Native",
          "White" = "White"
        ),
        selected = "Asian"
      )
    )
  ),
  
  fluidRow(
    box(
      title = "Filter options:",
      width = 3,
      
      sliderInput(
        "asianRange",
        label = "% asian:",
        min = 0,
        max = 31,
        value = c(0, 31),
        step = 1
      ),
      
      sliderInput(
        "blackRange",
        label = "% black:",
        min = 0,
        max = 45,
        value = c(0, 45),
        step = 1
      ),
      
      sliderInput(
        "hispanicRange",
        label = "% hispanic:",
        min = 0,
        max = 81,
        value = c(0, 81),
        step = 1
      ),
      
      sliderInput(
        "nativeRange",
        label = "% native:",
        min = 0,
        max = 23,
        value = c(0, 23),
        step = 1
      ),
      
      sliderInput(
        "whiteRange",
        label = "% white:",
        min = 14,
        max = 100,
        value = c(14, 100),
        step = 1
      )
      
    ),
    
    box(
      title = "Data table",
      dataTableOutput(outputId = "ma_table"),
      width = 9
    )
    
  )
)
