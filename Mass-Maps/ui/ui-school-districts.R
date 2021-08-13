library(shiny)
library(shinydashboard)
library(leaflet)

school_districts_ui <- tabItem(
  tabName = "school_districts",
  
  fluidRow(
    box(
      title = "School Districts",
      leafletOutput(outputId = "school_districts_map"),
      width = 9,
    ),
    
    box(
      title = "Map options:",
      width = 3,
      radioButtons(
        "selectedRaceSchools",
        label = "Race:",
        choices = c(
          "Asian" = "Asian",
          "Black" = "Black",
          "Hispanic" = "Hispanic",
          "Native" = "Native",
          "White" = "White"
        )
      )
    )
  ),
  
  fluidRow(
    box(
      title = "Filter options:",
      width = 3,
      
      sliderInput(
        "asianRangeSchools",
        label = "% asian:",
        min = 0,
        max = 31,
        value = c(0, 31),
        step = 1
      ),
      
      sliderInput(
        "blackRangeSchools",
        label = "% black:",
        min = 0,
        max = 45,
        value = c(0, 45),
        step = 1
      ),
      
      sliderInput(
        "hispanicRangeSchools",
        label = "% hispanic:",
        min = 0,
        max = 81,
        value = c(0, 81),
        step = 1
      ),
      
      sliderInput(
        "nativeRangeSchools",
        label = "% native:",
        min = 0,
        max = 23,
        value = c(0, 23),
        step = 1
      ),
      
      sliderInput(
        "whiteRangeSchools",
        label = "% white:",
        min = 14,
        max = 100,
        value = c(14, 100),
        step = 1
      )
      
    ),
    
    box(
      title = "Data table",
      dataTableOutput(outputId = "school_districts_table"),
      width = 9
    )
    
  )
)
