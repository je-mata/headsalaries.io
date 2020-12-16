#If interested in replicating the app, you can get & work the data using the
#"getting_cleaning_wikipedia_data_for_app.Rmd" in this repository
#
# Had a lot of help on using leaflet in Shiny from:
# https://rstudio.github.io/leaflet/shiny.html
#
# http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(readr)
library(geojsonio)
library(RColorBrewer)
library(sp)


#Data
#1. Get salaries data file and geojson file
Data <- read_csv("data.csv")
countries <- geojson_read("./json/simplecountries.geojson", what = "sp")

#Bins for color in polygons and for slider stops
bins <- c(0, 16639, 33809, 56529, 74594, 104999, 194299, 809672, Inf)
sliderStops <- c(0, 16639, 33809, 56529, 74594, 104999, 194299, 809672, max(Data$head_of_state_salary_usd, na.rm = TRUE))

#Set option to avoid scientific notation in pop-ups (don't want e+00 format)
options(scipen=999)


# Define UI for application full page map with Panel on top right corner
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10,
                  h3("Anual salary of heads of state in US dollars"),
                  sliderTextInput("range", "Salary", sliderStops, 
                        selected = c(0, max(Data$head_of_state_salary_usd, na.rm = TRUE))
                            ),
                  h4("Slide to visualize countries in desired salary range"),
                  h4("Press on countries to view details")
    )
)


server <- function(input, output, session) {
    
    # Reactive expression creates a merge of the "countries" spacial polygons dataframe 
    # and the subset of "Data" selected
    filteredData <- reactive({sp::merge(
        countries,
        Data[Data$head_of_state_salary_usd >= input$range[1] 
                  & Data$head_of_state_salary_usd <= input$range[2],], 
        by = "iso3")
    })

    
    output$map <- renderLeaflet({
        # Use leaflet() here, only including aspects of the map that
        # won't change dynamically 
        leaflet(countries) %>% addTiles()%>% setView(40.433333, 3.7, zoom = 2)
    })
    
    # Incremental changes to the map (in this case, adding the polygons) are performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        pal <- colorBin("YlOrRd", domain = filteredData(), bins = bins)
        leafletProxy("map", data = filteredData()) %>%
            addPolygons(fillColor = ~pal(head_of_state_salary_usd),
                        weight = .5,
                        opacity = 1,
                        color = "white",
                        fillOpacity = 0.7,
                        popup = paste(filteredData()$state, filteredData()$post, 
                                      prettyNum(filteredData()$head_of_state_salary_usd, 
                                      big.mark = ",", drop0trailing = FALSE), 
                                      sep = " - ")
                        )
    })

}


# Run the application 
shinyApp(ui = ui, server = server)
