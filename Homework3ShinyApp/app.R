---
output: html_document
runtime: shiny
---

library(shiny)
library(leaflet)
library(dplyr)

workplacesNY <- read.csv("csv_results/workplaces_NY_2019.csv")
# workplacesJC <- read.csv("csv_results/workplaces_JC_2019.csv")
# spendingFreeTimeNY <- read.csv("csv_results/spendingFreeTime_NY_2019.csv")
# spendingFreeTimeJC <- read.csv("csv_results/spendingFreeTime_JC_2019.csv")
# hiringByAgeNY <- read.csv("csv_results/hiringByAge_NY_2019.csv")
# hiringByAgeJC <- read.csv("csv_results/hiringByAge_JC_2019.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Project"),
    sidebarLayout(
        sidebarPanel(
            selectInput("hires",
                        "Who:",
                        list(`All` = "totalHires", `Men` = "manHires", `Women` = "womanHires"))
        ),
        mainPanel(
           leafletOutput("workspacesNYLeaflet")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    colorpal <- reactive({
        colorNumeric(palette = colorRamp(c("#0052D4","#65C7F7","#9CECFB"), interpolate = "spline"),
                     domain = workplacesNY[[input$hires]])
    })

    output$workspacesNYLeaflet <- renderLeaflet({
        pal = colorpal()
        leaflet(workplacesNY) %>%
            setView(lat = 40.730610, lng = -73.935242, zoom = 11) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
            # addCircles(lat = ~ latitude, lng = ~ longitude,
            #            weight = 1, radius = ~sqrt(totalHires), label = ~stationName,
            #            color = ~ pal(workplacesNY[[input$hires]]), fillOpacity = 1.0)
    })
    
    observe({
        pal = colorpal()
        leafletProxy("workspacesNYLeaflet", data = workplacesNY) %>%
            addCircles(lat = ~ latitude, lng = ~ longitude,
                       weight = 1, radius = ~sqrt(totalHires), label = ~stationName,
                       color = pal(workplacesNY[[input$hires]]), fillOpacity = 1.0)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
