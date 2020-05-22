---
output: html_document
runtime: shiny
---

library(shiny)
library(leaflet)
library(dplyr)

workplacesNY <- read.csv("csv_results/workplaces_NY_2019.csv")
workplacesJC <- read.csv("csv_results/workplaces_JC_2019.csv")
workplaces <- rbind(workplacesJC,workplacesNY)
spendingFreeTimeNY <- read.csv("csv_results/spendingFreeTime_NY_2019.csv")
spendingFreeTimeJC <- read.csv("csv_results/spendingFreeTime_JC_2019.csv")
spendingFreeTime <- rbind(spendingFreeTimeJC,spendingFreeTimeNY)
hiringByAgeNY <- read.csv("csv_results/hiringByAge_NY_2019.csv")
hiringByAgeJC <- read.csv("csv_results/hiringByAge_JC_2019.csv")
hiringByAge <- rbind(hiringByAgeJC,hiringByAgeNY)

ui <- navbarPage("BikeAnalyzer",
    tabPanel("Workplaces",
             sidebarLayout(
                 sidebarPanel(
                     p("See where people mostly leave bikes in working day morning, so we can evaluate where the biggest workplaces are."),
                     selectInput("hires",
                                 "Who:",
                                 list(`All` = "totalHires", `Men` = "manHires", `Women` = "womanHires"))
                 ),
                 mainPanel(
                     leafletOutput("workplacesLeaflet")
                 )
             )),
    tabPanel("Spending free time",
             sidebarLayout(
                 sidebarPanel(
                     p("Check where people mostly leave bikes before evening at the weekend. That can be helpful to presume, what are most common places to spend free time.")
                 ),
                 mainPanel(
                     leafletOutput("spendingFreeTimeLeaflet")
                 )
             )),
    tabPanel("Hiring by age",
             sidebarLayout(
                 sidebarPanel(
                     p("See where people in different age mostly take bikes in the morning, so we can guess typical living location for each age category."),
                     selectInput("ageCategory",
                                 "Age range:",
                                 list(`<20` = "under20", `20-39` = "under40", `40-59` = "under60", `60-79` = "under80"))
                 ),
                 mainPanel(
                     leafletOutput("hiringByAgeLeaflet")
                 )
             ))
    )

server <- function(input, output) {
    
    # palettes
    
    workPlacesColorpal <- reactive({
        colorNumeric(palette = colorRamp(c("#0052D4","#65C7F7","#9CECFB"), interpolate = "spline"),
                     domain = workplaces[[input$hires]])
    })
    
    hiringByAgeColorpal <- reactive({
        colorNumeric(palette = colorRamp(c("#0052D4","#65C7F7","#9CECFB"), interpolate = "spline"),
                     domain = hiringByAge[[input$ageCategory]])
    })
    
    # outputs

    output$workplacesLeaflet <- renderLeaflet({
        leaflet(workplaces) %>%
            setView(lat = 40.730610, lng = -73.935242, zoom = 11) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    })
    
    output$spendingFreeTimeLeaflet <- renderLeaflet({
        pal = colorNumeric(palette = colorRamp(c("#0052D4","#65C7F7","#9CECFB"), interpolate = "spline"),
                           domain = spendingFreeTime$hires)
        leaflet(spendingFreeTime) %>%
            setView(lat = 40.730610, lng = -73.935242, zoom = 11) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
            addCircles(lat = ~ latitude, lng = ~ longitude,
                       weight = 1, radius = ~sqrt(hires), label = ~stationName,
                       color = pal(spendingFreeTime$hires), fillOpacity = 1.0) %>%
            addLegend("bottomright", pal = pal, values = ~hires, 
                      opacity = 1, title="Number of returned bikes")
    })
    
    output$hiringByAgeLeaflet <- renderLeaflet({
        leaflet(hiringByAge) %>%
            setView(lat = 40.730610, lng = -73.935242, zoom = 11) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    })
    
    # observers
    
    observe({
        pal = workPlacesColorpal()
        leafletProxy("workplacesLeaflet", data = workplaces) %>%
            clearShapes() %>% clearControls() %>%
            addCircles(lat = ~ latitude, lng = ~ longitude,
                       weight = 1, radius = ~sqrt(workplaces[[input$hires]]), label = ~stationName,
                       color = pal(workplaces[[input$hires]]), fillOpacity = 1.0) %>%
            addLegend("bottomright", pal = pal, values = ~workplaces[[input$hires]],
                      opacity = 1, title="Number of returned bikes")
    })
    
    observe({
        pal = hiringByAgeColorpal()
        leafletProxy("hiringByAgeLeaflet", data = hiringByAge) %>%
            clearShapes() %>% clearControls() %>%
            addCircles(lat = ~ latitude, lng = ~ longitude,
                       weight = 1, radius = ~sqrt(hiringByAge[[input$ageCategory]]), label = ~stationName,
                       color = pal(hiringByAge[[input$ageCategory]]), fillOpacity = 1.0) %>%
            addLegend("bottomright", pal = pal, values = ~hiringByAge[[input$ageCategory]],
                      opacity = 1, title="Number of hired bikes")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
