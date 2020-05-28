---
output: html_document
runtime: shiny
---

library(shiny)
library(leaflet)
library(dplyr)
library(mapview)
library(ggplot2)
library(data.table)

workplacesNY <- read.csv("csv_results/workplaces_NY_2019.csv")
workplacesJC <- read.csv("csv_results/workplaces_JC_2019.csv")
workplaces <- rbind(workplacesJC,workplacesNY)

spendingFreeTimeNY <- read.csv("csv_results/spendingFreeTime_NY_2019.csv")
spendingFreeTimeJC <- read.csv("csv_results/spendingFreeTime_JC_2019.csv")
spendingFreeTime <- rbind(spendingFreeTimeJC,spendingFreeTimeNY)

hiringByAgeNY <- read.csv("csv_results/hiringByAge_NY_2019.csv")
hiringByAgeJC <- read.csv("csv_results/hiringByAge_JC_2019.csv")
hiringByAge <- rbind(hiringByAgeJC,hiringByAgeNY)

bikeRankingJC <- as.data.table(read.csv("csv_results/bikeRanking_JC.csv"))
bikeRankingNY <- as.data.table(read.csv("csv_results/bikeRanking_NYC_2019.csv"))
bikeRanking <- rbind(bikeRankingJC,bikeRankingNY) %>% arrange(desc(SumOfRentedTime))

bestTimeToRideJC <- as.data.table(read.csv("csv_results/BestTimeToRider_JC.csv"))
bestTimeToRideNY <- as.data.table(read.csv("csv_results/BestTimeToRider_NYC_2019.csv"))
bestTimeToRide <- rbind(bestTimeToRideJC,bestTimeToRideNY)

bestTimeToRideLongJC <- as.data.table(read.csv("csv_results/bestTimeToRideLong_JC.csv"))
bestTimeToRideLongNY <- as.data.table(read.csv("csv_results/bestTimeToRideLong_NYC_2019.csv"))
bestTimeToRideLong <- rbind(bestTimeToRideLongJC,bestTimeToRideLongNY)

ridersByAgeAndGenderJC <- as.data.table(read.csv("csv_results/RidersByAgeAndGender_JC.csv"))
ridersByAgeAndGenderNY <- as.data.table(read.csv("csv_results/RidersByAgeAndGender_NYC_2019.csv"))
ridersByAgeAndGender <- rbind(ridersByAgeAndGenderJC,ridersByAgeAndGenderNY)

mostCommonStopPerStationJC <- as.data.table(read.csv("csv_results/MostCommonStopPerStation_JC.csv"))
mostCommonStopPerStationNY <- as.data.table(read.csv("csv_results/MostCommonStopPerStation_NYC_2019.csv"))
mostCommonStopPerStation <- rbind(mostCommonStopPerStationJC,mostCommonStopPerStationNY)


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
    tabPanel("Hires by age",
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
             )),
    tabPanel("Most Common Destination",
             sidebarLayout(
                 sidebarPanel(
                     p("Here you can see what destination was most common for riders starting on choosen station and time of the day (afternoon(12-23) or morning(0-11)). You can see how some of the destination were more common"),
                     selectInput("stationName",
                                 "Station Name:",
                                 unique(mostCommonStopPerStation$start.station.name)),
                     selectInput("timeOfDay",
                                 "Station Name:",
                                 unique(mostCommonStopPerStation$DayTime))
                 ),
                 mainPanel(
                     leafletOutput("mostCommonDestinationLeaflet")

                 )
             )),
    tabPanel("When people most ride bikes",
             sidebarLayout(
                 sidebarPanel(
                     p("This plot shows at what time of the day people are riding bikes"),
                     selectInput("weekday",
                                 "Day of the week",
                                 list('Monday' = 1, 'Tuesday' = 2,'Wednesday' = 3,'Thursday' = 4,'Friday' = 5,'Saturday' = 6,'Sunday' = 7))
                 ),
                 mainPanel(
                     plotOutput("timetoRidePlot"),
                     plotOutput("timetoRideLongPlot"),
                 )
             )),
    tabPanel("What age and gender rides the most",
             sidebarLayout(
                 sidebarPanel(
                     p("This plot shows at what age group and gender rides the most"),
                     selectInput("typeOfStat",
                                 "Type of shown statistic",
                                 list('Max', 'Mean' , 'Median' ,'Count' ,'90th Percentile' = 'percentile90th'))
                 ),
                 mainPanel(
                     plotOutput("ageAndGender"),
                     dataTableOutput("ageAndGenderTable")
                 )
             )),
    tabPanel("Bonus - bikes ranking top 10",
             sidebarLayout(
                 sidebarPanel(
                     p("Bikes Ranking"),
                     selectInput("rankbyT",
                                 "Type of ranked statistic",
                                 list('Average time Rented' = 'AverageRentTime', 'Times Rented' = 'CountOfRenting', 'Sum of rented time' = 'SumOfRentedTime'))
                 ),
                 mainPanel(
                     dataTableOutput("bikeRanking"),
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
    
    mostCommonEndsColorpal <- reactive({
        colorNumeric(palette = colorRamp(c("#0052D4","#65C7F7","#9CECFB"), interpolate = "spline"),
                     domain = as.data.frame(mostCommonStopPerStation[start.station.name == input$stationName & DayTime == input$timeOfDay ,.(Count)])[['Count']])
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
    
    output$mostCommonDestinationLeaflet <- renderLeaflet({
        leaflet(mostCommonStopPerStation) %>%
            setView(lat = 40.730610, lng = -73.935242, zoom = 11) %>%
            addProviderTiles(providers$CartoDB.DarkMatter)
    })
    
    output$timetoRidePlot <- renderPlot({ coef <- 0.01
        ggplot(bestTimeToRideNY[weekday == input$weekday,],aes(x=DayHour))+
            geom_line(aes(y = Count,color = "Number Of Riders"))+ 
            geom_line(aes(y = Mean/coef,color = "Average trip duration"))+ 
            scale_y_continuous(
                
                # Features of the first axis
                name = "Number Of Riders",
                
                # Add a second axis and specify its features
                sec.axis = sec_axis(~.*coef, name="Average trip duration")
            ) + ggtitle("Riders and average ride duration per hour") +xlab("Hour") })
    
    output$timetoRideLongPlot <- renderPlot({ coef <- 10
        ggplot(bestTimeToRideLongNY[weekday == input$weekday,],aes(x=DayHour))+
        geom_line(aes(y = Count,color = "Number Of Riders"))+ 
        geom_line(aes(y = Mean/coef,color = "Average trip duration"))+ 
        scale_y_continuous(
            
            # Features of the first axis
            name = "Number Of Riders",
            
            # Add a second axis and specify its features
            sec.axis = sec_axis(~.*coef, name="Average trip duration")
        ) + ggtitle("Riders and average ride duration for longest rides (longer than 98% of rides)" ) +xlab("Hour")
     })
    
    output$ageAndGender <- renderPlot({
        ggplot(ridersByAgeAndGenderNY,aes(x=AgeGroup, y = ridersByAgeAndGenderNY[[input$typeOfStat]], fill = factor(gender, labels = c("male", "female") ))) +
        geom_bar(position="dodge", stat="identity") +
        ggtitle("Statistics by age Group and gender" ) +
        xlab("Age Group") + ylab(input$typeOfStat) + labs(fill='Gender')
    })
    
    output$ageAndGenderTable <- renderDataTable(ridersByAgeAndGenderNY)
    
    output$bikeRanking <- renderDataTable({
        setorderv(bikeRanking,input$rankbyT,order = -1L)
       head(bikeRanking,10L)
        
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
    
    observe({
        pal = mostCommonEndsColorpal()
        leafletProxy("mostCommonDestinationLeaflet", data = mostCommonStopPerStation[start.station.name == input$stationName & DayTime == input$timeOfDay,]) %>%
            clearShapes() %>% clearControls() %>%
            addCircles(lat = ~ end.station.latitude, lng = ~ end.Station.Longitude,
                       weight = 1, radius = ~sqrt(as.data.frame(mostCommonStopPerStation[start.station.name == input$stationName & DayTime == input$timeOfDay ,.(Count)])[['Count']]), label = ~end.station.name,
                       color = pal(as.data.frame(mostCommonStopPerStation[start.station.name == input$stationName & DayTime == input$timeOfDay ,.(Count)])[['Count']]),
                       fillOpacity = 1.0) %>%
            addLegend("bottomright", pal = pal, values = ~as.data.frame(mostCommonStopPerStation[start.station.name == input$stationName & DayTime == input$timeOfDay ,.(Count)])[['Count']],
                      opacity = 1, title="Number of ended rides") %>%
            addCircles(lat = mostCommonStopPerStation[start.station.name == input$stationName ,.(start.station.latitude)][[1]],
                       lng = mostCommonStopPerStation[start.station.name == input$stationName ,.(start.station.longitude)][[1]],
                       color = 'red',fillColor = 'red',weight = 1, radius = 10)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
