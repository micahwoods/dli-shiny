# server

library(shiny)
library(cowplot)
library(leaflet.esri)
library(plyr)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(patchwork)

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap,
                       options = providerTileOptions(noWrap = FALSE),
                       group = "Open Street Map (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "ESRI Topo") %>%
      addLayersControl(
        baseGroups = c("Open Street Map (default)", "ESRI Imagery", "ESRI Topo"),
    #    overlayGroups = c("Outline"),
        options = layersControlOptions(collapsed = FALSE)) %>%
    #  addTiles() %>%
    #  addResetMapButton() %>%
      addSearchOSM() %>%
        # apikey = Sys.getenv("GOOGLE_MAP_GEOCODING_KEY")) %>%
      setView(lng = 100, lat= 13.44, zoom = 6)
  })
  
  # Zoom in on user location if given
  observe({
    if(!is.null(input$lat)){
      map <- leafletProxy("map")
      dist <- 0.5
      lat <- input$lat
      lng <- input$long
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    }
  })
  
  # find lat lon of user selected point on the map
  # and place a transparent rectangle over it with 0.5 degree lat lon boundaries
  observeEvent(input$map_click, {
      click <- input$map_click
    
      proxy <- leafletProxy("map")
    
    proxy %>% clearPopups() %>%
      clearShapes() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "ESRI Topo") %>%
      
      # add here the half-degree boundary box overlay
      addRectangles(lng1 = round_any(click$lng, 0.5, floor),
                    lng2 = round_any(click$lng, 0.5, ceiling),
                    lat1 = round_any(click$lat, 0.5, floor),
                    lat2 = round_any(click$lat, 0.5, ceiling),
                    fill = TRUE, fillColor = "yellow", fillOpacity = 0.4,
                    weight = 2, color = "yellow", group = "Outline") %>%
    
      #  addPopups(click$lng, click$lat, group = "Popup") %>%
      addLayersControl(
        baseGroups = c("Open Street Map (default)", "ESRI Imagery", "ESRI Topo"),
        overlayGroups = c("Outline"),
        options = layersControlOptions(collapsed = FALSE) 
      )
    
    # label as north or south or east or west for chart output
    north_south <- ifelse(click$lat > 0, "N", "S")
    east_west <- ifelse(click$lng > 0, "E", "W")
    
    output$text_Step2 <- renderText(paste("You have selected a point at ",
                                          formatC(abs(click$lat), digits = 1, format = "f"),  "° ", north_south, 
                                          " & ", 
                                          formatC(abs(click$lng), digits = 1, format = "f"), "° ", east_west, sep = ""))
    
  })
  
 observeEvent(input$get_nasapower_data, {
  output$dliChart <- renderPlot({
  
 isolate( click <- input$map_click )
  
  # label as north or south or east or west for chart output
  north_south <- ifelse(click$lat > 0, "N", "S")
  east_west <- ifelse(click$lng > 0, "E", "W")
    
  loc_power <- get_power(community = "AG", 
                         lonlat = c(click$lng, click$lat),
                         pars = "ALLSKY_SFC_SW_DWN",
                         temporal_average = "DAILY",
                         dates = c(today() - days(369), today()))
  
  loc_power$Rs <- na.approx(loc_power$ALLSKY_SFC_SW_DWN, na.rm = FALSE)
  
  loc_power$dli <- loc_power$Rs * 2.04
  
  d3 <- subset(loc_power, YYYYMMDD < (today() - days(5)))
  
  # now try to do something where I calculate the mean by week, have start and end days on the week
  # and can then plot as flat lines perhaps colour-coded
  
  d3$wday <- wday(d3$YYYYMMDD, label = TRUE)
  
  d3$date <- d3$YYYYMMDD 
  
  d3$weekCount <- rep(1:52, each = 7)
  
  dli_weekly <- d3 %>%
    group_by(weekCount) %>%
    summarise(avg = mean(dli, na.rm = TRUE),
              start = min(date),
              end = max(date))
  
  dli_weekly2 <- pivot_longer(dli_weekly, cols = start:end, values_to = "date")
  
  dli_weekly2$level3040 <- ifelse(dli_weekly2$avg >= 40, "cd",
                                  ifelse(dli_weekly2$avg < 40 & dli_weekly2$avg >= 30, "pv",
                                         "zm"))
  
  d3$dliColour <- ifelse(d3$dli >= 40, "cd",
                         ifelse(d3$dli < 40 & d3$dli >= 30, "pv",
                                "zm"))
  
  # plot dli
  p <- ggplot(data = d3, aes(x = date, y = dli))
  dli <- p + theme_cowplot(font_family = "Roboto Condensed") +
    background_grid() +
    # geom_smooth(se = FALSE, colour = "grey",
    #            span = 0.2) +
    #  geom_smooth(se = FALSE, colour = "#7570b3", size = 0.5, span = 0.25) +
    geom_line(data = dli_weekly2,
              aes(x = date, y = avg, 
                  group = weekCount, colour = level3040),
              size = 1) +
    scale_y_continuous(limits = c(0, NA),
                       breaks = seq(0, 60, 10)) +
    scale_color_brewer(palette = "Dark2") +
    geom_point(aes(colour = dliColour), alpha = 0.3, size = 1) +
    scale_x_date(date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          axis.title.x = element_blank(),
          plot.caption = element_text(size = 8),
          legend.position = "none",
          plot.background = element_rect(fill = "#f7ffed", color = NA)) +
    labs(y = expression(paste("DLI, mol ", m^{-2}, " ", d^{-1})),
         title = "Daily light integral (DLI)",
         subtitle = paste("for the past 52 weeks at ", 
                          formatC(abs(click$lat), digits = 1, format = "f"),  "° ", north_south, " & ", 
                            formatC(abs(click$lng), digits = 1, format = "f"), "° ", east_west, sep = ""),
         caption = "These data were obtained from the NASA Langley Research Center POWER Project funded\nthrough the NASA Earth Science Directorate Applied Science Program: power.larc.nasa.gov\nusing the 'nasapower' R package by Adam Sparks") 
  # #  annotate("label", x = as.Date("2019-12-05"), hjust = 0, y = 50,
  #            colour = "#1b9e77", family = "Roboto Condensed", size = 3.5,
  #            label = "Weekly average DLI ≥ 40") +
  #   annotate("label", x = as.Date("2019-12-05"), hjust = 0, y = 18,
  #            colour = "#d95f02", family = "Roboto Condensed", size = 3.5,
  #            label = "Weekly average DLI < 40") +
  #   annotate("segment", x = as.Date('2020-01-29'), xend = as.Date('2020-02-11'), 
  #            y = 48, yend = 42.5, colour = "#1b9e77",  size = 0.5, arrow=arrow(type = 'closed',
  #                                                                              length = unit(0.3, 'cm'))) +
  #   annotate("segment", x = as.Date('2019-12-15'), xend = as.Date('2019-12-25'), 
  #            y = 20, yend = 35, colour = "#d95f02", size = 0.5, arrow=arrow(type = 'closed',
  #                                                                           length = unit(0.3, 'cm'))) +
  #   annotate("label", x = as.Date("2020-05-15"), hjust = 0, y = 5,
  #            colour = "#7570b3", family = "Roboto Condensed", size = 3.5,
  #            label = "DLI on a single day") +
  #   annotate("segment", x = as.Date('2020-05-20'), xend = as.Date('2020-04-14'), 
  #            y = 7, yend = 24.5, colour = "#7570b3", size = 0.5, arrow=arrow(type = 'closed',
  #                                                                            length = unit(0.3, 'cm'))) +
  #   annotate("segment", x = as.Date('2020-07-04'), xend = as.Date('2020-08-05'), 
  #            y = 7, yend = 15.5, colour = "#7570b3", size = 0.5, arrow=arrow(type = 'closed',
  #                                                                            length = unit(0.3, 'cm'))) 
  
  d3$monthCenter <- ymd(paste(d3$YEAR, d3$MM, 15))
  
  dli_monthly <- d3 %>%
    group_by(monthCenter) %>%
    summarise(avg = mean(dli, na.rm = TRUE),
              min = min(dli, na.rm = TRUE),
              max = max(dli, na.rm = TRUE))
  
  p2 <- ggplot(data = dli_monthly, aes(x = monthCenter, y = avg))
  monthPlot <- p2 + theme_cowplot(font_family = "Roboto Condensed") +
    background_grid() +
    geom_line(colour = "grey") +
    geom_point(alpha = 0.3, size = 1) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          axis.title.x = element_blank(),
          plot.caption = element_text(size = 8),
          legend.position = "none",
          plot.background = element_rect(fill = "#f7ffed", color = NA)) +
    labs(y = expression(paste("Mean DLI, mol ", m^{-2}, " ", d^{-1})),
         title = "Average DLI by month (DLI)",
         subtitle = paste("for the past 52 weeks at ", 
                          formatC(abs(click$lat), digits = 1, format = "f"),  "° ", north_south, " & ", 
                          formatC(abs(click$lng), digits = 1, format = "f"), "° ", east_west, sep = "")) +
    scale_y_continuous(limits = c(0, NA),
                       breaks = seq(0, 70, 10))
  
  
  # 
  # p3 <- ggplot(data = d3)
  # specialPlot <- p3 + theme_void() +
  #   theme(plot.background = element_rect(fill = "#f7ffed", color = NA)) +
  #   scale_y_continuous(limits = c(0, 100)) +
  #   scale_x_continuous(limits = c(0, 100)) +
  #   annotate("text", x = 5, hjust = 0, y = 50,
  #            family = "Roboto Condensed",
  #            label = paste("The highest DLI was", formatC(max(d3$dli, na.rm = TRUE), digits = 1, format = "f"),
  #                          "on", d3$wday, ",", d3$date, sep = ","))
  
  combinedPlot <- dli + monthPlot +
    plot_annotation(theme = theme(plot.background = element_rect(fill = "#f7ffed", color = NA))) +
    plot_layout(nrow = 1, widths = c(3, 1))
  
  combinedPlot
  
  })
  
 })


      }