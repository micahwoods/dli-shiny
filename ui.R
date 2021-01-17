# ui


library(shiny)
library(leaflet)
library(ggplot2)
library(nasapower)

ui <- (fluidPage( tabsetPanel(
  tabPanel("Get DLI data",
  
#  tags$head(tags$script("head.html")),
  
  h2("This app shows recent DLI at any location"),
  
  tags$script(HTML(
    "document.body.style.backgroundColor = '#f7ffed';"
  )),
  
  tags$head(
    tags$style(HTML('#ab{background-color:#3f7300}'))
  ),
  
  
  p("You can select a location on the map to highlight the encompassing 0.5° x 0.5° grid. Global solar irradiance data are then obtained from the NASA POWER agroclimatology dataset and are converted to DLI. Is there a place you'd like to check?"),
  
  tags$h4("Step 1"),
  
  p("Click the location on the map at which you'd like to find DLI data. A yellow rectangle will appear over the 0.5° latitude & longitude boundaries encolosing that point."),
  
  tags$script('
              $(document).ready(function () {
              navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
              function onError (err) {
              Shiny.onInputChange("geolocation", false);
              }
              
              function onSuccess (position) {
              setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
              }, 1100)
              }
              });
              '),
  
  leafletOutput("map"),

tags$h4("Step 2"),

textOutput("text_Step2"),

br(),

p("You will see a yellow rectangle over that point, marking the 0.5° x 0.5° grid from which data will be returned. Press the button below to get the DLI for that location for the past year."),

actionButton(inputId = "get_nasapower_data", label = "Get the DLI data", class = "btn-primary btn-lg"),

br(),

p("It takes a few seconds to get these data. You'll see a summary chart soon."),

br(),

plotOutput("dliChart"),

# tags$h4("Step 3"),
# 
# p("Would you like to download this chart?"),
# 
# # download
# downloadButton("download_chart", "Download chart"),
# 
# tags$h4("Step 4"),
# 
# p("You may now download this chart. View it here. Or download. It has 3 panes by patchwork. Daily with a smooth and weekly bars. Includes the lat lon. Includes ATC logo."),

# tags$h4("Step 3"),
# 
# p(""),

hr(),

a(href = "https://www.asianturfgrass.com", 
          img(src = "atc.png", height = 85))
  
  
  ),

tabPanel("Technical details",  includeMarkdown("dli_details.md"))

)
  
  
    )
  )

