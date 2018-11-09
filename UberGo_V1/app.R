if (1==1) {
        
        if(!require("leaflet")){
                install.packages("leaflet")
                library("leaflet")
        }
        
        if(!require("scales")){
                install.packages("scales")
                library("scales")
        }
        
        if(!require("lattice")){
                install.packages("lattice")
                library("lattice")
        }
        
        if(!require("DT")){
                install.packages("DT")
                library("DT")
        }
        
        if(!require("shiny")){
                install.packages("shiny")
                library("shiny")
        }
        
        if(!require("geosphere")){
                install.packages("geosphere")
                library("geosphere")
        }
        
        if(!require("stringr")){
                install.packages("stringr")
                library("stringr")
        }
        
        if(!require("reshape2")){
                install.packages("reshape2")
                library("reshape2")
        }
        
        if(!require("knitr")){
                install.packages("knitr")
                library("knitr")
        }
        
        if(!require("leaflet")){
                install.packages("leaflet")
                library("leaflet")
        }
        
        if(!require("rvest")){
                install.packages("rvest")
                library("rvest")
        }
        
        if(!require("sp")){
                install.packages("sp")
                library("sp")
        }
        
        if(!require("ggplot2")){
                install.packages("ggplot2")
                library("ggplot2")
        }
        
        
        if(!require("reshape2")){
                install.packages("reshape2")
                library("reshape2")
        }
        
        if(!require("knitr")){
                install.packages("knitr")
                library("knitr")
        }
        
        if(!require("plyr")){
                install.packages("plyr")
                library("plyr")
        }
        
        if(!require("dplyr")){
                install.packages("dplyr")
                library("dplyr")
        }
        
        if(!require("ggmap")){
                install.packages("ggmap")
                library("ggmap")
        }
        
        if(!require("curl")){
                install.packages("curl")
                library("curl")
        }
        
        if(!require("RJSONIO")){
                install.packages("RJSONIO")
                library("RJSONIO")
        }
        
        if(!require("RCurl")){
                install.packages("RCurl")
                library("RCurl")
        }
        if(!require("mapsapi")){
                library("devtools")
                devtools::install_github("michaeldorman/mapsapi")
                library("mapsapi")
        }
        
        if(!require("placement")){
                library("devtools")
                install_github("DerekYves/placement")
                library("placement")
        }
        
        if(!require("htmlwidgets")){
                install.packages("htmlwidgets")
                library("htmlwidgets")
        }
        library(xml2)
}


ui <- navbarPage("Ubergo", id="nav",
                 
                 tabPanel("Navigation map",
                          div(class="outer",
                              
                              tags$head(
                                      # Include our custom CSS
                                      includeCSS("styles.css"),
                                      includeScript("gomap.js")
                              ),
                              hr(),
                              hr(),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 800, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            # Read Input
                                            h2("Space Explorer"),
                                            
                                            fluidRow(
                                                    column(12,
                                                           textInput("Address", "", "Amli Wallingford"),
                                                           verbatimTextOutput("value"),
                                                           actionButton("button", "Explore Space", class="btn-block")
                                                    )
                                            )
                                            
                              )
                          )
                 )
                 
#                  ,tabPanel("Grading",
#                           hr(),
#                           DT::dataTableOutput("grading")
#                  ),
#                  
#                  
#                  conditionalPanel("false", icon("crosshair"))
)



server <- function(input, output) {
        
        output$map <- renderLeaflet({
                # Address <-"Amli Wallingford"
                input$button
                
                mykey1 <- "AIzaSyCfhW0pPKbXBmrrRUYRoN4uyZlm4UNe_uo"
                doc = mp_directions(
                        origin = input$Address,
                        destination = "University of Washington",
                        alternatives = TRUE,
                        key = mykey1
                )
                # doc
                
                
                # doc = as_xml_document(response_directions_driving)
                r = mp_get_routes(doc)
                # r
                
                # pal = colorFactor(palette = "Dark2", domain = r$alternative_id)
                # 
                # leaflet() %>% 
                #         addProviderTiles("CartoDB.DarkMatter") %>%
                #         addPolylines(data = r, opacity = 1, weight = 7, color = ~pal(alternative_id))
                seg = mp_get_segments(doc)
                
                pal = colorFactor(
                        palette = sample(colors(), length(unique(seg$segment_id))), 
                        domain = seg$segment_id
                )
                leaflet(seg) %>% 
                        addProviderTiles("CartoDB.DarkMatter") %>%
                        addPolylines(opacity = 1, weight = 7, color = ~pal(segment_id), popup = ~instructions)
                
                        
        })
}



# Run the application 
shinyApp(ui = ui, server = server)
