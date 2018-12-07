ui <- navbarPage("Ubergo", id="nav",
                 tabPanel("Driving",
                          div(class="outer",
                              
                              tags$head(
                                      # Include our custom CSS
                                      includeCSS("styles.css"),
                                      includeScript("gomap.js")
                              ),
                              # Map
                              leafletOutput("map", width="100%", height="100%"),
                              
                              # Panel
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = FALSE, top = 100, left = "auto", right = 20, bottom = "auto",
                                            width = 350, height = "auto",
                                            
                                            h1("Explorer"),
                                            
                                            fluidRow(
                                                    
                                                    ## Setting
                                                    column(12,
                                                           textInput("location", "Location", "NYU Wagner"),
                                                           actionButton("go", "Search", class="btn-block",width=300),
                                                           actionButton("navigate", "Navigate", class="btn-block",width=300)
                                                           
                                                           ## Filter
                                                           # selectInput("visualization", "Visualization",
                                                           #             c("Traffic"="traffic1",
                                                           #               "Surge"="surge1",
                                                           #               "Pickup"="pickup1"
                                                           #               )
                                                           # )
                                                           # ,
                                                           # sliderInput("obs", label = h3("Pickup"), min = 0, 
                                                           #             max = 50, value = range(c(20, 40)),step = 1,animate = TRUE),
                                                           # sliderInput("slider_surge_overview", label = h3("Surge"), min = 1, 
                                                           #             max = 2, value = range(c(1.4, 1.9)),step = 0.1),
                                                           # checkboxGroupInput(inputId="InFlags", label=h3("Traffic"),
                                                           #                     choices=setNames(object=c("slow", "medium", "fast"),
                                                           #                                      nm=c("Slow", "Medium", "Fast")),
                                                           #                     selected = c("medium", "fast"),
                                                           #                     inline = TRUE)
                                                    )
                                                    
                                                    ## Chart
                                                    # column(12,
                                                    #        h3("Traffic"),
                                                    #        plotOutput("traffic",height = 200, width=370),
                                                    #        h3("Surge"),
                                                    #        plotOutput("surge", height = 250),
                                                    #        h3("Pickup"),
                                                    #        plotOutput("pickup", height = 250)
                                                    #        )
                                                    
                                            )
                              ),
                              useShinyjs(), 
                              hidden( 
                                      div(id = "conditionalPanel",
                                          fluidRow(absolutePanel(id = "controls", 
                                                                 class = "panel panel-default", 
                                                                 fixed = TRUE,
                                                                 draggable = FALSE, 
                                                                 top = 350, 
                                                                 left = "auto", 
                                                                 right = 20, 
                                                                 bottom = "auto",
                                                                 width = 350, 
                                                                 height = "auto",
                                                                 
                                                                 h1("Filter"),
                                                                 column(12,
                                                                        sliderInput("obs_pickup", label = h3("Pickup"), min = 1, 
                                                                                    max = 50, value = c(2, 40)),
                                                                        sliderInput("obs_surge", label = h3("Surge"), min = 1, 
                                                                                    max = 2.5, value = range(c(1.4, 1.9)),step = 0.1),
                                                                        checkboxGroupInput(inputId="InFlags", label=h3("Traffic"),
                                                                                           choices=setNames(object=c("slow", "medium"),
                                                                                                            nm=c("Slow", "Medium")),
                                                                                           selected = c("slow", "medium"),
                                                                                           inline = TRUE)
                                                                        
                                                                 )
                                                                 # e. of fluidRow(
                                                                 
                                          ) # # e. of absolutePanel
                                          ) # e. of fluidRow
                                      ),
                                      div(id = "conditionalPanel2",
                                          fluidRow(absolutePanel(id = "controls", 
                                                                 class = "panel panel-default", 
                                                                 fixed = TRUE,
                                                                 draggable = FALSE, 
                                                                 top = 350, 
                                                                 left = "auto", 
                                                                 right = 20, 
                                                                 bottom = "auto",
                                                                 width = 350, 
                                                                 height = "auto",
                                                                 
                                                                 h1("Filter"),
                                                                 h2("Visualization"),
                                                                 column(4,actionButton("traffic1", "Traffic", class="btn-block")),
                                                                 column(4,actionButton("surge1", "Surge", class="btn-block")),
                                                                 column(4,actionButton("pickup1", "Pickup", class="btn-block"))
                                                                 # 
                                                                 # column(12,
                                                                 #        selectInput("visualization", "Visualization",
                                                                 #                    c("Traffic"="traffic1",
                                                                 #                      "Surge"="surge1",
                                                                 #                      "Pickup"="pickup1"
                                                                 #                    )
                                                                 #        )
                                                                 #        
                                                                 # )
                                                                 # e. of fluidRow(
                                                                 
                                          ) # # e. of absolutePanel
                                          ) # e. of fluidRow
                                      )
                                      
                              )
                          )
                 ),
                 
                 tabPanel("History",
                          div(class="outer",
                              
                              tags$head(
                                      # Include our custom CSS
                                      includeCSS("styles.css"),
                                      includeScript("gomap.js")
                              ),
                              
                              # Map
                              leafletOutput("history", width="100%", height="100%"),
                              
                              # Panel
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 400, height = "auto",
                                            
                                            # Select Visualization
                                            h2("Visualization"),
                                            column(4,actionButton("AmountofRides3", "Rides", class="btn-block")),
                                            column(4,actionButton("Income1Mile3", "Income", class="btn-block")),
                                            column(4,actionButton("Workingtime3", "Efficiency", class="btn-block")),
                                            hr(),
                                            hr(),
                                            
                                            h2("Detail"),
                                            
                                            fluidRow(
                                                    # column(12,dateInput(inputId = "n_date", label="Select a date", value = "2016-05-13", min = "2016-05-13", max = "2016-10-24",
                                                    #                     format = "dd-mm-yyyy", startview = "month",
                                                    #                     language = "en", width = NULL)),
                                                    
                                                    # Charts
                                                    column(12,
                                                           h3("Monthly Average Pickups"),
                                                           plotOutput("rides3",height = 250, width=370),
                                                           h3("Average Income / Hour"),
                                                           plotOutput("income3",height = 250, width=370),
                                                           h3("Profitable Time / Driving Time"),
                                                           plotOutput("workingtime3", height = 250,width=370))
                                            )
                              )
                          )
                 )
)