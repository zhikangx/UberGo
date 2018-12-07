server <- function(input, output,session) {
        
        ########################################### Tab of Driving ########################################
        # Get location of the click on map
        observeEvent(input$map_click, {
                ## Get the click info like had been doing
                click <- input$map_click
                clat <- click$lat
                clng <- click$lng
                # print(clat)
                # print(clng)
                # clng <- 40.72475
                # clat <- -73.9956
                address_geo <- as.character(paste(clat,clng))
                doc = mp_geocode(addresses =address_geo, key=mykey1)
                pnt = mp_get_points(doc)
                
                leafletProxy("map") %>% 
                        addMarkers(data = pnt,layerId = "ClickonMap")
                # print(address_geo)
                # print(pnt$address_google)
                # pnt$address_google
                updateTextInput(session, "location", value =pnt$address_google)
        })
        
        # Get location of the click on map shape
        observeEvent(input$map_shape_click, {
                click <- input$map_shape_click
                clat <- click$lat
                clng <- click$lng
                # print(clat)
                # print(clng)
                # clng <- 40.72475
                # clat <- -73.9956
                address_geo <- as.character(paste(clat,clng))
                doc = mp_geocode(addresses =address_geo, key=mykey1)
                pnt = mp_get_points(doc)
                
                # leafletProxy("map") %>%
                #         addMarkers(data = pnt,layerId = "ClickonMap")
                # print(address_geo)
                # print(pnt$address_google)
                # pnt$address_google
                updateTextInput(session, "location", value =pnt$address_google)
        })
        
        observeEvent(input$map_marker_click, {
                click<- input$map_marker_click
                clat <- click$lat
                clng <- click$lng
                # print(clat)
                # print(clng)
                # clng <- 40.72475
                # clat <- -73.9956
                address_geo <- as.character(paste(clat,clng))
                doc = mp_geocode(addresses =address_geo, key=mykey1)
                pnt = mp_get_points(doc)
                # 
                #                 leafletProxy("map") %>%
                #                         addMarkers(data = pnt,layerId = "ClickonMap")
                # print(address_geo)
                # print(pnt$address_google)
                # pnt$address_google
                updateTextInput(session, "location", value =pnt$address_google)
        })
        
        
        # loca is what user fills in the search bar
        loca <- eventReactive(input$go, {
                loc_input <- as.character(input$location)
                # loc_input <- "Amli wallingford"
                doc = mp_geocode(addresses = loc_input, key=mykey1)
                mp_get_points(doc)
                # t$pnt[[1]][1]
        })
        
        
        observeEvent(input$go, {
                leafletProxy("map") %>% 
                        addAwesomeMarkers(data = loca(),icon = icon.fa.target)%>%
                        setView(lng = loca()$pnt[[1]][1], lat = loca()$pnt[[1]][2] , zoom = 16)
                #         
                })
        
        # Reactive Data of Pickup
        pickup_in_range <- reactive({
                # if (is.null(input$obs))
                #         return(data_pickup_pop[FALSE,])
                t1 <- input$obs_pickup[1]
                t2 <- input$obs_pickup[2]
                subset(data_pickup_pop,
                       cnt >= t1 & cnt <= t2)
        })
        
        surge_in_range <- reactive({
                # if (is.null(input$obs))
                #         return(data_pickup_pop[FALSE,])
                t1 <- input$obs_surge[1]
                t2 <- input$obs_surge[2]
                subset(data_surge,
                       surge >= t1 & surge <= t2)
        })
        
        # Control Traffic with button
        # "slow", "medium", "fast"
        observeEvent(input$InFlags, {
                # selection <- input$InFlags
                # 
                # print(selection)
                choice1 <-  c("slow","medium")
                choice2 <-  c("slow")
                choice3 <-  c("medium")
                if (is.null(input$InFlags)) {
                        print("h")
                        leafletProxy("map") %>% 
                                hideGroup(group = "traffic_heavy")%>%
                                hideGroup(group = "traffic_medium")
                }
                
                else if (length(input$InFlags)==2) {
                        leafletProxy("map") %>% 
                                showGroup(group = "traffic_heavy")%>%
                                showGroup(group= "traffic_medium")
                }
                
                else if (length(input$InFlags==1)) {
                        if (input$InFlags==choice2) {
                        leafletProxy("map") %>% 
                                showGroup(group = "traffic_heavy")%>%
                                hideGroup(group = "traffic_medium")
                        }
                        if (input$InFlags==choice3) {
                                leafletProxy("map") %>% 
                                        hideGroup(group = "traffic_heavy")%>%
                                        showGroup(group = "traffic_medium")
                                }
                        }
           
                }, ignoreNULL=FALSE)
        
        #
        # Map for Driving Mode
        output$map <- renderLeaflet({
                # doc = mp_geocode(addresses = loca(), key=mykey1)
                # doc = mp_geocode(addresses = "115 Broadway E, Seattle, WA 98102", key=mykey1)                
                # pnt = loca()
                leaflet() %>% 
                        addProviderTiles("CartoDB.DarkMatter", group = "DarkMatter") %>%
                        addProviderTiles("Stamen.TonerLite", group = "TonerLite") %>%
                        addProviderTiles("Stamen.Toner", group = "Toner") %>%
                        addCircleMarkers(data = current_gps,weight = 10, radius = 20) %>% 
                        setView(lng =current_gps[1], lat =current_gps[2] , zoom = 24) %>%
                        addLayersControl(baseGroups = c("DarkMatter","Toner Lite","Toner"),
                                         overlayGroups = c("Traffic", "Picup", "Surge", "Event")
                                         # %>%
                                         
                                         # layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
                        ) %>%
                        addEasyButton(easyButton(
                                icon = "fa-crosshairs", title = "Recenter",
                                onClick = JS("function(btn, map){ map.setView([40.72465, -73.99539],23);}"))) 
                # %>%
                #         addEasyButton(easyButton(
                #                 icon = "fa-crosshairs", title = "Overview Mode",
                #                 onClick = JS("function(btn, map){ map.setView([40.72465, -73.99539],14);}")))
                # 
        })
        
        # Click on Navigate and show Navigation
        
        observe({
                click <- input$navigate
                if(is.null(click))
                        return()
                
                destination <- isolate(input$location)
                
                doc = mp_directions(
                        origin =  "NYU Wagner",
                        destination = destination,
                        alternatives = TRUE,
                        key = mykey1
                )
                seg = mp_get_segments(doc)
                
                # pal = colorFactor(
                #         palette = sample(colors(), length(unique(seg$segment_id))),
                #         domain = seg$segment_id
                # )
                # 
                leafletProxy("map",data=seg) %>% 
                        # removeShape(layerId = "navigation_polylines") %>%
                        clearGroup(group = "nav") %>%
                        addPolylines(group = "nav",opacity = 1, weight = 7, color = "#337CFF", popup = ~instructions) %>%
                        setView(lng =current_gps[1], lat =current_gps[2] , zoom = 18)
                
        })
        
        # Test for refreshing Map based on Zoom level
        observe({
                if(!is.null(input$map_zoom)){
                        if (input$map_zoom >= 15) {
                                shinyjs::show(id = "conditionalPanel")
                                shinyjs::hide(id = "conditionalPanel2")
                                
                                # pnt = loca()
                                leafletProxy("map") %>% 
                                        clearControls() %>%
                                        # clearShapes() %>%
                                        clearGroup(group='one') %>%
                                        addAwesomeMarkers(data = event_geo, icon = icon.concert, 
                                                          # clusterOptions = markerClusterOptions(),
                                                          popup = paste0("<strong>Name: </strong>", event_geo$event_name, 
                                                                         "<br>","<strong>Attendance: </strong>", event_geo$attendance, 
                                                                         "<br>","<strong>Time: </strong>", event_geo$date_and_time)) %>% 
                                        addAwesomeMarkers(data = data_event_other, icon = icon.otherevent, 
                                                          # clusterOptions = markerClusterOptions(),
                                                          popup = paste0("<strong>Name: </strong>", data_event_other$event_name, 
                                                                         "<br>","<strong>Type: </strong>", data_event_other$event_type, 
                                                                         "<br>","<strong>Start Time: </strong>", data_event_other$start_date_time, 
                                                                         "<br>","<strong>End Time: </strong>", data_event_other$end_date_time)) %>% 
                                        addAwesomeMarkers(data = data_sport_geo, icon = icon.otherevent, 
                                                          # clusterOptions = markerClusterOptions(),
                                                          popup = paste0("<strong>Name: </strong>", data_sport_geo$event_name, 
                                                                         # "<br>","<strong>Type: </strong>", data_sport_geo$event_type, 
                                                                         "<br>","<strong>Start Time: </strong>", data_sport_geo$date)) %>% 
                                        # addCircles(data = pnt, weight = 2,color = "red", radius = 2000) %>% 
                                        addLabelOnlyMarkers(data = surge_in_range(),
                                                            lng = ~lng, lat = ~lat, label = ~as.character(surge),
                                                            labelOptions = labelOptions(noHide = T, direction = "bottom",
                                                                                        style = list(
                                                                                                "color" = "blue",
                                                                                                # "font-family" = "serif",
                                                                                                "font-style" = "italic",
                                                                                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                                                "font-size" = "20px" ))) %>%
                                        addCircles(data = pickup_in_range(), weight = 7,color = "#00A699", stroke = FALSE, fillOpacity = 0.2, radius = pickup_in_range()$cnt/2) %>% 
                                        addPolylines(group= "traffic_heavy", data = seg1, opacity = 1, weight = 7, color = "#DC143C") %>% 
                                        addPolylines(group= "traffic_heavy", data = seg2, opacity = 1, weight = 7, color = "#DC143C") %>% 
                                        addPolylines(group= "traffic_medium", data = seg3, opacity = 1, weight = 7, color = "#eeAD10") %>% 
                                        addPolylines(group= "traffic_medium", data = seg5, opacity = 1, weight = 7, color = "#eeAD10") %>% 
                                        addPolylines(group= "traffic_heavy", data = seg4, opacity = 1, weight = 7, color = "#DC143C") 
                                # addMarkers(
                                #         lng = data_surge$lng[1], lat = data_surge$lat[1],
                                #         label = "This is a label")
                                # addLabelOnlyMarkers(data = data_surge, label =  ~as.character(data_surge$surge), 
                                #                     labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
                        }
                        else if(input$map_zoom <= 14){
                                shinyjs::hide(id = "conditionalPanel")
                                shinyjs::show(id = "conditionalPanel2")
                                # print("shape clicked")
                                
                                leafletProxy("map",data=nycounties2) %>% 
                                        clearShapes() %>%
                                        # clearMarkers() %>%
                                        clearControls() %>%
                                        addPolygons(group='one',stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                                    fillColor = ~pal(log2(nycounties2$traffic)),
                                                    label = labels_traffic_overview,
                                                    labelOptions = labelOptions(
                                                            # clickable = NULL
                                                            # noHide = FALSE,
                                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                                            textsize = "15px",
                                                            direction = "auto")) %>%
                                        addLegend("bottomleft", group = "legend_overview",
                                                  colors =c("#6A4574", "#8592B6", "#F3EF89", "#8CD0AA", "#8ACEA8"),
                                                  labels= c("Slow", "","Medium","", "Fast"),
                                                  title= "Community Traffic Index",
                                                  opacity = 1)
                                
                                observeEvent(input$traffic1, {
                                        click <- input$traffic1
                                        
                                        if(is.null(click))
                                                return()
                                        leafletProxy("map",data=nycounties2) %>% 
                                                clearShapes() %>%
                                                # clearMarkers() %>%
                                                clearControls() %>%
                                                addPolygons(group='one',stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                                            fillColor = ~pal(log2(nycounties2$traffic)),
                                                            label = labels_traffic_overview,
                                                            labelOptions = labelOptions(
                                                                    # clickable = NULL
                                                                    # noHide = FALSE,
                                                                    style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                    textsize = "15px",
                                                                    direction = "auto")) %>%
                                                addLegend("bottomleft", group = "legend_overview",
                                                          colors =c("#6A4574", "#8592B6", "#F3EF89", "#8CD0AA", "#8ACEA8"),
                                                          labels= c("Slow", "","Medium","", "Fast"),
                                                          title= "Community Traffic Index",
                                                          opacity = 1)
                                                # addLegend(pal = ~pal(log2(nycounties2$traffic)), values = ~log2(nycounties2$traffic), opacity = 0.7, title = NULL,
                                                #           position = "bottomleft" )
                                })
                                
                                observeEvent(input$pickup1, {
                                        click <- input$pickup1
                                        
                                        if(is.null(click))
                                                return()
                                        leafletProxy("map",data=nycounties2) %>% 
                                                clearShapes() %>%
                                                # clearMarkers() %>%
                                                clearControls() %>%
                                                addPolygons(group='one',stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                                            fillColor = ~pal(log2(nycounties2$pickup)),
                                                            label = labels_pickup_overview,
                                                            labelOptions = labelOptions(
                                                                    style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                    textsize = "15px",
                                                                    direction = "auto")) %>%
                                                addLegend("bottomleft", group = "legend_overview",
                                                          colors =c("#6A4574", "#8592B6", "#F3EF89", "#8CD0AA", "#8ACEA8"),
                                                          labels= c("Low", "","Medium","", "High"),
                                                          title= "Community Pickups Index",
                                                          opacity = 1)
                                        #         addLegend(pal = ~pal_pickup_overview, values = ~log2(nycounties2$pickup), opacity = 0.7, title = NULL,
                                        #                   position = "bottomleft" )
                                })
                                
                                observeEvent(input$surge1, {
                                        click <- input$surge1
                                        
                                        if(is.null(click))
                                                return()
                                        leafletProxy("map",data=nycounties2) %>% 
                                                clearShapes() %>%
                                                # clearMarkers() %>%
                                                clearControls() %>%
                                                addPolygons(group='one',stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                                            fillColor = ~pal(log2(nycounties2$surge)),
                                                            label = labels_surge_overview,
                                                            labelOptions = labelOptions(
                                                                    style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                    textsize = "15px",
                                                                    direction = "auto")) %>%
                                                addLegend("bottomleft", group = "legend_overview",
                                                          colors =c("#6A4574", "#8592B6", "#F3EF89", "#8CD0AA", "#8ACEA8"),
                                                          labels= c("Low", "","Medium","", "High"),
                                                          title= "Community Surge Index",
                                                          opacity = 1)
                                        #         addLegend(pal = ~pal_pickup_overview, values = ~log2(nycounties2$pickup), opacity = 0.7, title = NULL,
                                        #                   position = "bottomleft" )
                                })
                                # 
                                # 
                                # if (input$visualization == "pickup1") {
                                #         
                                #         leafletProxy("map",data=nycounties2) %>% 
                                #                 clearShapes() %>%
                                #                 clearMarkers() %>%
                                #                 addPolygons(group='one',stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                #                             fillColor = ~pal(log2(nycounties2$pickup)),
                                #                             label = labels,
                                #                             labelOptions = labelOptions(
                                #                                     style = list("font-weight" = "normal", padding = "3px 8px"),
                                #                                     textsize = "15px",
                                #                                     direction = "auto")) %>% 
                                #                 addLegend(pal = ~pal_pickup_overview, values = ~log2(nycounties2$pickup), opacity = 0.7, title = NULL,
                                #                           position = "bottomleft" )
                                # }
                                # else if (input$visualization == "traffic1") {
                                #         leafletProxy("map",data=nycounties2) %>% 
                                #                 clearShapes() %>%
                                #                 addPolygons(group='one', stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                #                             fillColor = ~pal(log10(nycounties2$traffic)),
                                #                             label = labels,
                                #                             labelOptions = labelOptions(
                                #                                     style = list("font-weight" = "normal", padding = "3px 8px"),
                                #                                     textsize = "15px",
                                #                                     direction = "auto")) %>% 
                                #                 addLegend(pal = ~pal_traffic_overview, values = ~log2(nycounties2$traffic), opacity = 0.7, title = NULL,
                                #                           position = "bottomleft" )
                                #         
                                # }
                                # else if (input$visualization == "surge1") {
                                #         leafletProxy("map",data=nycounties2) %>% 
                                #                 clearShapes() %>%
                                #                 addPolygons(group='one', stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                #                             fillColor = ~pal(log10(nycounties2$surge)),
                                #                             label = labels,
                                #                             labelOptions = labelOptions(
                                #                                     style = list("font-weight" = "normal", padding = "3px 8px"),
                                #                                     textsize = "15px",
                                #                                     direction = "auto")) %>% 
                                #                 addLegend(pal = ~pal_surge_overview, values = ~log2(nycounties2$surge), opacity = 0.7, title = NULL,
                                #                           position = "bottomleft" )
                                #         
                                # }
                                
                                
                                # leaflet(nycounties2) %>%
                                #         addProviderTiles("Stamen.TonerLite") %>%
                                #         addAwesomeMarkers(data = pnt,icon = icon.fa.target) %>% 
                                #         addPolygons(stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                #                     fillColor = ~pal(log10(nycounties2$traffic)),
                                #                     label = labels,
                                #                     labelOptions = labelOptions(
                                #                             style = list("font-weight" = "normal", padding = "3px 8px"),
                                #                             textsize = "15px",
                                #                             direction = "auto"))%>%
                                #         addEasyButton(easyButton(
                                #                 icon = "fa-crosshairs", title = "Locate Me",
                                #                 onClick = JS("function(btn, map){ map.setView([40.67838,-73.94822],14);}")))
                        }
                }
        })
        
        ######################## Tab of History ################################
        
        nycounties3InBounds <- reactive({
                if (is.null(input$history_bounds))
                        return(nycounties3[FALSE,])
                bounds <- input$history_bounds
                latRng <- range(bounds$north, bounds$south)
                lngRng <- range(bounds$east, bounds$west)
                
                subset(nycounties3,
                       latitude >= latRng[1] & latitude <= latRng[2] &
                               longitude >= lngRng[1] & longitude <= lngRng[2])
        })
        
        output$history <- renderLeaflet({
                doc = mp_geocode(addresses = "Nostrand Avenue Station", key=mykey1)
                pnt = mp_get_points(doc)
                
                labels2 <- sprintf(
                        "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
                        nycounties3$name, nycounties3$income
                ) %>% lapply(htmltools::HTML)
                
                leaflet(nycounties3) %>%
                        addProviderTiles("Stamen.TonerLite") %>%
                        # addAwesomeMarkers(data = pnt,icon = icon.fa.target) %>%
                        addPolygons(layerId= nycounties3$communityDistrict, stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                    fillColor = ~pal_income(income),
                                    # highlightOptions = highlightOptions(
                                    #         weight = 5,
                                    #         color = "#666",
                                    #         dashArray = "",
                                    #         fillOpacity = 0.7,
                                    #         bringToFront = TRUE),
                                    label = labels_income_history,
                                    labelOptions = labelOptions(
                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")) %>%
                        addLegend(pal = pal_income, values = ~log10(nycounties3$income), 
                                  position = "bottomleft", title= "Income per Hour",
                                  opacity = 1) %>%
                        addCircleMarkers(data = current_gps,weight = 10) %>% 
                        addCircleMarkers(data = current_gps, stroke=FALSE, color="white",fill = FALSE, radius = 14) %>% 
                        # setView(lng =current_gps[1], lat =current_gps[2] , zoom = 24) %>%
                        setView(lng =current_gps[1], lat =current_gps[2] , zoom = 14) %>%
                        # addEasyButton(easyButton(
                        #         icon = "fa-globe", title = "Zoom to Level 5",
                        #         onClick = JS("function(btn, map){ map.setZoom(5);}"))) %>%
                        addEasyButton(easyButton(
                                icon = "fa-car", title = "Driving Mode",
                                onClick = JS("function(btn, map){ map.setView([40.72465, -73.99539],14);}"))) %>%
                        addEasyButton(easyButton(
                                icon = "fa-crosshairs", title = "Overview Mode",
                                onClick = JS("function(btn, map){ map.setView([40.72465, -73.99539],12);}")))
                # addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
                #   labFormat = labelFormat(transform = function(x) round(10^x)))
        })
        
        # Plot of rides in History Mode
        output$rides3 <- renderPlot({
                rea_data <- nycounties3InBounds()
                df <- data.frame(area = rea_data$name, rides = rea_data$rides, clickstatus = rea_data$clickstatus)
                ggplot(df, aes(x=reorder(area, -rides), rides 
                               # ,fill=factor(ifelse(clickstatus == 1,"Selected","Others"))
                )) +
                        geom_col()+
                        # scale_fill_manual(name = "Time", values=c("light blue","grey50"),labels = c("Selected", "Others"))+
                        xlab("Community") + ylab("Monthly Average Pickups")
                # 
                # data_test <- data.frame(time=as.character(),
                #                         pickup=as.integer(),
                #                         surge=as.integer(),
                #                         traffic=as.integer(),
                #                         stringsAsFactors = FALSE)
                # data_test[1:3,"time"] <- c("Now","1 Hour","2 Hour")
                # data_test$pickup <- c(40,80,20)
                # data_test$surge <- c(1,1.5,1.1)
                # data_test$traffic <- c(1,4,2)
                # data_test$time <- as.factor(data_test$time)
                # 
                # plot.sale.bad2 <- ggplot(data=data_test,
                #                          aes(x=time,
                #                              y=pickup,
                #                              fill=factor(ifelse(time=="Now","1 Hour","Normal")))) +
                #         geom_bar(stat="identity") +
                #         scale_fill_manual(name = "Time", values=c("light blue","grey50"),labels = c("Now", "Future")) +
                #         ggtitle("Pickup Amount Prediction") +
                #         xlab("Time") +
                #         ylab("Pickup Index") +
                #         scale_x_discrete(limits=data_test$time)
                # 
                # plot.sale.bad2
                
                
                
        })
        
        # Plot of income in History Mode
        output$income3 <- renderPlot({
                rea_data <- nycounties3InBounds()
                df <- data.frame(area = rea_data$name, income = rea_data$income)
                ggplot(df, aes(x=reorder(area, -income), income)) +
                        geom_col()+
                        xlab("Community") + ylab("Income per Hour")
        })
        
        # Plot of wktime in History Mode
        output$workingtime3 <- renderPlot({
                rea_data <- nycounties3InBounds()
                rea_data <- nycounties3InBounds()
                df <- data.frame(area = rea_data$name, wktime = rea_data$wktime)
                ggplot(df, aes(x=reorder(area, -wktime), wktime)) +
                        geom_col()+
                        xlab("Community") + ylab("Profitable Time / Driving Time")
        })
        
        observeEvent(input$AmountofRides3, {
                click <- input$AmountofRides3
                
                if(is.null(click))
                        return()
                leafletProxy("history",data=nycounties3) %>% 
                        clearShapes() %>%
                        clearControls() %>%
                        addPolygons(stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                    fillColor = ~pal_rides(nycounties3$rides),
                                    label = labels_pickup_history,
                                    labelOptions = labelOptions(
                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto"))%>%
                        addLegend(pal = pal_rides, values = ~log10(nycounties3$rides),
                                  position = "bottomleft", title= "Monthly Average Pickups",
                                  opacity = 1)
        })
        
        observeEvent(input$Income1Mile3, {
                click <- input$Income1Mile3
                
                if(is.null(click))
                        return()
                leafletProxy("history",data=nycounties3) %>% 
                        clearShapes() %>%
                        clearControls() %>%
                        addPolygons(stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                    fillColor = ~pal_income(nycounties3$income),
                                    label = labels_income_history,
                                    labelOptions = labelOptions(
                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")) %>%
                        addLegend(pal = pal_income, values = ~log10(nycounties3$income), 
                                  position = "bottomleft", title= "Income per Hour",
                                  opacity = 1)
                
        })       
        
        observeEvent(input$Workingtime3, {
                click <- input$Income1Mile3
                
                if(is.null(click))
                        return()
                leafletProxy("history",data=nycounties3) %>% 
                        clearShapes() %>%
                        clearControls() %>%
                        addPolygons(stroke = TRUE,color = "black", weight = 2, smoothFactor = 1, fillOpacity = 0.6,
                                    fillColor = ~pal_wktime(nycounties3$wktime),
                                    label = labels_wktime_history,
                                    labelOptions = labelOptions(
                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto"))%>%
                        addLegend(pal = pal_wktime, values = ~log10(nycounties3$wktime), 
                                  position = "bottomleft", title= "Profitable Time / Driving Time",
                                  opacity = 1)
        })       
        
        # Try shape click and change plot
        # v <- reactiveValues(msg = "")
        
        # observeEvent(input$history_shape_click, {
        #         v$msg <- paste("Clicked shape", input$history_shape_click$id)
        #         print(v$msg)
        # })
        # 
        # Try Override and write a function to mouseover effect
        
        click <- reactive({
                click <- input$history_shape_click
                # if (is.null(click)) {
                #         return()
                # }
        })
        
        observeEvent(input$history_shape_click, {
                # click <- input$history_shape_click
                click <- click()
       
                if (is.null(click)) {
                        return()
                }
                leafletProxy("history") %>% 
                        clearGroup(group = "click") 
                # eventOver$id <- 101
                communityclick <- nycounties3[which(nycounties3$communityDistrict==click$id),]
                
                leafletProxy("history",data=communityclick) %>% 
                        addPolygons(group= "click", stroke = FALSE, weight = 1, smoothFactor = 1,fillColor = "black",fillOpacity = 0.7)
        })
        
        # observeEvent(input$history_shape_click, {
        #         click <- input$history_shape_click
        #         t1 <- nycounties3InBounds()
        #         # click$id
        #         click_id <- 101
        #         # t1 <- nycounties
        #         t1[which(t1$communityDistrict==click_id),"clickstatus"] <- 1
        #         nycounties3InBounds <- t1
        # })
        
        
        
}

# Backup Code
### Backup Code for Navigation, should be combined and deleted later
# output$map <- renderLeaflet({
#         mykey1 <- "AIzaSyDij382Bq1D_0M7ZVOaNozNkJnbO12ZIGI"
#         # origin_new <- isolate(input$Address)
#         
#         doc = mp_directions(
#                 origin = df(),
#                 destination = "University of Washington",
#                 alternatives = TRUE,
#                 key = mykey1
#         )
#         seg = mp_get_segments(doc)
#         
#         pal = colorFactor(
#                 palette = sample(colors(), length(unique(seg$segment_id))), 
#                 domain = seg$segment_id
#         )
#         leaflet(seg) %>%
#                 addProviderTiles("CartoDB.DarkMatter") %>%
#                 addPolylines(opacity = 1, weight = 7, color = ~pal(segment_id), popup = ~instructions)
#         
# })


# output$traffic <- renderDygraph({
#         dygraph(nhtemp, main = "Traffic") %>% 
#                 dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))
#         # hist(cars[1:10*as.integer(input$radius),"speed"])
# })

# 
# # Plot of Surge in Driving Mode
# output$surge <- renderPlot({
#         
#         data_test <- data.frame(time=as.character(),
#                                 pickup=as.integer(),
#                                 surge=as.integer(),
#                                 traffic=as.integer(),
#                                 stringsAsFactors = FALSE)
#         data_test[1:3,"time"] <- c("Now","1 Hour","2 Hour")
#         data_test$pickup <- c(80,40,40)
#         data_test$surge <- c(2,1.5,2.1)
#         data_test$traffic <- c(3,4,5) 
#         data_test$time <- as.factor(data_test$time)
#         
#         ggplot(data=data_test, aes(x=time, y=surge)) +
#                 geom_point(stat="identity", fill="steelblue",size = 5)+
#                 # geom_hline(yintercept=1,colour ="blue")+
#                 theme_minimal()+
#                 scale_x_discrete(limits=data_test$time)
# })
# 
# # Plot of traffic in Driving Mode
# output$traffic <- renderPlot({
#         
#         data_test <- data.frame(time=as.character(),
#                                 pickup=as.integer(),
#                                 surge=as.integer(),
#                                 traffic=as.integer(),
#                                 stringsAsFactors = FALSE)
#         data_test[1:3,"time"] <- c("Now","1 Hour","2 Hour")
#         data_test$pickup <- c(40,80,20)
#         data_test$surge <- c(1,1.5,1.1)
#         data_test$traffic <- c(1,4,2)
#         data_test$time <- as.factor(data_test$time)
#         
#         plot.sale.bad2 <- ggplot(data=data_test,
#                                  aes(x=time,
#                                      y=traffic,
#                                      fill=factor(ifelse(time=="Now","1 Hour","Normal")))) +
#                 geom_bar(stat="identity") +
#                 scale_fill_manual(name = "Time", values=c("light blue","grey50"),labels = c("Now", "Future")) +
#                 ggtitle("Traffic Index Prediction") +
#                 xlab("Time") +
#                 ylab("Traffic Index") +
#                 scale_x_discrete(limits=data_test$time)
#         
#         plot.sale.bad2
# })
# 
# # Plot of pickup in Driving Mode
# output$pickup <- renderPlot({
#         
#         data_test <- data.frame(time=as.character(),
#                                 pickup=as.integer(),
#                                 surge=as.integer(),
#                                 traffic=as.integer(),
#                                 stringsAsFactors = FALSE)
#         data_test[1:3,"time"] <- c("Now","1 Hour","2 Hour")
#         data_test$pickup <- c(40,80,20)
#         data_test$surge <- c(1,1.5,1.1)
#         data_test$traffic <- c(1,4,2)
#         data_test$time <- as.factor(data_test$time)
#         
#         plot.sale.bad2 <- ggplot(data=data_test,
#                                  aes(x=time,
#                                      y=pickup,
#                                      fill=factor(ifelse(time=="Now","1 Hour","Normal")))) +
#                 geom_bar(stat="identity") +
#                 scale_fill_manual(name = "Time", values=c("light blue","grey50"),labels = c("Now", "Future")) +
#                 ggtitle("Pickup Amount Prediction") +
#                 xlab("Time") +
#                 ylab("Pickup Index") +
#                 scale_x_discrete(limits=data_test$time)
#         
#         plot.sale.bad2
# })

# Function for click and make a circle
# observeEvent(input$map_click, {
#         ## Get the click info like had been doing
#         click <- input$map_click
#         clat <- click$lat
#         clng <- click$lng
#         address <- revgeocode(c(clng,clat))
#         
#         ## Add the circle to the map proxy
#         ## so you dont need to re-render the whole thing
#         ## I also give the circles a group, "circles", so you can
#         ## then do something like hide all the circles with hideGroup('circles')
#         leafletProxy('map') %>% # use the proxy to save computation
#                 addCircles(lng=clng, lat=clat, group='circles',
#                            weight=1, radius=1000, color='black', fillColor='orange',
#                            popup=address, fillOpacity=0.5, opacity=1)
# })
