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
        
        if(!require("xml2")){
                install.packages("xml2")
                library("xml2")
        }
        
        # if(!require("dygraphs")){
        #         install.packages("dygraphs")
        #         library("dygraphs")
        # }
        # if(!require("geojsonio")){
        #         install.packages("geojsonio")
        #         library("geojsonio")
        # }
        if(!require("shinyjs")){
                install.packages("shinyjs")
                library("shinyjs")
        }
        # if(!require("albersusa")){
        #         install.packages("albersusa")
        #         library("albersusa")
        # }
}

t <- getwd()
setwd(t)
### Define API Key
mykey1 <- "AIzaSyDij382Bq1D_0M7ZVOaNozNkJnbO12ZIGI"

## Static Traffic Data

seg1 <- readRDS(file = "data/seg1.rds")
seg2 <- readRDS(file = "data/seg2.rds")
seg3 <- readRDS(file = "data/seg3.rds")
seg4 <- readRDS(file = "data/seg4.rds")
seg5 <- readRDS(file = "data/seg5.rds")

# data_traffic <- geojsonio::geojson_read("path.json",what = "sp")

### Current Location

cur_location <- "NYU Wagner"
# doc = mp_geocode(addresses =cur_location, key=mykey1)
# pnt = mp_get_points(doc)
# pnt_2 <- pnt$pnt[[1]]
# saveRDS(pnt_2, file = "current_gps.rds")
# -73.99539 40.72465
current_gps <- readRDS(file = "data/current_gps.rds")

### ny mapping data for community
# nycounties <- geojsonio::geojson_read("com.geojson.json",what = "sp")

nycounties <- readRDS(file = "data/nycounties.rds")

nycounties2 <- nycounties
nycounties2$traffic <- sample(x = 100000:50000000, size = 149, replace = TRUE)
nycounties2$traffic <- log(nycounties2$traffic)
nycounties2$pickup <- runif(149, 40, 500)
nycounties2$surge <- runif(149, 1.0, 2.0)
nycounties2$surge <- round(nycounties2$surge,1)
nycounties2$name <- c(1:149)

pal <- colorNumeric("viridis", NULL)
# head(nycounties)
# class(nycounties$communityDistrict)
nycounties3 <- nycounties
nycounties3$rides <- sample(x = 1:100, size = 149, replace = TRUE)
nycounties3$income <- runif(149, 5.0, 35.0)
nycounties3$income <- round(nycounties3$income,1)
nycounties3$wktime <- runif(149, 10, 90)
nycounties3$wktime <- nycounties3$wktime/(100-nycounties3$wktime)
nycounties3$wktime <- round(nycounties3$wktime,1)
nycounties3$name <- c(1:149)


## Labels

# labels <- sprintf(
#         "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
#         nycounties2$name, nycounties2$traffic
# ) %>% lapply(htmltools::HTML)

### overview
labels_traffic_overview <- sprintf(
        "Community No.%s<br/><strong>%g Miles / hour</strong>",
        nycounties2$name, as.integer(nycounties2$traffic)
) %>% lapply(htmltools::HTML)

labels_pickup_overview <- sprintf(
        "Community No.%s<br/><strong>Daily Average Pickups: %g</strong>",
        nycounties2$name, as.integer(nycounties2$pickup)
) %>% lapply(htmltools::HTML)

labels_surge_overview <- sprintf(
        "Community No.%s<br/><strong>Surge Index: %g</strong>",
        nycounties2$name, nycounties2$surge
) %>% lapply(htmltools::HTML)

### history
labels_wktime_history <- sprintf(
        "Community No.%s<br/><strong>Profitable time / Driving time: %g</strong>",
        nycounties3$name, as.integer(nycounties3$wktime)
) %>% lapply(htmltools::HTML)

labels_income_history <- sprintf(
        "Community No.%s<br/><strong> Average Income: $%g / hour</strong>",
        nycounties3$name, as.integer(nycounties3$income)
) %>% lapply(htmltools::HTML)

labels_pickup_history <- sprintf(
        "Community No.%s<br/><strong>Monthly Average Pickups: %g</strong>",
        nycounties3$name, nycounties3$rides
) %>% lapply(htmltools::HTML)


### Popular Pickup Data
data_pickup_pop <- readRDS(file = "data/data_pickup_pop.rds")

### Surge Data
data_surge <- readRDS(file = "data/data_surge_geo.rds")

### event data
event_geo <- readRDS(file = "data/event_geo.rds")
data_event_other <- readRDS(file = "data/data_event_other.rds")
data_sport_geo <- readRDS(file = "data/data_sport_geo.rds")

### Define Icon
icon.fa.target <- makeAwesomeIcon(icon = "star", markerColor = "blue", library = "fa", iconColor = "black")
icon.fa.pickup <- makeAwesomeIcon(icon = "people", markerColor = "purple", library = "fa", iconColor = "black")
icon.concert <- makeAwesomeIcon(icon = "music", markerColor = "orange", library = "fa", iconColor = "black")
icon.otherevent <- makeAwesomeIcon(icon = "calendar", markerColor = "green", library = "fa", iconColor = "black")
icon.football <- makeAwesomeIcon(icon = "bookmark", markerColor = "blue", iconColor = "white", library = "glyphicon", squareMarker =  TRUE)


pickup_icon = makeIcon("Pickups.png", 24, 24)


### Define pal

pal_pickup_overview  <- ~pal(log2(nycounties2$pickup))
pal_traffic_overview  <- ~pal(log2(nycounties2$traffic))
pal_surge_overview  <- ~pal(log2(nycounties2$surge))

pal_income <- colorBin("YlOrRd", domain = nycounties3$income, bins = 7)
pal_rides <- colorBin("YlOrRd", domain = nycounties3$rides, bins = 7)
pal_wktime <- colorBin("YlOrRd", domain = nycounties3$wktime, bins = 7)

# pal_pickup_overview<- colorBin("Greens", domain = log2(nycounties2$pickup), bins = 7)
# pal_traffic_overview <- colorBin("viridis", domain = log2(nycounties2$traffic), bins = 7)
# pal_surge_overview <- colorBin("inferno", domain = log2(nycounties2$surge), bins = 7)


### Backup Code


# icon.fa.train <- makeAwesomeIcon(icon = "subway", markerColor = "purple", library = "fa", iconColor = "black")
# icon.fa.hotel <- makeAwesomeIcon(icon = "hotel", markerColor = "blue", library = "fa", iconColor = "black")
# icon.fa.restaurant <- makeAwesomeIcon(text = "F", markerColor = "lightgreen", library = "fa",iconColor = "black")


# icon.fa.me <- makeAwesomeIcon(icon = "Me", markerColor = "red", library = "fa",iconColor = "black")

