source("code/queries.R")
source("code/base.R")
source("code/data.R")

###################################################################
# SPATIAL VISUALIZATIONS                                        ###
###################################################################

# Install required packages if not already installed
chooseCRANmirror(graphics = TRUE, ind = c(1, 2, 3, 4, 5))
knitr::opts_chunk$set(echo = TRUE)

list.of.packages = c(
  "geosphere", 
  "leaflet", 
  "networkD3", 
  "RColorBrewer", 
  "rnaturalearth"
)
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE)
}

suppressMessages(library("geosphere"))
suppressMessages(library("leaflet"))
suppressMessages(library("networkD3"))
suppressMessages(library("RColorBrewer"))
suppressMessages(library("rnaturalearth"))

airport.icons = iconList(
  icon.tul = makeIcon("icons/airport-icon.png", "icon.tul@2x.png", 18, 18),
  icon.mke = makeIcon("icons/airport-icon.png", "icon.mke@2x.png", 18, 18)
)

#' Function for normalizing a variable.
#'
#' @param: variable value
#' @return: normalized variable value
min_max_normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

map.flights.outbound = function(df) {
  map_df = gcIntermediate(
    df[c("origin_airport_long", "origin_airport_lat")], 
    df[c("destination_airport_long", "destination_airport_lat")], 
    n = 50, 
    sp = TRUE, 
    addStartEnd = TRUE
  )
  map_df$counts = df$itineraries_count
  map_df$origins = df$origin_airport_name
  map_df$destinations = df$destination_airport_name
  
  tooltip.arc = paste0(
    map_df$origins, 
    " to ", 
    map_df$destinations, 
    " - ",
    "Flights: ", 
    as.character(map_df$counts)
  )
  
  color.palette = colorFactor(brewer.pal(3, "Dark2"), map_df$origins)
  
  icons.airport = icons(
    iconUrl = ifelse(
      df$origin_airport_code == "TUL",
      "icons/icon.tul.svg",
      "icons/icon.mke.svg"
    ),
    iconWidth = 18, 
    iconHeight = 18, 
    iconAnchorX = 0, 
    iconAnchorY = 0
  )
  
  plot = leaflet(df) %>% 
  addProviderTiles("Wikimedia") %>% 
  addPolylines( 
    data = map_df, 
    weight = ~pmax(min_max_normalize(counts) * 10, 1), 
    label = tooltip.arc, 
    group = ~origins, 
    color = ~color.palette(origins)
  ) %>% 
  addMarkers(
    ~destination_airport_long,
    ~destination_airport_lat,
    label = ~as.character(destination_airport_name),
    icon = icons.airport
  ) %>% 
  addLayersControl(
    overlayGroups = unique(map_df$origins), 
    options = layersControlOptions(collapsed = TRUE)
  )
  
  return(plot)
}

map.flights.delays.security = function(df) {
  map_df = gcIntermediate(
    df[c("origin_airport_long", "origin_airport_lat")], 
    df[c("destination_airport_long", "destination_airport_lat")], 
    n = 50, 
    sp = TRUE, 
    addStartEnd = TRUE
  )
  map_df$counts = df$average_security_delay
  map_df$origins = df$origin_airport_name
  map_df$destinations = df$destination_airport_name
  
  tooltip.arc = paste0(
    map_df$origins, 
    " to ", 
    map_df$destinations, 
    " - ",
    "Average security delay: ", 
    as.character(round(map_df$counts, digits = 2))
  )
  
  color.palette = colorFactor(brewer.pal(3, "Dark2"), map_df$origins)
  
  icons.airport = icons(
    iconUrl = ifelse(
      df$origin_airport_code == "TUL",
      "icons/icon.tul.svg",
      "icons/icon.mke.svg"
    ),
    iconWidth = 18, 
    iconHeight = 18, 
    iconAnchorX = 0, 
    iconAnchorY = 0
  )
  
  plot = leaflet(df) %>% 
    addProviderTiles("Wikimedia") %>% 
    addPolylines( 
      data = map_df, 
      weight = ~pmax(min_max_normalize(counts) * 10, 1), 
      label = tooltip.arc, 
      group = ~origins, 
      color = ~color.palette(origins)) %>% 
    addMarkers(
      ~destination_airport_long,
      ~destination_airport_lat,
      label = ~as.character(destination_airport_name),
      icon = icons.airport
    ) %>% 
    addLayersControl(
      overlayGroups = unique(map_df$origins), 
      options = layersControlOptions(collapsed = TRUE)
    )
  
  return(plot)
}
