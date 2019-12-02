###################################################################
# DEPENDENCIES and GLOBALS                                      ###
###################################################################

# Install required packages if not already installed
chooseCRANmirror(graphics = TRUE, ind = c(1, 2, 3, 4, 5))
knitr::opts_chunk$set(echo = TRUE)

list.of.packages = c("dplyr", "dbplyr", "ggplot2", "kableExtra", "readr", "RSQLite", "sf", "shiny", "stringr","data.table","hrbrthemes")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

if(length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE)
}

suppressMessages(library("data.table"))
suppressMessages(library("DBI"))
suppressMessages(library("dplyr"))
suppressMessages(library("dbplyr"))
suppressMessages(library("ggplot2"))
suppressMessages(library("hrbrthemes"))
#suppressMessages(library("igraph"))
suppressMessages(library("kableExtra"))
suppressMessages(library("lubridate"))
suppressMessages(library("readr"))
suppressMessages(library("DBI"))
#suppressMessages(library("rnaturalearth"))
#suppressMessages(library("rnaturalearthhires"))
suppressMessages(library("sf"))
suppressMessages(library("shiny"))
suppressMessages(library("stringr"))
suppressMessages(library("hrbrthemes"))

theme = theme_ipsum()

###################################################################
# DATA                                                          ###
###################################################################

#' Prepares the flight dataset.
#'
#' @param: dataframe
#' @return: data.frame
clean = function(df) {
  # TailNum
  # Remove non-unicode chars.
  df = df %>% mutate_at(vars(TailNum), function(x){gsub("[^ -~]", "", x)})
  # Keep only numbers and characters.
  df = df %>% mutate_at(vars(TailNum), function(x){gsub("[^A-Za-z0-9 ]", "", x)})
  
  return(df)
}

#' Function for ingesting a .csv file.
#'
#' @param: .csv file path
#' @return: data.frame
ingest_one_csv = function(file_path) {
  df = read.csv(file_path, encoding = "ASCII")
  
  return(df)
}

#' Function for ingesting a .rds file.
#'
#' @param: .rds file path
#' @return: data.frame
ingest_one_rds = function(file_path) {
  df = readRDS(file_path)
  
  return(df)
}

#' Function for ingesting and concatenating .csv files in a directory.
#'
#' @param: .csv files directory
#' @return: data.frame
ingest_all_csv = function(path) {
  df = list.files(path = path, , pattern = ".csv", full.names = TRUE) %>%
    lapply(read.csv) %>% 
    bind_rows
  
  return(df)
}

#' Function for ingesting and concatenating .rds files in a directory.
#'
#' @param: .rds files directory
#' @return: data.frame
ingest_all_rds = function(path) {
  file_list = list.files(path = path, pattern = ".rds")
  df = unlist(lapply(file_list, readRDS))
  
  return(df)
}

#' Insert / append to a SQLLite table.
#'
#' @param: dataframe
#' @param: SQLLite table name
#' @param: mode
#' @return: dataframe
df_to_sqlite = function(df, db.name = "db.sqlite", table.name, mode = "append") {
  db = dbConnect(RSQLite::SQLite(), db.name)
  
  if(mode == "append") {
    dbWriteTable(db, table.name, df, append = TRUE)
  }
  
  if(mode == "overwrite") {
    dbWriteTable(db, table.name, df, overwrite = TRUE)
  }
  
  dbDisconnect(db)
  
  return(db)
}

#' Retrieves a SQLite query in a data.frame format.
#'
#' @param: database name
#' @param: SQL query
#' @return: data.frame
sqlite_to_df = function(db.name = "db.sqlite", query) {
  df = NULL
  
  # Connect and fetch.
  out = tryCatch(
    {
      # Connect to db.
      db = dbConnect(RSQLite::SQLite(), db.name)
      
      # Get result set.
      result_set = dbSendQuery(db, query)
      
      # To data.frame.
      df = fetch(result_set)
      
      return(df)
    }, 
    error = function(e) {
      message("Exception: ")
      message(e)
      
      return(NULL)
    }, 
    warning = function(e) {
      message("Warning: ")
      message(e)
      
      return(NULL)
    }
  )
  
  return(out)
}

#' Augments the flights data.frame with "origin" and "destination" information.
#'
#' @param: flights data.frame
#' @param: airports data.frame
#' @return: data.frame
# flights_with_coordinates = function(flights_df, airports_df) {
#   # Create a data.frame with "origin" additional information.
#   df = inner_join(
#     flights_df, 
#     airports_df, 
#     by = c("Origin" = "iata"),
#     copy = FALSE, 
#     suffix = c(".f", ".o")
#   )
#   
#   # Augment the data.frame with "destination" information.
#   df = inner_join(
#     df, 
#     airports_df, 
#     by = c("Dest" = "iata"),
#     copy = FALSE, 
#     suffix = c(".o", ".d")
#   )
#   
#   # Also, translate (lat,long) values to geom.
#   df[["geom.o"]] = df %>%
#     st_as_sf(coords = c("long.o", "lat.o"), crs = 4326)
#   
#   df[["geom.d"]] = df %>%
#     st_as_sf(coords = c("long.d", "lat.d"), crs = 4326)
#   
#   return(df)
# }


add_datetime_column = function(df, field) {
  
  df = df %>%
    mutate(
      field = 
        parse_date_time(
          paste(
            paste(
              paste(
                df$Year, df$Month, sep = "-"
              ), 
              df$DayofMonth, sep = "-"
            ), 
            paste(
              substr(df[[field]], 0, 2), substr(df[[field]], 3, 4), sep = ":")
            ), 
          order = "y-m-d HM"
        )
      )
  
  return(df)
}

#' Converts (lat,long) pairs to geom points.
#'
#' @param: data.frame
#' @return: data.frame
lat_log_to_geom = function(df) {
  df = df %>% st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  return(df)
}


###################################################################
# PLOTS                                                         ###
###################################################################

testBarLinePlotWithFacets = function() {
  data.plot = data.demographics_gdp %>%
    filter(
      country == "European Union - 28 countries" | 
        country == "Portugal" | 
        country == "Italy" | 
        country == "Greece" | 
        country == "Spain", 
      sex == "Total"
    ) 
  
  # Convert NAs to 0s.
  data.plot[is.na(data.plot)] = 0
  
  # Normalize data.
  data.plot["life_expectancy"] = lapply(data.plot["life_expectancy"], min_max_normalize)
  data.plot["gdp"] = lapply(data.plot["gdp"], min_max_normalize)
  
  data.plot = data.plot %>% 
    select(
      country, 
      year, 
      life_expectancy, 
      gdp
    ) %>% 
    mutate(
      year = as.factor(year)
    ) %>% 
    group_by(
      year, 
      country
    ) %>%
    summarise(
      life_expectancy_mean = mean(life_expectancy),
      gdp_mean = mean(gdp)
    )
  
  # Plot.
  ggplot(
    data.plot
  ) + 
  geom_col(
    aes(
      x = year, 
      y = life_expectancy_mean
    ), 
    position = "stack",
    fill = "grey90"
  ) +
  geom_point(
    aes(
      x = year, 
      y = gdp_mean, 
      col = country
    ),
    size = 2
  ) +
  geom_line(
    aes(
      x = year, 
      y = gdp_mean, 
      col = country, 
      group = country
    ),
    size = 1
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~ . )
  ) +
  # Labels.
  labs(
    x = "Year", 
    y = "Life expectancy / GDP per capita", 
    title = "Life Expectancy vs. GDP per capita (normalized)", 
    subtitle = "European Union - 28 countries, Portugal, Italy, Greece and Spain\n\n2000-2017"
  ) + 
  # Color.
  scale_fill_manual(
    name = "Mean life expectancy",
    values = c("#FFCC00", "#0000FF", "#20b32c", "#662F00", "#ff7700"),
    labels = c("E.U.","Greece", "Italy", "Portugal", "Spain")
  ) + 
  scale_colour_manual(
    name = "Mean GDP per capita", 
    values = c("#FFCC00", "#0000FF", "#20b32c", "#662F00", "#ff7700"), 
    labels = c("E.U.","Greece", "Italy", "Portugal", "Spain")
  ) + 
  theme + 
  guides(
    fill = guide_legend(title = "Mean life expectancy"), 
    color = guide_legend(title = "Mean GDP per capita")
  ) + 
  theme(legend.position = "bottom") + 
  theme(
    legend.background = element_rect(
      fill = "gray95", 
      size = 1, 
      linetype = "solid", 
      colour = "gray60"
    )
  ) + 
  facet_grid(country ~ .)
}

