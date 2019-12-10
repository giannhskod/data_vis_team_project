###################################################################
# DEPENDENCIES and GLOBALS                                      ###
###################################################################

# Add path for the function library file
current_working_dir = ""
setwd(current_working_dir)
source(paste(getwd(), "/base.R", sep = ""))

df = ingest_one_csv("2004.csv")
plane_data_df = ingest_one_csv("plane-data.csv")
carriers = ingest_one_csv("carriers.csv")
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

