


# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
#   devtools::install_github("jcheng5/googleCharts")
library("googleCharts")
library("shinyWidgets")


#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# If googleCharts is not installed uncomment the below lines
#if (!require(devtools))
#  install.packages("devtools")
#devtools::install_github("jcheng5/googleCharts")

current_working_dir = "~/master_classes/data_visualization_and_communication/data_vis_team_proj/"
setwd(current_working_dir)
personal_working_directory = paste(getwd(), "/giannis_kontogeorgos", sep = "")
source(paste(getwd(), "/base.R", sep = ""))

df = sqlite_to_df(
  "data_src/db.sqlite",
  query = "
  SELECT flights.*,
  orig_air.city as OriginCity,
  orig_air.airport as OriginAirport,
  dest_air.city as DestCity,
  dest_air.airport as DestAirport
  FROM flights
  INNER JOIN airports  as orig_air on orig_air.iata = flights.Origin
  INNER JOIN airports as dest_air on dest_air.iata = flights.Dest
  WHERE (Origin = 'TUL' OR Dest = 'TUL' OR Origin = 'MKE' OR Dest = 'MKE') AND Year > 2003;
  "
)

# Initial mutations for the dataframe
base_delay_plots <- df %>%
  mutate(
    Distance = replace_na(Distance, 0),
    ArrDelay = replace_na(ArrDelay, 0),
    DepDelay = replace_na(DepDelay, 0),
    CarrierDelay = replace_na(CarrierDelay, 0),
    Year = as.character(Year),
    WeatherDelay = replace_na(WeatherDelay, 0),
    NASDelay = replace_na(NASDelay, 0),
    LateAircraftDelay = replace_na(LateAircraftDelay, 0),
    SecurityDelay = replace_na(SecurityDelay, 0),
    TotalDelay = case_when((Origin == "TUL" |
                              Origin == "MKE") ~ DepDelay,
                           (Origin != "TUL" &
                              Origin != "MKE") ~ (ArrDelay - DepDelay)
    ),
    InferenceAirport = case_when((Origin == "TUL" |
                                    Origin == "MKE") ~ OriginAirport,
                                 (Origin != "TUL" &
                                    Origin != "MKE") ~ DestAirport
    ),
    InferenceCity = case_when((Origin == "TUL" |
                                 Origin == "MKE") ~ OriginCity,
                              (Origin != "TUL" &
                                 Origin != "MKE") ~ OriginAirport
    )
  ) %>%
  select(
    Origin,
    OriginCity,
    OriginAirport,
    Dest,
    DestCity,
    DestAirport,
    InferenceAirport,
    Distance,
    Year,
    Month,
    ArrDelay,
    DepDelay,
    TotalDelay,
    CarrierDelay,
    WeatherDelay,
    NASDelay,
    SecurityDelay,
    LateAircraftDelay
  ) %>%
  filter(TotalDelay > 0) %>%
  group_by(Year, InferenceAirport)

# See above for the definitions of ui and server
# constant as the user moves between years
xlim <- list(
  min = min(base_delay_plots$NASDelay) - 3,
  max = max(base_delay_plots$NASDelay) + 3
)
ylim <- list(
  min = min(base_delay_plots$TotalDelay),
  max = max(base_delay_plots$TotalDelay) + 3
)

ui <- shinyUI(
  fluidPage(
    # This line loads the Google Charts JS library
    googleChartsInit(),
    
    # Use the Google webfont "Source Sans Pro"
    tags$link(
      href = paste0(
        "http://fonts.googleapis.com/css?",
        "family=Source+Sans+Pro:300,600,300italic"
      ),
      rel = "stylesheet",
      type = "text/css"
    ),
    tags$style(type = "text/css",
               "body {font-family: 'Source Sans Pro'}"),
    
    h2("Google Charts demo"),
    titlePanel("Hello Shiny!"),
    googleBubbleChart(
      "chart",
      width = "100%",
      height = "475px",
      # Set the default options for this chart; they can be
      # overridden in server.R on a per-update basis. See
      # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
      # for option documentation.
      options = list(
        fontName = "Source Sans Pro",
        fontSize = 13,
        # Set axis labels and ranges
        hAxis = list(title = "Health expenditure, per capita ($USD)",
                     viewWindow = xlim),
        vAxis = list(title = "Life expectancy (years)",
                     viewWindow = ylim),
        # The default padding is a little too spaced out
        chartArea = list(
          top = 50,
          left = 75,
          height = "75%",
          width = "75%"
        ),
        # Allow pan/zoom
        explorer = list(),
        # Set bubble visual props
        bubble = list(
          opacity = 0.4,
          stroke = "none",
          # Hide bubble label
          textStyle = list(color = "none")
        ),
        # Set fonts
        titleTextStyle = list(fontSize = 16),
        tooltip = list(textStyle = list(fontSize = 12))
      )
    ),
    sidebarLayout(
      sliderTextInput(
        inputId = "year",
        label = "Year range slider:",
        choices = base_delay_plots$Year),
      
      mainPanel(plotOutput("distPlot"))
    )
  )
)


server <- shinyServer(function(input, output, session) {
  # Provide explicit colors for regions, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
  defaultColors <-
    c("#3366cc",
      "#dc3912")
  series <- structure(lapply(defaultColors, function(color) {
    list(color = color)
  }),
  names = levels(base_delay_plots$InferenceAirport))
  
  yearData <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- base_delay_plots %>%
      filter(Year == input$year) %>%
      select(Month, Distance, NASDelay, InferenceAirport, TotalDelay) %>%
      arrange(InferenceAirport)
  })
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf("Health expenditure vs. life expectancy, %s",
                        input$year),
        series = series
      )
    )
  })
})

shinyApp(ui = ui, server = server)