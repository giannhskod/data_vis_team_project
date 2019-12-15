#####################################################################################
#  Hands-on Project                                                                 #
#  Student: Giannis Kontogeorgos                                                      #
#  Student ID:                                                                      #
#  Email: ikontogeorgos@aueb.gr                                                       #
#  Deadline: 2/12/2019                                                             #
#####################################################################################


###########################################
#  Delay's relevant graps                 #
#                                         #
#                                         #
#                                         #
###########################################
current_working_dir = "~/master_classes/data_visualization_and_communication/data_vis_team_proj/"
personal_working_directory = paste(getwd(), "/giannis_kontogeorgos", sep = "")
setwd(current_working_dir)
source(paste(getwd(), "/base.R", sep = ""))

df = sqlite_to_df(
  "data_src/db.sqlite",
  query = "
  select *
  from flights
  where (Origin = 'TUL' OR Dest = 'TUL'
  OR
  Origin = 'MKE' OR Dest = 'MKE')
  AND Year > 2003;"
)

# Initial mutations for the dataframe
base_delay_plots <- df %>%
  mutate(
    Distance = replace_na(Distance, 0),
    ArrDelay = replace_na(ArrDelay, 0),
    DepDelay = replace_na(DepDelay, 0),
    CarrierDelay = replace_na(CarrierDelay, 0),
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
                                    Origin == "MKE") ~ Origin,
                                 (Origin != "TUL" &
                                    Origin != "MKE") ~ Dest
    )
  ) %>%
  select(
    Origin,
    Dest,
    InferenceAirport,
    Year,
    Month,
    DayofMonth,
    DayOfWeek,
    ArrDelay,
    DepDelay,
    TotalDelay,
    CarrierDelay,
    WeatherDelay,
    NASDelay,
    SecurityDelay,
    LateAircraftDelay
  )

plane_data_df = ingest_one_csv("plane-data.csv")
carriers = ingest_one_csv("carriers.csv")

#############################
# Resuable dataframes #
#############################

grouped_annual_avg_positive <- base_delay_plots %>%
  filter(TotalDelay > 0) %>%
  group_by(InferenceAirport, Year) %>%
  summarise(
    TotalAvgDelay = mean(TotalDelay),
    CarrierAvgDelay = mean(CarrierDelay),
    WeatherAvgDelay = mean(WeatherDelay),
    NASAvgDelay = mean(NASDelay),
    SecurityAvgDelay = mean(SecurityDelay),
    LateAircraftAvgDelay = mean(LateAircraftDelay)
  )

grouped_monthly_avg_positive <- base_delay_plots %>%
  filter(TotalDelay > 0) %>%
  group_by(InferenceAirport, Year, Month) %>%
  summarise(
    TotalAvgDelay = mean(TotalDelay),
    CarrierAvgDelay = mean(CarrierDelay),
    WeatherAvgDelay = mean(WeatherDelay),
    NASAvgDelay = mean(NASDelay),
    SecurityAvgDelay = mean(SecurityDelay),
    LateAircraftAvgDelay = mean(LateAircraftDelay)
  )

#############################
# First PLot Dodged Barplot #
#############################

plot_1 <-
  grouped_monthly_avg_positive %>%
  group_by(InferenceAirport, Year) %>%
  summarise(SumAnnualDelay = sum(TotalAvgDelay)) %>%
  ggplot(aes(fill = InferenceAirport, y = SumAnnualDelay, x = Year)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_ipsum() +
  labs(
    x = "Years 2004 - 2008",
    y = "Annual Delays in minutes",
    title = "Annual Aggragated Delays",
    subtitle = paste(
      "TotalDelay per entry = DepartureDelay if Origin = InferenceAiroport else ArrivalDelay - DepartureDelay",
      "MonthlyDelay = mean(TotalDelay)",
      "AnnualDelay = sum(MonthlyDelay)",
      sep = "\n"
    )
  )

plot_1


#############################
# HEATMAPS #
#############################
hmp_plot <- function (df, ...) {
  new_index <- df$Year
  df <- subset(df,  select = colnames(df)[-1])
  rownames(df) <- new_index
  plot <- heatmaply(
    as.matrix(df),
    dendrogram = "none",
    margins = c(60, 100, 40, 20),
    grid_color = "white",
    grid_width = 0.00001,
    branches_lwd = 0.1,
    label_names = c("Years", "Delay Type", "Delay Value"),
    fontsize_row = 10,
    fontsize_col = 10,
    labCol = colnames(df),
    labRow = rownames(df),
    heatmap_layers = theme(axis.line = element_blank()),
    ...
  )
  return (plot)
}
select_values <- c("Year",
                   "CarrierAvgDelay",
                   "WeatherAvgDelay",
                   "NASAvgDelay",
                   "SecurityAvgDelay",
                   "LateAircraftAvgDelay")
heatmap_df_tul <- grouped_annual_avg_positive %>% 
  filter(InferenceAirport == "TUL") %>%
  ungroup() %>%
  select(select_values) %>%
  arrange(desc(Year))

heatmap_df_mke <- grouped_annual_avg_positive %>% 
  filter(InferenceAirport == "MKE") %>%
  ungroup() %>%
  select(select_values) %>%
  arrange(desc(Year))
  
heatmap_mke <- hmp_plot(heatmap_df_mke,
                        xlab = " ",
                        ylab ="",
                        main = "Milwaukee Airport Average Delay Types years 2004-2008"
)

heatmap_tul <- hmp_plot(heatmap_df_tul,
                        xlab = " ",
                        ylab ="",
                        main = "Tulsa Airport Average Delay Types years 2004-2008"
)

heatmap_tul
heatmap_mke
#############################
# Active PLot  #
#############################

delay_per_carrier <- df %>%
  select(UniqueCarrier, ArrDelay, DepDelay, Total_delay, Distance, Month) %>%
  filter(Total_delay > 0) %>%
  group_by(UniqueCarrier, Month) %>%
  summarise(
    TotalDistance = sum(Distance),
    TotalArrDelay = sum(ArrDelay),
    TotalDepDelay = sum(DepDelay)
  )

# Make a ggplot, with interactive bullet points for monthly timelapse, Total Distance, UniqueCarrier, and the relationship
# between Departures Delay and Arrivals. 5 Fields Display.
ggplot(
  delay_per_carrier,
  aes(
    TotalDepDelay,
    TotalArrDelay,
    size = TotalDistance,
    color = UniqueCarrier
  )
) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Month: {frame_time}', x = 'Departure Delay', y = 'Arrivals Delay') +
  transition_time(Month) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")
