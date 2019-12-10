#####################################################################################
#  Hands-on Project                                                                 # 
#  Student: Giannis Kontogeorgos                                                      #
#  Student ID:                                                                      #
#  Email: ikontogeorgos@aueb.gr                                                       #
#  Deadline: 2/12/2019                                                             #
#####################################################################################

# Add path for the function library file
current_working_dir = "~/master_classes/data_visualization_and_communication/data_vis_team_proj/"
setwd(current_working_dir)
source(paste(getwd(), "/base.R", sep = ""))

df = ingest_one_csv("2004.csv")
plane_data_df = ingest_one_csv("plane-data.csv")
carriers = ingest_one_csv("carriers.csv")

df <- df %>%
  mutate(
    Distance = replace_na(Distance, 0),
    ArrDelay = replace_na(ArrDelay, 0),
    DepDelay = replace_na(DepDelay, 0),
    Total_delay = rowSums(cbind(ArrDelay, DepDelay))
  )

delay_per_carrier <- df %>%
  select(UniqueCarrier,ArrDelay, DepDelay, Total_delay, Distance, Month) %>%
  filter(Total_delay > 0) %>%
  group_by(UniqueCarrier, Month) %>%
  summarise(TotalDistance = sum(Distance),
            TotalArrDelay = sum(ArrDelay),
            TotalDepDelay = sum(DepDelay))

# Make a ggplot, with interactive bullet points for monthly timelapse, Total Distance, UniqueCarrier, and the relationship
# between Departures Delay and Arrivals. 5 Fields Display.
ggplot(delay_per_carrier, aes(TotalDepDelay, TotalArrDelay, size = TotalDistance, color = UniqueCarrier)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Month: {frame_time}', x = 'Departure Delay', y = 'Arrivals Delay') +
  transition_time(Month) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate1.gif")