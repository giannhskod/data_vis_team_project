# Add path for the function library file
current_working_dir = "~/master_classes/data_visualization_and_communication/data_vis_team_proj/"
setwd(current_working_dir)
data_src_dir = paste(getwd(), "/data_src/", sep = "")

source(paste(getwd(), "code.R", sep = ""))
library(ggplot2)
library(tidyr)
library(gganimate)

# Folder with it's content in .gitingore. It should contain the source files
# in your local working directory.
# Add your src file here

df = read.csv(paste(data_src_dir, "2004.csv", sep = ""), encoding = "UTf-8")
plane_data_df = read.csv(paste(data_src_dir, "plane-data.csv", sep = ""), encoding = "UTF-8")
carriers = read.csv(paste(data_src_dir, "carriers.csv", sep = ""), encoding = "UTF-8")

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

# Make a ggplot, but add frame=year: one image per year
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