# Add path for the function library file
current_working_dir = "C:/Users/dimitris/Documents/DataScience/visualization/Hands_on_project/data_vis_team_project/"
setwd(current_working_dir)
source(paste(getwd(), "/base.R", sep = ""))

sql_lite_db = df_to_sqlite

df_TUL = sqlite_to_df("C:/Users/dimitris/Documents/DataScience/visualization/Hands_on_project/plane_data/db.sqlite", query = "select * from flights where (Origin = 'TUL' OR Dest = 'TUL') and Year > 2003;")
df_MKE = sqlite_to_df("C:/Users/dimitris/Documents/DataScience/visualization/Hands_on_project/plane_data/db.sqlite", query = "select * from flights where (Origin = 'MKE' OR Dest = 'MKE') and Year > 2003;")
df_test = sqlite_to_df("C:/Users/dimitris/Documents/DataScience/visualization/Hands_on_project/plane_data/db.sqlite", query = "select * from flights where Origin = 'MKE' and Year > 2003 and UniqueCarrier = 'CO';")

#df_t = sqlite_to_df("C:/Users/dimitris/Documents/DataScience/visualization/Hands_on_project/plane_data/db.sqlite", query = "SELECT name FROM sqlite_master WHERE type ='table' AND name NOT LIKE 'sqlite_%';")
#df_tt = sqlite_to_df("C:/Users/dimitris/Documents/DataScience/visualization/Hands_on_project/plane_data/db.sqlite", query = "select * from airports;")
#plane_data_df = ingest_one_csv("C:/Users/dimitris/Documents/DataScience/visualization/Hands_on_project/plane_data/plane-data.csv")
#carriers = ingest_one_csv("C:/Users/dimitris/Documents/DataScience/visualization/Hands_on_project/plane_data/carriers.csv")

plot_TUL_df = group_by(df_TUL, Year,)
plot_MKE_df = group_by(df_MKE, Year,)
count_TUL_df = summarise(plot_TUL_df, TotalTraffic = n())
count_MKE_df = summarise(plot_MKE_df, TotalTraffic = n())
count_TUL_df$Airport = rep('Tulsa',nrow(count_TUL_df))
count_MKE_df$Airport = rep('Milwauke',nrow(count_MKE_df))
count_df = rbind(count_TUL_df , count_MKE_df)

# Plot
g_1 <- ggplot(count_df, aes(factor(Year), TotalTraffic,fill = Airport))+ 
  geom_bar(stat = "identity", width = 0.5, position="dodge") +
  labs(title = "TUL & MKE total Traffic throughout the years") +
  theme_ipsum() +
  ylab("Traffic (Destination + Arrivals)") +
  xlab("Years") +
  theme(
    axis.title.y = element_text(
      color = "#3D3D3D",
      size = 14,
      face = "bold",
      hjust = 0.5,
      margin = margin(
        t = 0,
        r = 20,
        b = 0,
        l = 0
      )
    ),
    axis.title.x = element_text(
      color = "#3D3D3D",
      size = 14,
      face = "bold",
      hjust = 0.5,
      margin = margin(
        t = 20,
        r = 0,
        b = 0,
        l = 0
      )
    ),
    legend.title = element_text(color = "black", size = 14),
    legend.text = element_text(size = 20))
g_1
###############
df_TUL_sev_eig = sqlite_to_df("C:/Users/dimitris/Documents/DataScience/visualization/Hands_on_project/plane_data/db.sqlite", query = "select * from flights where (Origin = 'TUL' OR Dest = 'TUL') and Year > 2006;")
df_MKE_sev_eig = sqlite_to_df("C:/Users/dimitris/Documents/DataScience/visualization/Hands_on_project/plane_data/db.sqlite", query = "select * from flights where (Origin = 'MKE' OR Dest = 'MKE') and Year > 2006;")

df_TUL_sev = subset(df_TUL_sev_eig, Year == "2007")
df_TUL_eig = subset(df_TUL_sev_eig, Year == "2008")

df_MKE_sev = subset(df_MKE_sev_eig, Year == "2007")
df_MKE_eig = subset(df_MKE_sev_eig, Year == "2008")

plot_TUL_df_sev = group_by(df_TUL_sev, Month)
plot_MKE_df_sev = group_by(df_MKE_sev, Month)
count_TUL_df_sev = summarise(plot_TUL_df_sev, TotalTraffic = n())
count_MKE_df_sev = summarise(plot_MKE_df_sev, TotalTraffic = n())
count_TUL_df_sev$Airport = rep('Tulsa',nrow(count_TUL_df_sev))
count_MKE_df_sev$Airport = rep('Milwauke',nrow(count_MKE_df_sev))
count_df_sev = rbind(count_TUL_df_sev , count_MKE_df_sev)

count_df_sev %>%
  ggplot( aes(x=factor(Month), y=TotalTraffic, group=Airport, color=Airport)) +
  geom_line(size=2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "TUL & MKE total Traffic 2007") +
  theme_ipsum() +
  ylab("Traffic (Destination + Arrivals)") +
  xlab("Months") +
  theme(
    axis.title.y = element_text(
      color = "#3D3D3D",
      size = 14,
      face = "bold",
      hjust = 0.5,
      margin = margin(
        t = 0,
        r = 20,
        b = 0,
        l = 0
      )
    ),
    axis.title.x = element_text(
      color = "#3D3D3D",
      size = 14,
      face = "bold",
      hjust = 0.5,
      margin = margin(
        t = 20,
        r = 0,
        b = 0,
        l = 0
      )
    ),
    legend.title = element_text(color = "black", size = 14),
    legend.text = element_text(size = 20))


plot_TUL_df_eig = group_by(df_TUL_eig, Month)
plot_MKE_df_eig = group_by(df_MKE_eig, Month)
count_TUL_df_eig = summarise(plot_TUL_df_eig, TotalTraffic = n())
count_MKE_df_eig = summarise(plot_MKE_df_eig, TotalTraffic = n())
count_TUL_df_eig$Airport = rep('Tulsa',nrow(count_TUL_df_eig))
count_MKE_df_eig$Airport = rep('Milwauke',nrow(count_MKE_df_eig))
count_df_eig = rbind(count_TUL_df_eig , count_MKE_df_eig)

count_df_eig %>%
  ggplot( aes(x=factor(Month), y=TotalTraffic, group=Airport, color=Airport)) +
  geom_line(size=2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "TUL & MKE total Traffic 2008") +
  theme_ipsum() +
  ylab("Traffic (Destination + Arrivals)") +
  xlab("Months") +
  theme(
    axis.title.y = element_text(
      color = "#3D3D3D",
      size = 14,
      face = "bold",
      hjust = 0.5,
      margin = margin(
        t = 0,
        r = 20,
        b = 0,
        l = 0
      )
    ),
    axis.title.x = element_text(
      color = "#3D3D3D",
      size = 14,
      face = "bold",
      hjust = 0.5,
      margin = margin(
        t = 20,
        r = 0,
        b = 0,
        l = 0
      )
    ),
    legend.title = element_text(color = "black", size = 14),
    legend.text = element_text(size = 20))



##################################
