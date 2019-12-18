# Add path for the function library file
current_working_dir = "~/master_classes/data_visualization_and_communication/data_vis_team_proj/"
setwd(current_working_dir)
source(paste(getwd(), "/base.R", sep = ""))

sql_lite_db = df_to_sqlite
df = sqlite_to_df("data_src/db.sqlite", query = "select * from flights where Origin = 'TUL' OR Dest = 'TUL' and Year > 2003;")
plane_data_df = ingest_one_csv("plane-data.csv")
carriers = ingest_one_csv("carriers.csv")

plot_1_df <- group_by(df, Year)
count_df = summarise(plot_1_df, TotalTraffic = n())


# Plot
g_1 <- ggplot(count_df, aes(factor(Year), TotalTraffic))
g_1 + geom_bar(stat = "identity", width = 0.5, fill = "tomato2") +
  labs(title = "TUL total Traffic throughout the years") +
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
    ))

CancelledFl <- df[ which(df$Cancelled == 1 ), ]
CancelledFl

CancellationsYearR <- setNames(aggregate(CancelledFl$Cancelled, 
                                          by=list(CancelledFl$Year, CancelledFl$CancellationCode), 
                                          FUN=sum), c("Year","Reason", "Cancelled"))
CancellationsYearR

cncl_code <- c("A","B", "C", "D")
cncl_reason <- c("Carrier", "Weather", "NAS", "Security") 

p4 <- ggplot(CancellationsYearR, aes(Year, Cancelled, colour = Reason)) +
  ggtitle("Cancelled Flights per year") +
  geom_line(size=2) +
  gghighlight(TRUE,  label_key = Reason) +
  theme_ipsum()+
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
    )
  ) +
  labs(title= '',
       y = 'Number of cancelled flights',
       x = 'Year') +
  scale_fill_discrete(name  ="Reason",
                      breaks=cncl_code,
                      labels=cncl_reason)

p4

# Plot
fly <- USAFlights2004 %>% 
  filter(Origin %in% top_origin & Dest %in% top_dest & UniqueCarrier %in% top_carrier) %>% 
  count(Origin, UniqueCarrier, Dest) %>% 
  mutate(Origin = fct_relevel(as.factor(Origin), c("ATL", "ORD", "DFW")))

alluvial(fly %>% select(-n),
         freq=fly$n, border=NA, alpha = 0.5,
         col=case_when(fly$Origin == "ATL" ~ "red",
                       fly$Origin == "ORD" ~ "blue",
                       TRUE ~ "orange"),
         cex=0.75,
         axis_labels = c("Origin", "Carrier", "Destination"),
         hide = fly$n < 150)


df_2 = sqlite_to_df("data_src/db.sqlite", query = "select * from flights where Year > 2003; group by Year")

