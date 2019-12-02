#####################################################################################
#  Hands-on Project                                                                 # 
#  Student: Giorgos Koroniotis                                                      #
#  Student ID:                                                                      #
#  Email: gkoroniotis@aueb.gr                                                       #
#  Deadline: 18/11/2019                                                             #
#####################################################################################

##########################################################################
# 01. Updload the xlsx file to R & Data Preprocessing
##########################################################################
current_working_dir = "C:/Users/user/Documents/MasterofDataScience/DataVisualization_TEMP/data_vis_team_proj/"
setwd(current_working_dir)
source(paste(getwd(), "/base.R", sep = ""))


# Read CSV into R
USAFlights2002 = ingest_one_csv("2002.csv")
USAFlights2003 = ingest_one_csv("2003.csv")
USAFlights2004 = ingest_one_csv("2004.csv")
USAFlights2005 = ingest_one_csv("2005.csv")
USAFlights2006 = ingest_one_csv("2006.csv")
USAFlights2007 = ingest_one_csv("2007.csv")
USAFlights2008 = ingest_one_csv("2008.csv")

# Bind all datasets to one.
USAFlights <- rbind(USAFlights2002,USAFlights2003,USAFlights2004,USAFlights2005,USAFlights2006,USAFlights2007,USAFlights2008)


# -------------------------------------------------------------------------------------
# Number of cancellations per month for a random year (i,e. 2004)
CancellationsMonth <- setNames(aggregate(USAFlights2004$Cancelled, 
                                         by=list(USAFlights2004$Month), 
                                         FUN=sum), c("Month","Cancelled"))
CancellationsMonth

# Plot  1
p1 <- barplot(CancellationsMonth$Cancelled, names.arg =  month.abb[CancellationsMonth$Month],
              main = 'Cancelled Flights per month in 2004',
              xlab = "Month",
              ylab = 'Number of cancellations')
p1


# -------------------------------------------------------------------------------------
# Number of cancellations per month and per reason for a random year (i,e. 2004)

CancelledFl <- USAFlights2004[ which(USAFlights2004$Cancelled == 1 ), ]
CancelledFl

CancellationsMonthR <- setNames(aggregate(CancelledFl$Cancelled, 
                                          by=list(CancelledFl$Month, CancelledFl$CancellationCode), 
                                          FUN=sum), c("Month","Reason", "Cancelled"))
CancellationsMonthR

cncl_code <- c("A","B", "C", "D")
cncl_reason <- c("Carrier", "Weather", "NAS", "Security") 
factor(cncl_code,labels=cncl_reason)

# Plot  2
p2 <- ggplot(CancellationsMonthR, aes(fill=Reason, y=Cancelled, x=Month)) +
  geom_bar(position="stack", stat="identity") + 
  scale_x_continuous("Month", labels = as.character( month.abb[CancellationsMonthR$Month]), breaks=CancellationsMonthR$Month) +
  labs(title= 'Cancelled Flights in 2004 per month group by Reason Code',
       y = 'Number of cancelled flights',
       x = 'Month') +
  scale_fill_discrete(name  ="Reason",
                      breaks=cncl_code,
                      labels=cncl_reason)
p2

# -------------------------------------------------------------------------------------
# Frequency of flights in 2004 per month and per weekday

Fl <- USAFlights2004[ which(USAFlights2004$Cancelled == 0 ), ]
Fl 

day_code <- c(1,2,3,4,5,6,7)
day_desc <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

FlCounts <- table(Fl$Month, Fl$DayOfWeek)
FlCounts


p3 <- ggplot(data = as.data.frame(FlCounts), aes(x=Var1, y = Freq, fill=Var2)) + 
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete("Month", labels=as.character( month.abb[Fl$Month]), breaks=Fl$Month) +
  labs(title= 'Flights in 2004 per month group by Day of Week',
       y = 'Number of flights',
       x = 'Month') +
  scale_fill_discrete(name  ="Week Day",
                      breaks = day_code,
                      labels = day_desc)
p3

# -------------------------------------------------------------------------------------
# Present the most frequent origins, destinations, carriers in a plot
# Plot 4


top_origin <- USAFlights2004 %>%
  count(Origin) %>% 
  top_n(3,n) %>% 
  pull(Origin)

top_dest <- USAFlights2004 %>% 
  count(Dest) %>% 
  top_n(5, n) %>% 
  pull(Dest)

top_carrier <- USAFlights2004 %>% 
  filter(Origin %in% top_origin) %>% 
  filter(Dest %in% top_dest) %>% 
  count(UniqueCarrier) %>% 
  top_n(4, n) %>% 
  pull(UniqueCarrier)


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

