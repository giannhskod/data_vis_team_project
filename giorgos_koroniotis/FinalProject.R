#####################################################################################
#  Final Project                                                                    # 
#  Student: Giorgos Koroniotis                                                      #
#  Student ID:                                                                      #
#  Email: gkoroniotis@aueb.gr                                                       #
#  Deadline: 17/12/2019                                                             #
#####################################################################################

##########################################################################
# Load Libraries
##########################################################################

# Load Libraries
library(readxl)
library(stringr)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(rbin)
library(shiny)
library(shinythemes)
library(viridis)
library(tidyverse)
library(kableExtra)
library(alluvial)
library(ggalluvial)

######################################################################
## Data Preprocessing ##
######################################################################
# Read CSV into R
USAFlights2004 <- read.csv(file="C:/Users/user/Documents/MasterofDataScience/DataVisualization_TEMP/Project_HandsOn/2004.csv", header=TRUE, sep=",")
USAFlights2005 <- read.csv(file="C:/Users/user/Documents/MasterofDataScience/DataVisualization_TEMP/Project_HandsOn/2005.csv", header=TRUE, sep=",")
USAFlights2006 <- read.csv(file="C:/Users/user/Documents/MasterofDataScience/DataVisualization_TEMP/Project_HandsOn/2006.csv", header=TRUE, sep=",")
USAFlights2007 <- read.csv(file="C:/Users/user/Documents/MasterofDataScience/DataVisualization_TEMP/Project_HandsOn/2007.csv", header=TRUE, sep=",")
USAFlights2008 <- read.csv(file="C:/Users/user/Documents/MasterofDataScience/DataVisualization_TEMP/Project_HandsOn/2008.csv", header=TRUE, sep=",")

# Bind all datasets to one.
USAFlights <- rbind(USAFlights2004,USAFlights2005,USAFlights2006,USAFlights2007,USAFlights2008)

TULFlights <- USAFlights [which(USAFlights$Origin == 'TUL' | USAFlights$Dest == 'TUL'  ), ]
MKEFlights <- USAFlights [which(USAFlights$Origin == 'MKE' | USAFlights$Dest == 'MKE'  ), ]

flights <- rbind(TULFlights,MKEFlights)

flights$airport <- case_when(flights$Origin == 'TUL' | flights$Dest == 'TUL' ~ 'TUL',
                             flights$Origin == 'MKE' | flights$Dest == 'MKE' ~ 'MKE')

flights$airportName <- case_when(flights$airport =='TUL' ~ "Tulsa International",
                                 flights$airport =='MKE' ~ "General Mitchell International")

CancelledFl <- flights[ which(flights$Cancelled == 1 ), ]

##################################################
## Plot 1 ## 
##################################################

# 
CancellationsMonth <- setNames(aggregate(CancelledFl$Cancelled, 
                                         by=list(CancelledFl$Month), 
                                         FUN=sum), c("Month","Cancelled"))
CancellationsMonth

# Plot  1
p1 <- barplot(CancellationsMonth$Cancelled, names.arg =  month.abb[CancellationsMonth$Month],
              main = 'Cancelled Flights per month for Tulsa International airport & \nGeneral Mitchell International airport (2004-2008)',
              xlab = "Month",
              ylab = 'Number of cancellations')
p1


#############################################################
# Plot 2 #
#############################################################

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
  labs(title= 'Cancelled Flights per month group by Reason Code (2004 - 2008)',
       y = 'Number of cancelled flights',
       x = 'Month') +
  scale_fill_discrete(name  ="Reason",
                      breaks=cncl_code,
                      labels=cncl_reason) 
p2


##############################################################
# Plot 3
##############################################################


#day_code <- c(1,2,3,4,5,6,7)
#day_desc <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")



CnclMonths <- setNames(aggregate(CancelledFl$Cancelled, by=list(CancelledFl$Month, CancelledFl$airport),FUN=sum), 
                     c("Month","Airport", "Cancelled"))
CnclMonths

CnclMonths$Day<- as.factor(CnclDays$Day)
levels(CnclDays$Day) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
CnclDays


p3 <- ggplot(data = CnclMonths, aes(x=CnclMonths$Month, y = CnclMonths$Cancelled, fill=CnclMonths$Airport)) + 
  geom_line(aes(colour = CnclMonths$Airport, group = CnclMonths$Airport),size=2) +
  scale_x_continuous("Months", labels=as.character( month.abb[CnclMonths$Month]), breaks=CnclMonths$Month ) +
  scale_y_continuous("Number of Cancelled flights", breaks = seq(0, 5000, by = 250)) +
  labs(title= 'Cancelled flights per month for both airports (2004 - 2008)',
       y = 'Number of Cancelled flights') +
  scale_color_discrete(name = "Airport", labels = c("General Mitchell International","Tulsa International") ) + 
  theme_bw()
p3


##############################################################
# Plot 4
##############################################################


#day_code <- c(1,2,3,4,5,6,7)
#day_desc <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")



CnclDays <- setNames(aggregate(CancelledFl$Cancelled, by=list(CancelledFl$DayOfWeek, CancelledFl$airport),FUN=sum), 
                           c("Day","Airport", "Cancelled"))
CnclDays

CnclDays$Day<- as.factor(CnclDays$Day)
levels(CnclDays$Day) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
CnclDays


p4 <- ggplot(data = CnclDays, aes(x=CnclDays$Day, y = CnclDays$Cancelled, fill=CnclDays$Airport)) + 
  geom_line(aes(colour = CnclDays$Airport, group = CnclDays$Airport),size=2) +
  scale_x_discrete("Days of Week",  breaks=CnclDays$Day) +
  labs(title= 'Cancelled flights per day for both airports (2004 - 2008)',
       y = 'Number of Cancelled flights') +
  scale_color_discrete(name = "Airport", labels = c("General Mitchell International","Tulsa International") ) + 
  theme_bw()
p4



###################################################################
## Plot 5
###################################################################

my_airport <-USAFlights[ which((USAFlights$Origin == 'TUL' | USAFlights$Origin == 'MKE') & USAFlights$Cancelled == 1 ), ]
my_airport

top_origin <- my_airport %>%
  count(Origin) %>% 
  top_n(2,n) %>% 
  pull(Origin)

top_dest <- my_airport %>% 
  count(Dest) %>% 
  top_n(3, n) %>% 
  pull(Dest)

top_carrier <- my_airport %>% 
  filter(Origin %in% top_origin) %>% 
  filter(Dest %in% top_dest) %>% 
  count(UniqueCarrier) %>% 
  top_n(4, n) %>% 
  pull(UniqueCarrier)


fly <- my_airport %>% 
  filter(Origin %in% top_origin & Dest %in% top_dest & UniqueCarrier %in% top_carrier) %>% 
  count(Origin, UniqueCarrier, Dest) %>% 
  mutate(Origin = fct_relevel(as.factor(Origin), c("TUL", "MKE")))

alluvial(fly %>% select(-n),
         freq=fly$n, border=NA, alpha = 0.5,
         col=case_when(fly$Origin == "MKE" ~ "red",
                       fly$Origin == "TUL" ~ "blue"),
         cex=0.75,
         axis_labels = c("Origin", "Carrier", "Destination"),
         hide = fly$n < 1)
mtext("Top 3 Cancelled Destinations including top Carriers", 3, line=3, font=2)



