# Add path for the function library file
library_file_path = "C:\\Users\\dimitris\\Documents\\DataScience\\visualization\\Hands_on_project\\data_vis_team_project\\"
source(paste(library_file_path,"code.R",sep=""))
library(ggplot2)
install.packages("gganimate")
library(gganimate)
install.packages("babynames")
library(babynames)
library(hrbrthemes)
install.packages("viridis")
library("viridis")

dataset_path = "C:\\Users\\dimitris\\Documents\\DataScience\\visualization\\Hands_on_project\\Plane_data\\"

#dataset = read.table(paste(dataset_path,"2002.csv",sep = ""), header=T,sep = ",")

dataset = ingest_one_csv(paste(dataset_path,"2003.csv",sep = ""))
current_working_dir = "C:\\Users\\dimitris\\Documents\\DataScience\\visualization\\Hands_on_project\\data_vis_team_project\\"
setwd(current_working_dir)
source(paste(getwd(), "/base.R", sep = ""))

dataset = ingest_one_csv("2003.csv")

group_uc = group_by(dataset, UniqueCarrier)
mean_delay = summarize(group_uc, DepDelay = mean(DepDelay,na.rm =T),ArrDelay = mean(ArrDelay, na.rm = T))
x = melt(mean_delay, c("UniqueCarrier"), c("DepDelay","ArrDelay"))


theme_set(theme_classic())

# Plot
#g <- ggplot(mean_arr_delay, aes(UniqueCarrier, delay))
#g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
#  labs(title="Bar Chart", 
#       subtitle="Manufacturer of vehicles", 
#       caption="Source: Frequency of Manufacturers from 'mpg' dataset") +
#  theme(axis.text.x = element_text(angle=65, vjust=0.6))
#mean_arr_delay#


plot <- ggplot(x, aes(UniqueCarrier, value, fill=variable))
plot <- plot + geom_bar(stat = "identity", position = 'dodge')
plot

s <- ggplot(x, aes(x = UniqueCarrier, y = value))+
  geom_col(aes(fill = variable), width = 0.7)
s

boxplot(Distance~UniqueCarrier,data=dataset, main="Car Milage Data",
        xlab="Number of Cylinders", ylab="Miles Per Gallon")


violin <- ggplot(dataset, aes(x=UniqueCarrier, y=Distance,fill = UniqueCarrier)) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()
violin



group_planes = group_by(dataset,Month,UniqueCarrier)
car_month = summarize(group_planes, TotalDistance = sum(Distance, na.rm = T))


# Plot
car_month %>%
  ggplot( aes(x=factor(Month), y=TotalDistance, group=UniqueCarrier, color=UniqueCarrier)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Total Distance per Month by each Carrier") +
  theme_ipsum() +
  ylab("Total Distance") +
  scale_y_continuous(breaks =c(0e+00,2e+07,4e+07,6e+07),labels = c("0","20M", "40M", "60M"))+
  transition_reveal(Month)

anim_save("C:\\Users\\dimitris\\Documents\\DataScience\\visualization\\Hands_on_project\\data_vis_team_project\\animation_one.gif")
