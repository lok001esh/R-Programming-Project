#Lokesh Bhatta
#NP000526
#Weather Analysis of 'hourly_weather_data.csv'

#install packages

install.packages("ggplot2") #include ggplot function
install.packages("dplyr") #include filter function
install.packages("RColorBrewer")
install.packages("ggthemes")

#Import Data
data<-read.csv("C:\\Users\\Dell\\OneDrive - Lord Buddha Education Foundation\\Desktop\\R_Final\\hourly_weather_data.csv ")
data

#Load Data
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggthemes)



#Summary Data
summary(data)



#Analysis 1
#Temperature differences between JFK and LGA Airport throughout the year.


library(ggplot2)

ggplot(data=data, mapping=aes(x=month, y=temp, colour=origin)) +
  aes(x = month, y = temp, colour = origin) +
  geom_jitter(size = 1.5) +
  scale_x_continuous(breaks=c(1:12, 1))+
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Month", y = "Temperature", title = "Temperature Differences",
       color = "Airport") +
  theme_classic() +
  theme(plot.title = element_text(size = 15L, face = "bold",
                                  hjust = 0.5)) +
  facet_wrap(vars(origin))



#Analysis 2

#Precipitation Levels at JFK and LGA Airport throughout the year.


ggplot(data=data, mapping=aes(x=month, y-precip, colour=origin)) +
  aes(x = month, y = precip, colour = origin) +
  geom_line(size = 0.5) +
  scale_x_continuous(breaks=c(1:12, 1))+
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Month", y = "Precipitation", title = "Precipitation Throughout The Year",
       color = "Airport") +
  theme_classic() +
  theme(plot.title = element_text(size = 15L, face = "bold",
                                  hjust = 0.5))


#Analysis 3
#Pressure patterns throughout the year


ggplot(data=data, mapping=aes(x=month, y=pressure, colour=origin)) +
  aes(x = month, y = pressure, colour = origin) +
  geom_jitter(size = 0.5) +
  scale_x_continuous(breaks=c(1:12, 1))+
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Month", y = "Pressure", title = "Pressure Patterns Throughout The Year",
       color = "Airport") +
  theme_classic() +
  theme(plot.title = element_text(size = 15L, face = "bold",
                                  hjust = 0.5))


#Analysis 4
#Humidity


ggplot(data=data, mapping=aes(x=humid, fill=origin)) +
  aes(x = humid, fill = origin) +
  geom_histogram(bins = 50L) +
  scale_fill_brewer(palette = "Set1",
                    direction = 1) +
  labs(x = "Humidity", y = "Count", title = "Histogram of Humidity", fill = "Airport") +
  theme_classic() +
  theme(plot.title = element_text(size = 10L, face = "bold", hjust = 0.5))


#Analysis 5
#Wind Speed


ggplot(data=data, mapping=aes(x=wind_speed, fill=origin)) +
  aes(x = wind_speed, fill = origin) +
  geom_histogram(bins = 30L) +
  scale_fill_brewer(palette = "Dark2",
                    direction = 1) +
  labs(x = "Wind Speed", y = "Count", title = "Histogram of Wind Speed", fill = "Airport") +
  theme_classic() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5)) +
  facet_wrap(vars(origin))


#Analysis 6
#Temperature affecting dew point


ggplot(data=data, mapping=aes(x=temp, y=dewp, colour=origin)) +
  aes(x = temp, y = dewp, colour = origin) +
  geom_point(shape = "circle",
             size = 1.5) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Temperature", y = "Dewpoint",
       title = "Temperature Affecting Dewpoint", color = "Airport") +
  theme_classic() +
  theme(plot.title = element_text(size = 15L,
                                  face = "bold", hjust = 0.5))


#Analysis 7
#Monthly wind gust patterns


ggplot(data=data, mapping=aes(x=day, y=wind_gust, colour=origin)) +
  aes(x = day, y = wind_gust, colour = origin) +
  geom_point(shape = "circle",
             size = 1.5) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Day", y = "Wind Gust",
       title = "Wind Gust Patterns for Every Month", color = "Airport") +
  theme_classic() +
  theme(plot.title = element_text(size = 15L,
                                  face = "bold", hjust = 0.5)) +
  facet_wrap(vars(month))



#Analysis 8
#Visibility Throughout the Year


ggplot(data=data, mapping = aes(x = "", y = visib, fill = origin) ) +
  aes(x = "", y = visib, fill = origin) +
  geom_violin(adjust = 20L, scale = "area") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Count", y = "Visibility", title = "Visibility Throughout the Year",
       fill = "Airport") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, face = "bold",hjust = 0.5)) +
  facet_wrap(vars(month))




#Analysis 9
#Dew point throughout the year for each day


ggplot(data=data, mapping=aes(x=month, y=day, colour=origin, size=dewp)) +
  aes(x = month, y = day, colour = origin, size = dewp) +
  geom_point(shape = "circle") +
  scale_x_continuous(breaks=c(1:12, 1))+
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Month", y = "Days in the Month",
       title = "Dewpoint Throughout The Year For Each Day", color = "Airport", size = "Dewpoint") +
  theme_classic() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5)) +
  facet_wrap(vars(origin))



#Analysis 10
#Mean of Wind Speed at each month (Data exploration and manipulation)

library(ggplot2)
aggregate(wind_speed~month, data, mean)

data%>%ggplot(aes(x = month, y = wind_speed))+
  scale_x_continuous(breaks=c(1:12, 1))+
  geom_point(color="brown") + geom_smooth(method="lm", color="green")
labs(x = "Month", y = "Wind Speed",
     title = "Wind Speed of Each Month", color = "Airport")




#Analysis 11
#Relationship between humidity and visibility (Data Manipulation; use of dplyr)

data%>%ggplot(aes(x=humid, y=visib))+
  facet_wrap(~month)+
  labs(title="Relationship Between Humidity and Visibility For Each Month", x="Humidity", y="Visibility")+
  geom_point(color="green")+ geom_smooth(color="Brown")
cor(x=data$humid, y=data$visib, use="complete.obs")





#Analysis 12
#Relationship Between Wind Direction and Wind Speed


ggplot(data=data, mapping=aes(x = wind_dir, y = wind_speed, fill = origin, colour = origin)) +
  aes(x = wind_dir, y = wind_speed, fill = origin, colour = origin) +
  geom_line(size = 0.5) +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  scale_color_brewer(palette = "Dark2",
                     direction = 1) +
  labs(x = "Wind Direction", y = "Wind Speed", title = "Relationship Between Wind Direction and Wind Speed",
       color = "Airport") +
  theme_classic() +
  theme(plot.title = element_text(size = 15L, face = "bold",
                                  hjust = 0.5))


#Analysis 13
#Analysis on how temperature affects pressure


ggplot(data=data, mapping =aes(x = temp, y = pressure, fill = origin) ) +
  aes(x = temp, y = pressure, fill = origin) +
  geom_tile(size = 0.5) +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Temperature", y = "Pressure", title = "Temperature Affecting Pressure",
       fill = "Airport") +
  theme_classic() +
  theme(plot.title = element_text(size = 15L, face = "bold",
                                  hjust = 0.5))




#Analysis 14
#Relationship Between Humidity and Visibility


ggplot(data=data, mapping = aes(x = humid, y = visib, colour = origin)) +
  aes(x = humid, y = visib, colour = origin) +
  geom_area(size = 1.5) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Humidity", y = "Visibility", title = "Relationship Between Humidity and Visibility",
       color = "Airport") +
  theme_classic() +
  theme(plot.title = element_text(size = 15L, face = "bold",
                                  hjust = 0.5))



#Extra Feature 1
#Analysis of Pressure in LGA (Data Manipulation and Data Exploration)

data%>%filter(origin=="LGA")%>%ggplot(aes(x=pressure))+
  geom_histogram(bins=30)+
  facet_wrap(~month)+
  labs(title="Histogram of Pressure in LGA Throughout the Year", x="Pressure", y="Frequency")
data.frame(y=data[["data"]], x=pressure[["data"]][[1]][["x"]])
data%>%summarise(Origin="LGA", MaxPressure=max(pressure, na.rm = TRUE), MinPressure=min(pressure, na.rm = TRUE),
                 MeanPressure=mean(pressure,na.rm = TRUE))



#Extra Feature 2
#Wind Direction


data%>%ggplot(aes(wind_dir))+
  geom_bar(color="green", fill="white")+
  coord_polar(theta="x", direction=1)+
  scale_x_continuous(breaks=c(0,90,180,270))+
  labs(title="Wind Direction",x="South", y="Frequency")
