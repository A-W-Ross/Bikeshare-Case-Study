---
title: "Coursera Bikeshare Casestudy"
author: "Anthony"
date: "2025-04-06"
output: html_document
---
### The following script contains the code used for cleaning and analysis of data from
### the Google Data Analytics Course on Coursera. The main goal of this  project is to 
### utilize 12 months of data  (January-December, 2024) to provide business insights to 
### Cysclistic, a fictional bikeshare company, on how they might convert  casual users of
### the service to annual members. 
### Though the company is fictional, the data is real data provided byy Divvy, and has 
### had all personal information removed.


### Setting Up Environment

```{r setup, echo=FALSE, message=FALSE}
install.packages("tidyverse")
library(tidyverse)
install.packages("conflicted")
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
```


### Importing raw data from .csv files

```{r, message=FALSE}
Jan_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202401-divvy-tripdata.csv")
Feb_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202402-divvy-tripdata.csv")
Mar_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202403-divvy-tripdata.csv")
Apr_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202404-divvy-tripdata.csv")
May_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202405-divvy-tripdata.csv")
Jun_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202406-divvy-tripdata.csv")
Jul_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202407-divvy-tripdata.csv")
Aug_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202408-divvy-tripdata.csv")
Sep_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202409-divvy-tripdata.csv")
Oct_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202410-divvy-tripdata.csv")
Nov_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202411-divvy-tripdata.csv")
Dec_2024 <- read_csv("C:/Users/18046/OneDrive/Desktop/Coursera - Anthony/bikeshare_case_study/202412-divvy-tripdata.csv")
```


### Checking that all column names are the same

```{r, echo=FALSE, message=FALSE }
colnames(Jan_2024)
colnames(Feb_2024)
colnames(Mar_2024)
colnames(Apr_2024)
colnames(May_2024)
colnames(Jun_2024)
colnames(Jul_2024)
colnames(Aug_2024)
colnames(Sep_2024)
colnames(Oct_2024)
colnames(Nov_2024)
colnames(Dec_2024)
```

### Combie Data

```{r, message=FALSE}
all_trips <- bind_rows(Jan_2024, Feb_2024, Mar_2024, Apr_2024, May_2024, Jun_2024,
                       Jul_2024, Aug_2024,
                       Sep_2024, Oct_2024, Nov_2024,  Dec_2024)
```

### Remove placeholder datasets

```{r}
remove(Jan_2024, Feb_2024, Mar_2024, Apr_2024, May_2024, Jun_2024,
       Jul_2024, Aug_2024, Sep_2024, Oct_2024, Nov_2024, Dec_2024)
```

### Splitting up date and time to make data easier to us and understand

```{r, message=FALSE}
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$trip_duration <- difftime(all_trips$ended_at, all_trips$started_at)
all_trips$trip_duration <- as.numeric(as.character(all_trips$trip_duration))
```


### Creating new table excluding all rows where trip duration is less than zero seconds or greater than one day

```{r, message=FALSE}
all_trips_v2 <- all_trips[!(all_trips$trip_duration < 0) & !(all_trips$trip_duration > 86400),]
```

```{r, include=FALSE}
remove(all_trips)
```

### Removing data that won't be used

```{r, message=FALSE}
all_trips_v3 <- all_trips_v2 %>% 
  select(-c(start_station_id, end_station_id))
```

```{r, include=FALSE} 
remove(all_trips_v2)
```

### Converting average duration to minutes

```{r, message=FALSE}
all_trips_v3 %>% 
  mutate(trip_duration_m =  trip_duration/60)
```
### Removing 'null' values

```{r, message=FALSE}
 all_trips_v4 <- na.omit(all_trips_v3)
```

```{r, include=FALSE}
remove(all_trips_v3)
```

### Plot total number of rides per  member type

```{r, echo=FALSE}
all_trips_v4 %>%
  group_by(member_casual) %>% 
  summarise(number_of_rides = n())%>% 
  ggplot(aes(x="", y=number_of_rides, fill=member_casual))+
  geom_col(color="black")+
  coord_polar(theta = "y")+
  labs(title = "Total Rides per Member Type",
       x = " ",
       y = " ",
       fill = "User Type")
```


### Plot Average Ride Duration per Day of the Week

```{r, echo=FALSE}
all_trips_v4 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual))+
  geom_col(position = "dodge")+
  labs(title = "Average Ride Duration per  Day of the Week",
       x = "Day of Week",
       y = "Average Duration",
       fill = "User Type")
```

### Plot Number of Rides per Day of the Week

```{r, echo=FALSE}
all_trips_v4 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual))+
  geom_col(position = "dodge")+
  labs(title = "Rides per Day of the Week",
       x = "Day of Week",
       y = "# of Rides",
       fill = "User Type")
```

### Plot Number of Rides per Month

```{r, echo=FALSE}
all_trips_v4 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=number_of_rides, fill=member_casual))+
  geom_col(position = "dodge")+
  labs(title  =  "Rides per Month",
       x = "Month",
       y = "# of Rides",
       fill = "User Type")
```
