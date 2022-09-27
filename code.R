# lets load the relevent packages
# Prepare Phase
#install.packages('skimr')
#install.packages('tidyverse')
library(tidyverse)
dataset1=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\1\\202208-divvy-tripdata.csv")


# dataset2=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\2\\202207-divvy-tripdata.csv")
# dataset3=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\3\\202206-divvy-tripdata.csv")
# dataset4=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\4\\202205-divvy-tripdata.csv")
# dataset5=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\5\\202204-divvy-tripdata.csv")
# dataset6=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\6\\202203-divvy-tripdata.csv")
# dataset7=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\7\\202202-divvy-tripdata.csv")
# dataset8=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\8\\202201-divvy-tripdata.csv")
# dataset9=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\9\\202112-divvy-tripdata.csv")
# dataset10=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\10\\202111-divvy-tripdata.csv")
# dataset11=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\11\\202110-divvy-tripdata.csv")
# dataset12=read.csv("C:\\Users\\500053108\\Documents\\Google Data  Analytic Case Study\\Dataset\\12\\202109-divvy-tripdata.csv")

# str(dataset1)
# str(dataset2)
# str(dataset3)
# str(dataset4)
# str(dataset5)
# str(dataset6)
# str(dataset7)
# str(dataset8)
# str(dataset9)
# str(dataset10)
# str(dataset11)
# str(dataset12)


##Now we will combine all datasets into one dataset


# dataset=bind_rows(dataset1,
#                   dataset2,
#                   dataset3,
#                   dataset4,
#                   dataset5,
#                   dataset6,
#                   dataset7,
#                   dataset8,
#                   dataset9,
#                   dataset10,
#                   dataset11,
#                   dataset12)

##Lets Describe our dataset
glimpse(dataset1)
#str(dataset)
colnames(dataset1)
head(dataset1)
## renaming member_casual to membership
dataset1=rename(dataset1,membership=member_casual)
# 2*Process Phase
## Feature ride_id
## * Since this is a unique_id for each observation it must have a constant pattern 
num_of_char_in_ride_id=c(str_length(dataset1$ride_id))

library(skimr)
skim_without_charts(num_of_char_in_ride_id)
summary(num_of_char_in_ride_id)
## No missing values and all the observation in ride_id are 16 character lenght

## * checking dimention of our dataset
dim(dataset1)

## * lets check all values of categorical(nominal) variable rideable_type
table(dataset1$rideable_type)

## * let check if membership_status entries have typos
table(dataset1$membership)

## * lets convert date strings to date and making a consistent formating to changes to YYYY-MM-DD HH:MM:SS  
library(lubridate)
dataset1$started_at <- ymd_hms(dataset1$started_at)                
dataset1$ended_at <- ymd_hms(dataset1$ended_at)

## * dropping all rows with null observations
dim(dataset1)
dataset1 <- drop_na(dataset1)
dim(dataset1)

## * creating features that list the date, month, day, and year of each ride
dataset1$date <- as.Date(dataset1$started_at)                      #The default format is yyyy-mm-dd
dataset1$month <- format(as.Date(dataset1$date), "%m")             #stores month
dataset1$day <- format(as.Date(dataset1$date), "%d")               #stores the day
dataset1$year <- format(as.Date(dataset1$date), "%Y")              #stores the year
dataset1$day_of_week <- format(as.Date(dataset1$date), "%A")       #stores the day of the week

## finding time length of ride(in seconds) to check if there are any negatives and for future analysis need
dataset1$length_of_ride <- difftime(dataset1$ended_at,dataset1$started_at)
glimpse(dataset1)

##changing to a numeric format for using it in analysis
dataset1$length_of_ride <- as.numeric(as.character(dataset1$length_of_ride)) 
is.numeric(dataset1$length_of_ride)

# deleting extra df

# #rm(dataset1)
# rm(dataset2)
# rm(dataset3)
# rm(dataset4)
# rm(dataset5)
# rm(dataset6)
# rm(dataset7)
# rm(dataset8)
# rm(dataset9)
# rm(dataset10)
# rm(dataset11)
# rm(dataset12)
# rm(num_of_char_in_ride_id)



## lets check the distribution of length_of_ride
ggplot(dataset1, aes(y=length_of_ride)) +
  geom_boxplot(outlier.color = "black",outlier.shape=16, outlier.size=2, notch=FALSE)
## density plot
ggplot(dataset1, aes(x=length_of_ride)) +
    geom_density()
## removing outliers observations where lenth of ride is less than 0 or greater than 1 day
clean_data <- dataset1%>% filter(!(length_of_ride<=0 | length_of_ride>86400))
#rm(dataset)
## observations left
dim(clean_data)
## * lets check for duplicate observations
sum(duplicated(clean_data$ride_id)) ## bad code

##checking to see if we have any duplicate ride_ids
temp <- clean_data[duplicated(clean_data$ride_id),]
temp %>% 
  print(n=40)

#---------------------------------------------------------------------------------------
clean_data %>% 
  ggplot(mapping=aes(x=length_of_ride )) +geom_histogram(aes(y=..density..,fill=membership),binwidth = 500,color="black")+
  geom_density(alpha=.2,fill="#FF6666")+
  labs(title="Distribution of Casual and Members customers accross Length of ride attribute ",
       subtitle="This graph explains which group between casual and members have larger length of ride",
       caption="OCT 2021- SEPT 2022")




ggplot(clean_data, aes(x=length_of_ride)) +
  geom_density()

ggplot(clean_data, aes(y=length_of_ride)) +
  geom_boxplot(outlier.color = "red",outlier.shape=16, outlier.size=2, notch=FALSE)

# ride_distance
install.packages("geosphere")                 ## helps in finding distance between geographic coordinates
library(geosphere)

##Adding column ride_distance for finding dist. between coordinates with column headers start_lng, start_lat, end_lang, end_lat
clean_data$ride_distance <- distGeo(matrix(c(clean_data$start_lng, clean_data$start_lat), ncol = 2), matrix(c(clean_data$end_lng, clean_data$end_lat), ncol = 2))
summary(clean_data)

# *4 Analyze Phase
colnames(clean_data)

str(clean_data)

## Univariate analysis
### length_of_ride
clean_data %>% 
  ggplot(mapping=aes(x=length_of_ride))+geom_histogram(aes(y=..density..),binwidth = 500,color="black",fill="white")+
  geom_density(alpha=.2,fill="#FF6666")

### ride_distance
clean_data %>% 
  ggplot(mapping=aes(x=ride_distance))+geom_histogram(aes(y=..density..),binwidth = 500,color="black",fill="white")+
  geom_density(alpha=.2,fill="#FF6666")


## Bivariate analysis
  
  table(clean_data$day_of_week)
  
  ##for arranging, we need to input the levels for months, days as our analysis results will be arranged by months and days
  clean_data$day_of_week <- 
    ordered(clean_data$day_of_week, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
  
  ##above, arrange function will arrange days by week order
  
  ##for analysing number of rides through each day of week in a summary for both casual riders and members
  
  clean_data %>%
    group_by(membership, day_of_week) %>%
    summarise(number_of_ride = n(), .groups = 'drop') %>%
    arrange(day_of_week)%>%
    print(n=30) %>% 
    ggplot()+geom_col(mapping = aes(x=day_of_week,y=number_of_ride,fill=membership))+
    labs(title="Distribution of Casual and Members customers accross week days ",
         subtitle="This graph explains the distribution of casual and members customers over different days of the week",caption="OCT 2021- SEPT 2022")
  
  ##we again iput levels for months like we did for days, here 12 referes to month of previous year
  clean_data$month <-
    ordered(clean_data$month, levels = c('12', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11'))
  
  
  ##for analysing number of rides for every month in a summary for both casual riders and members
  ##here, I wanted to see all rows of table instead of a tibble( shows 10 rows), so I took values in a temporary table
  clean_data %>%
    group_by(membership, month) %>%
    summarise(number_of_ride = n(), .groups = 'drop') %>%
    arrange(month) %>%
    print(n=30) %>% 
    ggplot()+geom_col(mapping = aes(x=month,y=number_of_ride,fill=membership))
  
  
##aggregating for length of ride, for each week for both casual riders and members
  aggregate(clean_data$length_of_ride ~ clean_data$membership + clean_data$day_of_week, FUN=mean)
  
  ##for analysing AVERAGE TIME LENGTH of rides through each day of week in a summary for both casual riders and members
  ##using a temporary table to see all rows
  clean_data %>%
    group_by(membership, month) %>%
    summarise(average_ride_length = mean(length_of_ride), .groups = 'drop') %>%
    arrange(month)%>%
    print(n=30) %>% 
    ggplot()+geom_col(mapping = aes(x=month,y=average_ride_length,fill=membership))
  
  ##for analysing AVERAGE ride distance through each day of week in a summary for both casual riders and members
  clean_data %>%
    group_by(membership, day_of_week) %>%
    summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
    arrange(day_of_week)%>%
    print(n=30) %>% 
    ggplot()+geom_col(mapping = aes(x=day_of_week,y=distance_of_ride,fill=membership))
  
  ##for analysing AVERAGE ride distance for each month in a summary for both casual riders and members
  ##using temporary table 
  clean_data %>%
    group_by(membership, month) %>%
    summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
    arrange(month)%>%
    print(n=30) %>% 
    ggplot()+geom_col(mapping = aes(x=month,y=distance_of_ride,fill=membership))
  
  ##for seeing how many rides were taken by which member type 
  clean_data %>%
    group_by(membership) %>%
    summarize(number_of_rides = n() , .groups = 'drop') %>% 
    ggplot()+geom_col(mapping = aes(x=membership,y=number_of_rides,fill=membership))
  
  
  ##for seeing how long each type of bike was used for
  clean_data %>%
    group_by(rideable_type) %>%
    summarise(distance_of_ride = mean(ride_distance), .groups = 'drop') %>%
    arrange(rideable_type)%>%
    print(n=30) %>% 
    ggplot()+geom_col(mapping = aes(x=rideable_type,y=distance_of_ride,fill=rideable_type))
  ##electric_bike was used for longer distances
  
  