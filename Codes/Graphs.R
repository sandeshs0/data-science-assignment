library(tidyverse)
library(ggplot2)
library(fmsb)
library(scales)

#Housing 
housing_data=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/housing_cleaned.csv")

#For Average House Price of 2023 for Bristol and Cornwall

housing_2023<- housing_data %>% 
  filter(Year==2023) %>% 
  group_by(County) %>% 
  summarise(average_house_price_2023=mean(Price))

colnames(housing_data)

#Avaeage house price for each county for 2020-2023
housing_20_23=housing_data %>% 
  filter(Year>=2020) %>% 
  group_by(Year,County) %>% 
  summarise(avg_house_price=mean(Price))

head(housing_2023)
head(housing_20_23)

#BarChart
ggplot(housing_2023,aes(x=County,y=average_house_price_2023,fill=County))+
  geom_bar(stat="identity")+
  ggtitle("Average Housing Price in 2023")+
  ylab("Average Price")+
  xlab("County")+
  theme_minimal()

#Box Plot
ggplot(housing_data %>% 
         filter(Year==2023),aes(x=County,y=Price,fill=County))+
  geom_boxplot()+
  ggtitle("Average Housing Price in Year 2023")+
  ylab("Price")+
  xlab("County")

#Line Chart
housing_20to23 <- housing_data %>% 
  filter(Year >= 2020 & Year <= 2023)
average_per_year <- housing_20to23 %>% 
  group_by(Year, County) %>% 
  summarise(Average_Price = mean(Price))

ggplot(average_price_per_year, aes(x = Year, y = Average_Price, color = County)) +
  geom_line() +
  geom_point() +
  ggtitle("Line Chart of Average House Price from 2020 to 2023") +
  xlab("Year") +
  ylab("Average Price")

#housing by years
housing_by_year = housing_data %>%
  filter(Year >= 2020) %>%
  group_by(Year, Town_City) %>%
  summarise(avg_price = mean(Price))

# Filtering data for 2023
housing_data_2023 = housing_by_year %>%
  filter(Year == 2023)

ggplot(housing_data_2023, aes(x = Town_City, y = avg_price, fill = Town_City)) +
  geom_bar(stat = "identity") +
  ggtitle("Average House Price by Town in 2023") +
  ylab("Average Price") +
  xlab("City") +
  theme_minimal()

#-----------Broadband Data
broadband=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/broadband_cleaned.csv")

colnames(broadband)
View(broadband)
#Average download speed
ggplot(broadband, aes(x = County, y = AvgDownSpeed, fill = County)) +
  geom_boxplot() +
  labs(title = "Average Download Speed by County",
       x = "County",
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal()

#Average and maximum speed for cornwall
cornwall_speed <-broadband %>% 
  filter(County=="CORNWALL") %>% 
  group_by(Town_City) %>% 
  summarize(
    AvgDownSpeed=mean(AvgDownSpeed),
    MaxDownSpeed=max(MaxDownSpeed)
  ) %>% 
  pivot_longer(cols=c(AvgDownSpeed,MaxDownSpeed),names_to="SpeedType",values_to = "Speed")


View(cornwall_speed)

#Visualization
ggplot(cornwall_speed, aes(x = Town_City, y = Speed, fill = SpeedType)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average and Maximum Download Speeds by Town/City in Cornwall",
       x = "Town/City",
       y = "Speed (Mbit/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top")

#Average and maximum speed for Bistrol
bistrol_speed <-broadband %>% 
  filter(County=="CITY OF BRISTOL") %>% 
  group_by(Town_City) %>% 
  summarize(
    AvgDownSpeed=mean(AvgDownSpeed),
    MaxDownSpeed=max(MaxDownSpeed)
  ) %>% 
  pivot_longer(cols=c(AvgDownSpeed,MaxDownSpeed),names_to="SpeedType",values_to = "Speed")

View(bistrol_speed)

#Visualization
ggplot(bistrol_speed, aes(x = Town_City, y = Speed, fill = SpeedType)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average and Maximum Download Speeds by Town/City in Bistrol",
       x = "Town/City",
       y = "Speed (Mbit/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top")

#---------------------CRIME--------------------------------------------
crime_data = read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/crime_cleaned.csv")
View(crime_data)
colnames(crime_data)


# Vehicle Crime per 10000 people in April 2022
vehicleCrime <- crime_data %>%
  filter(`Crime type` == "Vehicle crime")
colnames(vehicleCrime)

vehicle_sum <- vehicleCrime %>%
  group_by(Year) %>%
  summarise(total_crime = sum(population, na.rm = TRUE))

View(vehicle_sum)

for_radar = as.data.frame(t(vehicle_sum$total_crime))
colnames(for_radar) <- vehicle_sum$Year
view(for_radar)

for_radar <- rbind(rep(max(vehicle_sum$total_crime), length(years)), 
                   rep(0, length(years)), 
                   for_radar)

par(mar = c(2, 2, 2, 2))

#Plotting Radar Chart for Vehicle Crime
radarchart(for_radar,
           axistype = 1,
           pcol = "purple",
           pfcol = "lightblue",
           plwd = 4,
           title = "Vehicle Crime Rate from 2021 to 2024"
)


#Pie Chart for Robbery in 2023 by month

robbery_data =crime_data %>% 
  filter(`Crime type`=="Robbery" & Year=="2023") %>% 
  group_by(Month) %>% 
  summarise(count=n()) %>% 
  mutate(percentage=count/sum(count)*100)

#Plotting the Pie Chart
ggplot(robbery_data,aes(x = "", y = percentage, fill = as.factor(Month))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Robberies by Month in 2023", fill = "Month") +
  theme_minimal()

#_____________________Schools

school_data= read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/schoolcleaned.csv")

school_filtered= school_data %>% 
  filter(YEAR==2022)

#BOXPLOT: Average Attainment 8 scores by County in 2022
ggplot(school_filtered,aes(x = COUNTY, y = ATT8SCR, fill = COUNTY)) +
  geom_boxplot() +
  labs(title = "Average Attainment 8 Scores by County in 2022",
       x = "County",
       y = "Attainment 8 Score") +
  theme_minimal()

#Line Graph : Average Attainment 8 Score in Academic Year 2021-2022 in BISTROL
bristol_21=school_data %>% 
  filter(YEAR==2021) %>% 
  filter(COUNTY=="Bristol")

ggplot(bristol_21, aes(x = SCHNAME, y = ATT8SCR, group = 1)) +
  geom_line(color = "purple") +
  geom_point() +
  labs(title = "Average Attainment 8 Score in 2021-2022 in Bristol",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Line Graph : Average Attainment 8 Score in Academic Year 2021-2022 in CORNWALL
cornwall_21=school_data %>% 
  filter(YEAR==2021) %>% 
  filter(COUNTY=="Cornwall")

ggplot(cornwall_21, aes(x = SCHNAME, y = ATT8SCR, group = 1)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Average Attainment 8 Score in 2021-2022 in Cornwall",
       x = "School Name",
       y = "Average Attainment 8 Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


