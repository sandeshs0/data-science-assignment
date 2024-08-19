library(tidyverse)
library(ggplot2)

#Importing Datasets
crimeData=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/crime_cleaned.csv")
schoolData=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/schoolcleaned.csv")
housingData=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/housing_cleaned.csv")
broadbandData=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/broadband_cleaned.csv")                     

#Linear Model for : Housing Price vs Average Download Speed
housingPrice <- housingData %>% 
  select(Postcode,Price)

downSpeed <- broadbandData %>% 
  select(Postcode,AvgDownSpeed)

housing_speed <- merge(housingPrice, downSpeed, by="Postcode")

View(housing_speed)  

ggplot(data = housing_speed, aes(x = Price, y = AvgDownSpeed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "House Prices vs Average Download Speed", 
       x = "House Price", 
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal()

#Linear Model for Housing Price Vs Drug Offence
housingPrice23=housingData %>% 
  filter(Year==2023) %>%
  mutate(Postcode=str_trim((substring(Postcode,1,6)))) %>% 
  distinct()

drug_offence_23 <-crimeData %>% 
  filter(Year==2023, `Crime type`=="Drugs") %>% 
  group_by(postcode) %>% 
  summarise(DrugRate=sum(population)) %>% 
  distinct() %>% 
  na.omit()

housing_drugs <- housingPrice23 %>% 
  left_join(drug_offence_23,by=c("Postcode"="postcode")) %>% 
  distinct() %>% 
  na.omit()

ggplot(data = housing_drugs, aes(x = DrugRate, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Housing Price vs Drug Rates in 2023", 
       x = "Drug Rates", 
       y = "House Price") +
  theme_minimal()
  


#Linear Model for Average Download Speed Vs Attainment 8 Score
school <- schoolData %>% 
  filter(YEAR==2022) %>% 
  select(PCODE, ATT8SCR) %>% 
  rename(Postcode="PCODE")

school_broadband <-merge(downSpeed,school, by= "Postcode")

ggplot(data = school_broadband, aes(x = AvgDownSpeed, y = ATT8SCR)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average download speed VS Attainment 8 Score", 
       x = "Average download speed", 
       y = "Attainment 8 Score") +
  theme_minimal()


#Linear Model for Attainment 8 Score Vs Housing Price
school_housing = merge(school, housingPrice, by = "Postcode")

ggplot(data = school_housing, aes(x = ATT8SCR, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average attainment 8 score vs Housing Price", 
       x = "ATT8SCR", 
       y = "Price") +
  theme_minimal()



