# libraries
library(dplyr)
library(readr)
library(tidyverse)
# Defining column names 
column_names <- c("Transaction_ID", "Price", "Transaction_Date", "Postcode", 
                  "Property_Type", "Old_New", "Duration", "PAON", "SAON", 
                  "Street", "Locality", "Town_City", "District", "County", 
                  "PPD_Category_Type", "Record_Status")
# Loading the datasets with column names
housing_2020 <- read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Housing/pp-2020.csv", col_names = column_names)
housing_2021 <- read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Housing/pp-2021.csv", col_names = column_names)
housing_2022 <- read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Housing/pp-2022.csv", col_names = column_names)
housing_2023 <- read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Housing/pp-2023.csv", col_names = column_names)
# Combining the datasets
combined_housing = bind_rows(housing_2020, housing_2021, housing_2022, housing_2023)
#Data Cleaning
cleaned_housing <- combined_housing %>%
  filter(County %in% c('CORNWALL', 'CITY OF BRISTOL')) %>%
  mutate(Year = year(Transaction_Date)) %>% 
  select(Price, Postcode,Year,Town_City,County) %>% 
  na.omit() %>%
  distinct()
write_csv(cleaned_housing, 'D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/housing_cleaned.csv')

#------------------CLEANING BROADBAND SPEED DATASET--------
broadband <- read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Broadband Speed/201805_fixed_pc_performance_r03.csv")
#Selecting Relevant Columns
broadband_selected <- broadband %>%
  select("postcode_space",
         "Median download speed (Mbit/s)",
         "Median upload speed (Mbit/s)",
         "Average upload speed (Mbit/s)",
         "Maximum upload speed (Mbit/s)",
         "Average download speed (Mbit/s)",
         "Maximum download speed (Mbit/s)",
  )
#Checking Null Values
na_summary <- sapply(broadband_selected, function(x) sum(is.na(x)))
print(na_summary)
#Cleaning
broadband_clean <-broadband_selected %>% 
  rename(
    Postcode = postcode_space,
    MedianDownSpeed = `Median download speed (Mbit/s)`,
    MedianUpSpeed = `Median upload speed (Mbit/s)`,
    AvgUpSpeed = `Average upload speed (Mbit/s)`,
    MaxUpSpeed = `Maximum upload speed (Mbit/s)`,
    AvgDownSpeed = `Average download speed (Mbit/s)`,
    MaxDownSpeed = `Maximum download speed (Mbit/s)`
  ) %>% 
  na.omit() %>% 
  distinct()
#Checking for null values after Cleaning
na_summary_clean <- sapply(broadband_clean, function(x) sum(is.na(x)))
print(na_summary_clean)
#Selecting only 3 rows from housing
housing_selected <- cleaned_housing %>% 
  select(Postcode,Town_City,County)

#Using Inner Join to Merge the broadband with housind data
broadband_W_housing = inner_join(housing_selected,broadband_clean,by="Postcode");
View(broadband_W_housing)

na_merged <- sapply(broadband_W_housing, function(x) sum(is.na(x)))
print(na_merged)
dim(broadband_W_housing)


#Removing redundent rows
broadband_final <- broadband_W_housing %>%
  distinct()
dim(broadband_final)
View(broadband_final)

#Saving the dataset
write_csv(broadband_final, "D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/broadband_cleaned.csv")


#________________CRIME Cleaning_____________________
# Cornwall
#2021
corn1=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-07/2021-07-devon-and-cornwall-street.csv")
corn2=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-08/2021-08-devon-and-cornwall-street.csv")
corn3=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-09/2021-09-devon-and-cornwall-street.csv")
corn4=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-10/2021-10-devon-and-cornwall-street.csv")
corn5=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-11/2021-11-devon-and-cornwall-street.csv")
corn6=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-12/2021-12-devon-and-cornwall-street.csv")
#2022
corn7=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-01/2022-01-devon-and-cornwall-street.csv")
corn8=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-02/2022-02-devon-and-cornwall-street.csv")
corn9=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-03/2022-03-devon-and-cornwall-street.csv")
corn10=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-04/2022-04-devon-and-cornwall-street.csv")
corn11=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-05/2022-05-devon-and-cornwall-street.csv")
corn12=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-06/2022-06-devon-and-cornwall-street.csv")
corn13=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-07/2022-07-devon-and-cornwall-street.csv")
corn14=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-08/2022-08-devon-and-cornwall-street.csv")
corn15=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-09/2022-09-devon-and-cornwall-street.csv")
corn16=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-10/2022-10-devon-and-cornwall-street.csv")
corn17=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-11/2022-11-devon-and-cornwall-street.csv")
corn18=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-12/2022-12-devon-and-cornwall-street.csv")
#2023
corn19=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-01/2023-01-devon-and-cornwall-street.csv")
corn20=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-02/2023-02-devon-and-cornwall-street.csv")
corn21=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-03/2023-03-devon-and-cornwall-street.csv")
corn22=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-04/2023-04-devon-and-cornwall-street.csv")
corn23=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-05/2023-05-devon-and-cornwall-street.csv")
corn24=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-06/2023-06-devon-and-cornwall-street.csv")
corn25=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-07/2023-07-devon-and-cornwall-street.csv")
corn26=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-08/2023-08-devon-and-cornwall-street.csv")
corn27=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-09/2023-09-devon-and-cornwall-street.csv")
corn28=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-10/2023-10-devon-and-cornwall-street.csv")
corn29=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-11/2023-11-devon-and-cornwall-street.csv")
corn30=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-12/2023-12-devon-and-cornwall-street.csv")
#2024
corn31=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-01/2024-01-devon-and-cornwall-street.csv")
corn32=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-02/2024-02-devon-and-cornwall-street.csv")
corn33=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-03/2024-03-devon-and-cornwall-street.csv")
corn34=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-04/2024-04-devon-and-cornwall-street.csv")
corn35=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-05/2024-05-devon-and-cornwall-street.csv")
corn36=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-06/2024-06-devon-and-cornwall-street.csv")


#Bistrol
#2021
bist1=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-07/2021-07-avon-and-somerset-street.csv")
bist2=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-08/2021-08-avon-and-somerset-street.csv")
bist3=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-09/2021-09-avon-and-somerset-street.csv")
bist4=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-10/2021-10-avon-and-somerset-street.csv")
bist5=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-11/2021-11-avon-and-somerset-street.csv")
bist6=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2021-12/2021-12-avon-and-somerset-street.csv")
#2022
bist7=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-01/2022-01-avon-and-somerset-street.csv")
bist8=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-02/2022-02-avon-and-somerset-street.csv")
bist9=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-03/2022-03-avon-and-somerset-street.csv")
bist10=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-04/2022-04-avon-and-somerset-street.csv")
bist11=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-05/2022-05-avon-and-somerset-street.csv")
bist12=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-06/2022-06-avon-and-somerset-street.csv")
bist13=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-07/2022-07-avon-and-somerset-street.csv")
bist14=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-08/2022-08-avon-and-somerset-street.csv")
bist15=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-09/2022-09-avon-and-somerset-street.csv")
bist16=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-10/2022-10-avon-and-somerset-street.csv")
bist17=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-11/2022-11-avon-and-somerset-street.csv")
bist18=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2022-12/2022-12-avon-and-somerset-street.csv")
#2023
bist19=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-01/2023-01-avon-and-somerset-street.csv")
bist20=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-02/2023-02-avon-and-somerset-street.csv")
bist21=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-03/2023-03-avon-and-somerset-street.csv")
bist22=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-04/2023-04-avon-and-somerset-street.csv")
bist23=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-05/2023-05-avon-and-somerset-street.csv")
bist24=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-06/2023-06-avon-and-somerset-street.csv")
bist25=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-07/2023-07-avon-and-somerset-street.csv")
bist26=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-08/2023-08-avon-and-somerset-street.csv")
bist27=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-09/2023-09-avon-and-somerset-street.csv")
bist28=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-10/2023-10-avon-and-somerset-street.csv")
bist29=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-11/2023-11-avon-and-somerset-street.csv")
bist30=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2023-12/2023-12-avon-and-somerset-street.csv")
#2024
bist31=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-01/2024-01-avon-and-somerset-street.csv")
bist32=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-02/2024-02-avon-and-somerset-street.csv")
bist33=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-03/2024-03-avon-and-somerset-street.csv")
bist34=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-04/2024-04-avon-and-somerset-street.csv")
bist35=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-05/2024-05-avon-and-somerset-street.csv")
bist36=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Crime/2024-06/2024-06-avon-and-somerset-street.csv")

#Combining the Datasets
crime_combined=rbind(
  bist1,bist2,bist3,bist4,bist5,bist6,bist7,bist8,bist9,bist10,bist11,bist12,bist13,bist14,bist15,bist16,bist17,bist18,bist19,bist20,bist21,
  bist22,bist23,bist24,bist25,bist26,bist27,bist28,bist29,bist30,bist31,bist32,bist33,bist34,bist35,bist36,
  
  corn1,corn2,corn3,corn4,corn5,corn6,corn7,corn8,corn9,corn10,corn11,corn12,corn13,corn14,corn15,corn16,corn17,corn18,corn19,corn20,corn21,corn22,
  corn23,corn24,corn25,corn26,corn27,corn28,corn29,corn30,corn31,corn32,corn33,corn34,corn35,corn36
)

head(crime_combined)
View(crime_combined)
dim(crime_combined)

#Converting it to a tibble
crime_combined <- crime_combined %>% 
  as_tibble()


#POST CODE TO LSOA

postcode_to_lsoa <- read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Postcode to LSOA.csv")
population <- read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/Population.csv")
#Selecting relevant columns
selected_lsoa <- postcode_to_lsoa %>%
  select(`lsoa11cd`, `lsoa11nm`, `ladnm`, `pcds`)

selected_crime <- crime_combined %>% 
  select(Month, `LSOA code`, `Crime type`, `Falls within`)

#Renaming the columns
colnames(selected_lsoa) = c('LSOA code', 'street', 'counties', "postcode")
colnames(population) = c("postcode", "population")
View(selected_crime)
View(selected_lsoa)
#Cleaning LSOA
clean_lsoa <- selected_lsoa %>% 
  filter(counties %in% c("Bristol, City of","Cornwall")) %>% 
  mutate(postcode=str_trim((substring(postcode,1,6))))

#Checking for duplicates of lsoa codes
any(duplicated(selected_crime$`LSOA code`))
any(duplicated(clean_lsoa$`LSOA code`))

#Since the duplicates are there, removing them
clean_lsoa=unique(clean_lsoa,by="LSOA code")
selected_crime=unique(selected_crime,by="LSOA code")

#Final Cleaning and merging
finalCrime= selected_crime %>% 
  left_join(clean_lsoa, by=("LSOA code"),relationship = "many-to-many") %>% 
  mutate(Year=str_trim(substring(Month,1,4))) %>% 
  mutate(Month=str_trim(substring(Month,6,7))) %>% 
  left_join(population,by="postcode") %>% 
  distinct() %>% 
  na.omit()

dim(finalCrime)
dim(clean_lsoa)
View(finalCrime)
dim
write_csv(finalCrime, "D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/crime_cleaned.csv")

#--------------------------------SCHOOL-----------------------------
bSchool21=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/School/Bristol/2021-2022/801_ks4final.csv")
bSchool22=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/School/Bristol/2022-2023/801_ks4final.csv")
cSchool21=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/School/Cornwall/2021-2022/908_ks4final.csv")
cSchool22=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Obtained Datasets/School/Cornwall/2022-2023/908_ks4final.csv")

view(bSchool21)
head(bSchool21)
view(cSchool21)

bSchool21 <- bSchool21 %>% 
  select(SCHNAME,PCODE,ATT8SCR,TOWN) %>% 
  mutate(YEAR=2021,COUNTY="Bristol")

bSchool22 <- bSchool22 %>% 
  select(SCHNAME,PCODE,ATT8SCR,TOWN) %>% 
  mutate(YEAR=2022,COUNTY="Bristol")

cSchool21 <- cSchool21 %>% 
  select(SCHNAME,PCODE,ATT8SCR,TOWN) %>% 
  mutate(YEAR=2021,COUNTY="Cornwall")

cSchool22 <- cSchool22 %>% 
  select(SCHNAME,PCODE,ATT8SCR,TOWN) %>% 
  mutate(YEAR=2022,COUNTY="Cornwall")

#Combining
bCombined=rbind(bSchool21,bSchool22)
cCombined=rbind(cSchool21,cSchool22)

View(bCombined)
View(cCombined)

schoolCombined=rbind(bCombined,cCombined)
View(schoolCombined)

schoolCombined <- schoolCombined %>% 
  filter(ATT8SCR!="NE"&ATT8SCR!="SUPP") %>%
  na.omit() %>% 
  distinct()

dim(schoolCombined)
View(schoolCombined)

write_csv(schoolCombined,"D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/schoolcleaned.csv")


