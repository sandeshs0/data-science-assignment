library(tidyverse)

#Importing Libraries
crime=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/crime_cleaned.csv")
school=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/schoolcleaned.csv")
housingPrice=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/housing_cleaned.csv")
broadband=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/broadband_cleaned.csv")

dim(crime)

#Selecting Relevent Rows from Broadband
selected_broadband= broadband %>%
  group_by(Town_City) %>%
  summarise(
    avg_upl_speed = mean(AvgUpSpeed),
    avg_down_speed = mean(AvgDownSpeed)
  )%>%
  mutate(TOWN =  str_trim(toupper(Town_City)))%>%
  select(TOWN, avg_upl_speed, avg_down_speed)

selected_attainment8= school %>%
  group_by(TOWN) %>%
  summarise(avgAtt8 = mean(ATT8SCR))%>%
  select(TOWN, avgAtt8)%>%
  distinct()%>%
  mutate(TOWN= str_trim(toupper(TOWN)))

selected_house = housingPrice %>%
  filter(Year == 2023)%>%
  mutate(TOWN =  str_trim(toupper(Town_City)))%>%
  group_by(TOWN) %>%
  summarise(avgPrice = mean(Price))%>%
  select(avgPrice, TOWN)%>%
  na.omit()%>%
  distinct()

town = housingPrice %>%
  mutate(postcode  = str_trim(substring(Postcode,  1, 6))) %>%
  mutate(TOWN =  str_trim(toupper(Town_City)))%>%
  select(postcode, TOWN)%>%
  distinct()

selected_crime = crime %>%
  filter(Year==2023)%>%
  group_by(postcode)%>%
  summarise(crimeno =n())%>%
  arrange(desc(crimeno))%>%
  select(postcode, crimeno)

final_crime = selected_crime%>%
  left_join(town, by= "postcode")%>%
  na.omit()%>%
  distinct()

final_crime = final_crime%>%
  group_by(TOWN)%>%
  summarise(crimerate = sum(crimeno))%>%
  select(TOWN, crimerate)

ranking = selected_house %>%
  left_join(selected_attainment8, by = "TOWN") %>%  
  left_join(selected_broadband, by = "TOWN") %>%  
  left_join(final_crime, by = "TOWN") %>%
  na.omit()
view(ranking)


# Calculating the minimum and Maximum for each column
Extremes <- ranking_Points %>%
  summarise(
    minDownSpeed= min(avg_down_speed),
    maxDownSpeed = max(avg_down_speed),
    minUpSpeed = min(avg_upl_speed),
    maxUpSpeed = max(avg_upl_speed),
    minAtt8 = min(avgAtt8),
    maxAtt8 = max(avgAtt8),
    minHousingPrice = min(avgPrice),
    maxHousingPrice = max(avgPrice),
    minCrimeRate = min(crimerate),
    maxCrimeRate = max(crimerate)
  )

# Normalizing and calculating the final points
finalRanking <- ranking_Points %>%
  mutate(
    normDownSpeed = (avg_down_speed - Extremes$minDownSpeed) / (Extremes$maxDownSpeed - Extremes$minDownSpeed),
    normUpSpeed = (avg_upl_speed - Extremes$minUpSpeed) / (Extremes$maxUpSpeed - Extremes$minUpSpeed),
    normAtt8 = (avgAtt8 - Extremes$minAtt8) / (Extremes$maxAtt8 - Extremes$minAtt8),
    normHousingPrice = 1 - (avgPrice - Extremes$minHousingPrice) / (Extremes$maxHousingPrice - Extremes$minHousingPrice),
    normCrimeRate = 1 - (crimerate - Extremes$minCrimeRate) / (Extremes$maxCrimeRate - Extremes$minCrimeRate),
    finalPoints = normDownSpeed + normUpSpeed + normAtt8 + normHousingPrice + normCrimeRate
  )

# View the data with the final score
finalRanking= finalRanking%>%
  arrange(desc(finalPoints))

View(finalRanking)
write_csv(finalRanking, "D:/Academics/Fourth Semester/Data Science/Assignment/ranking.csv")

houserank = finalRanking %>%
  select(TOWN, avgPrice, normHousingPrice)%>%
  arrange(desc(normHousingPrice))

view(houserank)  

crimerank = finalRanking %>%
  select(TOWN, normCrimeRate, crimerate) %>%
  arrange(desc(normCrimeRate))
view(crimerank)

schoolrank = finalRanking %>%
  select(TOWN, avgAtt8, normAtt8)%>%
  arrange(desc(normAtt8))
view(schoolrank)

broadbandrank = finalRanking %>%
  select(TOWN, avg_down_speed, normDownSpeed)%>%
  arrange(desc(normDownSpeed))

view(broadbandrank)

final_rank = finalRanking  %>%
  select(TOWN, avgPrice, crimerate, avgAtt8, avg_down_speed, finalPoints) %>%
  arrange(desc(finalPoints))
view(final_rank)
