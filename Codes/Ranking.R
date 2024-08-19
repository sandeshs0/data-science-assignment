library(tidyverse)

crimeData=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/crime_cleaned.csv")
schoolData=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/schoolcleaned.csv")
housingData=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/housing_cleaned.csv")
speedData=read_csv("D:/Academics/Fourth Semester/Data Science/Assignment/Cleaned Datasets/broadband_cleaned.csv")

speedSelected= speedData %>%
  group_by(Town_City) %>%
  summarise(
     avgUpload= mean(AvgUpSpeed),
    avgDownload = mean(AvgDownSpeed)
  )
speedSelected = speedSelected %>% 
  mutate(Town =  str_trim(toupper(Town_City)))%>%
  select(Town, avgUpload,avgDownload)


att8= school %>%
  group_by(TOWN) %>%
  summarise(av_att8 = mean(ATT8SCR))%>%
  select(TOWN, av_att8)%>%
  distinct()%>%
  mutate(TOWN= str_trim(toupper(TOWN)))

house_price = housing %>%
  filter(Year == 2023)%>%
  mutate(TOWN =  str_trim(toupper(`Town/City`)))%>%
  group_by(TOWN) %>%
  summarise(avg_price = mean(Price))%>%
  select(avg_price, TOWN)%>%
  na.omit()%>%
  distinct()

town = housing %>%
  mutate(postcode  = str_trim(substring(Postcode,  1, 6))) %>%
  mutate(TOWN =  str_trim(toupper(`Town/City`)))%>%
  select(postcode, TOWN)%>%
  distinct()

sel_crime = crime %>%
  filter(Year==2023)%>%
  group_by(postcode)%>%
  summarise(crimeno =n())%>%
  arrange(desc(crimeno))%>%
  select(postcode, crimeno)

final_crime = sel_crime%>%
  left_join(town, by= "postcode")%>%
  na.omit()%>%
  distinct()

last_crime = final_crime%>%
  group_by(TOWN)%>%
  summarise(crimerate = sum(crimeno))%>%
  select(TOWN, crimerate)

view(house_price)
view(att8)
view(sel_speed)
view(last_crime)

ranking_data = house_price %>%
  left_join(att8, by = "TOWN") %>%  
  left_join(sel_speed, by = "TOWN") %>%  
  left_join(last_crime, by = "TOWN") %>%
  na.omit()
view(ranking_data)

rankk = ranking_data %>%
  # Rank each criterion: lower rank for better values
  mutate(
    rank_avg_price = rank(avg_price, ties.method = "min"),
    rank_avg_down_speed = rank(-avg_down_speed, ties.method = "min"),
    rank_avg_upl_speed = rank(-avg_upl_speed, ties.method = "min"),
    rank_av_att8 = rank(-av_att8, ties.method = "min"),
    rank_crimerate = rank(crimerate, ties.method = "min")
  ) %>%
  # Combine the ranks into a final score
  mutate(
    total_rank = rank_avg_price + rank_avg_down_speed + 
      rank_avg_upl_speed + rank_av_att8 + rank_crimerate
  ) %>%
  # Order the towns based on the final score
  arrange(total_rank)

# View the final ranked dataset
view(rankk)