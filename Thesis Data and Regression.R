# Call libraries
library(dplyr)
library(stringr)
library(lubridate)
library(ggmap)
library(sqldf)
library(plm)
library(stargazer)
library(writexl)
library(readxl)

### FUNCTIONS

## F-test
Ftest <- function(uReg, rReg, restrictors, totalCoefficients, n) {
  RSSu <- sum(resid(uReg)^2)
  RSSr <- sum(resid(rReg)^2)
  num <- (RSSr - RSSu)/restrictors
  den <- RSSu/(n-totalCoefficients)
  df <- c(RSSu, RSSr, num, den, num/den)
  return(num/den)
}

## Gather data points into DF
createZIP <- function(zip, ownerOccupancy2021, ownerOccupancy2022, housingUnits2021, housingUnits2022, householdIncome2021, householdIncome2022, Vacanct2021, Vacant2022, 
                      Population2021, Population2022, PercentWhite2021, PercentWhite2022, PercentAA2021,PercentAA2022, PercentAsian2021, PercentAsian2022,
                      PercentHispanic2021, PercentHispanic2022, UnemploymentRate2021, UnemploymentRate2022, medianAge2021, medianAge2022,
                      percentCollegeEducated2021, percentCollegeEducated2022, totalManagement2021, occasionalUse2021, occasionalUse2022) {
  
  # Primary independent variables
  x  <- data.frame(Date = c(as.Date("2021/10/01"), as.Date("2021/11/01"),as.Date("2021/12/01"),as.Date("2022/01/01"),as.Date("2022/02/01"),as.Date("2022/03/01"),as.Date("2022/04/01"),as.Date("2022/05/01"),as.Date("2022/06/01"),as.Date("2022/07/01"),as.Date("2022/08/01"),as.Date("2022/09/01"),as.Date("2022/10/01"),as.Date("2022/11/01"), as.Date("2022/12/01")))
  numberOfABNBS <- activeABNBdf %>% filter(ZIP == zip) %>% summarise(sum(Oct2021),sum(Nov2021),sum(Dec2021),sum(Jan2022), sum(Feb2022),sum(Mar2022),sum(Apr2022),sum(May2022),sum(Jun2022), sum(Jul2022), sum(Aug2022), sum(Sep2022), sum(Oct2022), sum(Nov2022), sum(Dec2022))
  x['numberOfABNBS'] <- t(numberOfABNBS) #Number of Airbnbs in given zip code
  x <- x %>% mutate(ABNBConcentration = (numberOfABNBS/ifelse(year(x$Date) == 2021, housingUnits2021, housingUnits2022))*100) # Concentration of Airbnbs in ZIP code by month
  
  # Control Variables
  x$OwnerOccupancyRate <- ifelse(year(x$Date) == 2021, ownerOccupancy2021, ownerOccupancy2022) #Percent of Owner Occupants in given zip code
  x$HousingUnits <- ifelse(year(x$Date) == 2021, housingUnits2021/1000, housingUnits2022/1000) #Number of housing units in given 
  x$HouseholdMedianIncome <- ifelse(year(x$Date) == 2021, householdIncome2021/10000, householdIncome2022/10000) #Median Household Income for given zip code (in thousands of dollars)
  x$PercentVacant <- ifelse(year(x$Date) == 2021, (Vacanct2021/housingUnits2021)*100, (Vacant2022/housingUnits2022)*100) #Percent of housing units that are vacant in given zip code
  x$Population <- ifelse(year(x$Date) == 2021, Population2021/1000, Population2022/1000) #Population in given zip code
  x$PercentWhiteNH <- ifelse(year(x$Date) == 2021, PercentWhite2021 - PercentHispanic2021, PercentWhite2022 - PercentHispanic2022) #Percent of Non-Hispanic Whites in given zip code
  x$PercentAA <- ifelse(year(x$Date) == 2021, PercentAA2021, PercentAA2022) #Percent of African Americans in given zip code
  x$PercentAsian <- ifelse(year(x$Date) == 2021, PercentAsian2021, PercentAsian2022) #Percent of Asians in given zip code
  x$PercentHispanic <- ifelse(year(x$Date) == 2021, PercentHispanic2021, PercentHispanic2022) # Percent of Hispanics in given zip code
  x$UnemploymentRate <- ifelse(year(x$Date) == 2021, UnemploymentRate2021, UnemploymentRate2022) # Unemployment Rate in given zip code
  x$medianAge <- ifelse(year(x$Date) == 2021, medianAge2021, medianAge2022) # Median age of residents in given zip code
  x$percentCollegeEducated <- ifelse(year(x$Date) == 2021, percentCollegeEducated2021, percentCollegeEducated2022) # Percent of residents over 25 with at least a bachelor's degree
  x$percentManagement <- (totalManagement2021/Population2021)*100 #percent in Professional, scientific, and management, and administrative and waste management servicesndustry over 16
  x$percentOccasionalUse <- ifelse(year(x$Date) == 2021, occasionalUse2021/Vacanct2021, occasionalUse2022/Vacant2022) #number of vacant home that are deemed vacant because of occasional use
  x$occasionalUse <- ifelse(year(x$Date) == 2021, occasionalUse2021, occasionalUse2022) # percent of homes without full time residents
  x$ZIP <- zip # ZIP code
  
  
  #month dummies
  x$Jan <- ifelse(month(x$Date) == 1, 1, 0)
  x$Feb <- ifelse(month(x$Date) == 2, 1, 0)
  x$Mar <- ifelse(month(x$Date) == 3, 1, 0)
  x$Apr <- ifelse(month(x$Date) == 4, 1, 0)
  x$May <- ifelse(month(x$Date) == 5, 1, 0)
  x$Jun <- ifelse(month(x$Date) == 6, 1, 0)
  x$Jul <- ifelse(month(x$Date) == 7, 1, 0)
  x$Aug <- ifelse(month(x$Date) == 8, 1, 0)
  x$Sep <- ifelse(month(x$Date) == 9, 1, 0)
  x$Oct <- ifelse(month(x$Date) == 10, 1, 0)
  x$Nov <- ifelse(month(x$Date) == 11, 1, 0)
  x$Dec <- ifelse(month(x$Date) == 12, 1, 0)
  x$SnowBirdSeason <- ifelse(x$May == 1 | x$Jun == 1 | x$Jul == 1 | x$Aug == 1 | x$Sep == 1, 0, 1)
  
  # Convert all broader economic data to be from the beginning of the month
  x <- x %>% left_join(mortgagerate30yfixed, by = join_by(closest(Date >= DATE))) 
  x <- x %>% select(-'DATE')
  x <- x %>% left_join(disInc %>% mutate(DSPI = DSPI/1000), by= join_by(Date == DATE))
  x <- x %>% left_join(nationalUnem, by= join_by(Date == DATE))
  x <- x %>% left_join(SFlistings, by= join_by(Date == DATE))
  x$consumerIndex <- consumerSentiment[10:24,]$Index
  x$SFCORECPI <- southFLCPIlessFE
  x$sp500 <- sp500/100
  
  ## Indepenedent Variables
  x['ZHVIALL'] <- t(ZHVI %>% filter(RegionName == zip) %>% select(270:284))
  # Add log of dependent variable for interpretability of results
  editedRegdf$log_all <- log(editedRegdf$ZHVIALL)
  
  return(x)
}

### DATA IMPORTS

## Broader Economic Factors
disInc = read.csv("C:/Users/kthay/Downloads/DSPI.csv")
disInc$DATE <- as.Date(disInc$DATE)
nationalUnem = read.csv("C:/Users/kthay/Downloads/UNRATE.csv")
nationalUnem$DATE <- as.Date(nationalUnem$DATE)
consumerSentiment <- read.csv("C:/Users/kthay/Downloads/sca-table1-on-2023-Nov-14 (1).csv")
sp500 <- c(4605, 4567, 4766, 4515, 4373, 4530, 4131, 4132, 3785, 4130, 3955, 3585, 3871, 4080, 3839) #sp500 close by month from yahoo finance https://finance.yahoo.com/quote/%5EGSPC/history?period1=1632960000&period2=1674000000&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true
southFLCPIlessFE <- c(4.5, 5.7, 6.0, 6.4, 8.3, 7.2, 7.5, 7.95, 7.9, 8.7, 9.3, 9.4, 9.7, 10.2, 10.4)
SFCPI <- c(5.7,7.2, 7.1,7.8,9.3,8.3,9.1,8.9,10.1,9.4,10.2,10.0,9.6,10.5,9.9)
southFLHomeListings = read.csv("C:/Users/kthay/Downloads/ACTLISCOU33100.csv")
southFLHomeListings$DATE <- as.Date(southFLHomeListings$DATE)
SFlistings <- southFLHomeListings %>% mutate(ACTLISCOU33100 = ACTLISCOU33100/1000)

## Independent Variable - ALL home ZHVI
ZHVI <- read.csv("C:/Users/kthay/OneDrive/Desktop/Inside ABNB data/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month (1).csv")

## Airbnb listing data
listings2022 = read.csv("C:/Users/kthay/OneDrive/Desktop/Inside ABNB data/sept 21, 2022/listings2022.csv")
listings2023 = read.csv("C:/Users/kthay/OneDrive/Desktop/Inside ABNB data/june 25, 2023/listings2023.csv")

#Only select entire homes
df2022 <- listings2022 %>% subset(room_type == "Entire home/apt", select = c('id', 'first_review', 'last_review', 'beds', 'bedrooms', 'room_type', 'host_id', 'latitude', 'longitude', 'reviews_per_month','calculated_host_listings_count_entire_homes'))
df2023 <- listings2023 %>% subset(room_type == "Entire home/apt", select = c('id', 'first_review', 'last_review', 'beds', 'bedrooms', 'room_type', 'host_id', 'latitude', 'longitude', 'reviews_per_month','calculated_host_listings_count_entire_homes'))
df2022 <- df2022 %>% replace(is.na(.), 0)
df2023 <- df2023 %>% replace(is.na(.), 0)
sum(df$first_review == '')

#combine dfs
dfcombined <- full_join(df2023, df2022, by='id') %>% mutate(first_review = coalesce(first_review.x,first_review.y)) %>% mutate(beds = coalesce(beds.x, beds.y)) %>% mutate(last_review = coalesce(last_review.x,last_review.y)) %>% mutate(bedrooms = coalesce(bedrooms.x,bedrooms.y)) %>% mutate(latitude = coalesce(latitude.x,latitude.y)) %>% 
  mutate(longitude = coalesce(longitude.x,longitude.y)) %>%  mutate(reviews_per_month = coalesce(reviews_per_month.x,reviews_per_month.y)) %>%  mutate(calculated_host_listings_count_entire_homes = coalesce(calculated_host_listings_count_entire_homes.x,calculated_host_listings_count_entire_homes.y)) %>% subset(select = c('id', 'first_review', 'last_review', 'bedrooms', 'beds', 'latitude', 'longitude', 'reviews_per_month','calculated_host_listings_count_entire_homes'))

#filter any listings without reviews
dfcombined <- dfcombined %>% filter(first_review != '')

# convert review date data to ymd format
dfcombined$last_review <- ymd(dfcombined$last_review)
dfcombined$first_review <- ymd(dfcombined$first_review)

#count based on reviews with 2 month buffer 
activeABNBdf <- dfcombined %>% mutate(Oct2021 = ifelse(last_review > as.Date('2021-08-01') & first_review < as.Date('2021-12-01'), 1, 0)) %>%
  mutate(Nov2021 = ifelse(last_review > as.Date('2021-09-01') & first_review < as.Date('2022-1-01'), 1, 0)) %>%
  mutate(Dec2021 = ifelse(last_review > as.Date('2021-10-01') & first_review < as.Date('2022-2-01'), 1, 0)) %>%
  mutate(Jan2022 = ifelse(last_review > as.Date('2021-11-01') & first_review < as.Date('2022-3-01'), 1, 0)) %>%
  mutate(Feb2022 = ifelse(last_review > as.Date('2021-12-01') & first_review < as.Date('2022-4-01'), 1, 0)) %>%
  mutate(Mar2022 = ifelse(last_review > as.Date('2022-1-01') & first_review < as.Date('2022-5-01'), 1, 0)) %>%
  mutate(Apr2022 = ifelse(last_review > as.Date('2022-2-01') & first_review < as.Date('2022-6-01'), 1, 0)) %>%
  mutate(May2022 = ifelse(last_review > as.Date('2022-3-01') & first_review < as.Date('2022-7-01'), 1, 0)) %>%
  mutate(Jun2022 = ifelse(last_review > as.Date('2022-4-01') & first_review < as.Date('2022-8-01'), 1, 0)) %>%
  mutate(Jul2022 = ifelse(last_review > as.Date('2022-5-01') & first_review < as.Date('2022-9-01'), 1, 0)) %>%
  mutate(Aug2022 = ifelse(last_review > as.Date('2022-6-01') & first_review < as.Date('2022-10-01'), 1, 0)) %>%
  mutate(Sep2022 = ifelse(last_review > as.Date('2022-7-01') & first_review < as.Date('2022-11-01'), 1, 0)) %>%
  mutate(Oct2022 = ifelse(last_review > as.Date('2022-8-01') & first_review < as.Date('2022-12-01'), 1, 0)) %>%
  mutate(Nov2022 = ifelse(last_review > as.Date('2022-9-01') & first_review < as.Date('2023-1-01'), 1, 0)) %>%
  mutate(Dec2022 = ifelse(last_review > as.Date('2022-10-01') & first_review < as.Date('2023-2-01'), 1, 0)) 

#get ZIP codes for every coordinate pair in dataframe
ziplist <- list() #empty list to store ZIP codes
for(i in 1:nrow(activeABNBdf)){ 
  # get full address of each listing using Google Maps reverse geocode tool
  address <- revgeocode(c(activeABNBdf[i,]$longitude, activeABNBdf[i,]$latitude), output = "address")
  # isolate ZIP code from address by selecting last 5 digit number
  zip <- str_extract(address, "[d]{5}(?![d])(?!.*\\d)")
  # add ZIP code to list
  ziplist <- append(ziplist, zip)
}

# Attach ZIP codes to df
activeABNBdf$ZIP <- ziplist

# Fill in data for each ZIP code
df33301 <- createZIP("33301", 39.7, 37.7, 12636, 13378, 104436, 115161, 3466, 3361, 16261, 17069, 87.2, 85.2, 9.2, 8.8, 3.2, 3.8, 16.4, 17.3, 1.6, 1.7, 42.6, 41.3, 59.9, 61.0, 3252, 1662, 1619)
df33304 <- createZIP("33304", 38.2, 40.9, 12683, 12554, 69343, 81266, 3153, 2988, 18085, 18923, 78.6, 75.2, 20.1, 20.5, 1.9, 3.1, 17.7, 18.4, 4.8, 3.7, 45.3, 45.9, 51.5, 50.8, 2084, 1897, 1868) 
df33305 <- createZIP("33305", 67.8, 68.8, 8065, 8074, 85800, 85246, 1425, 1348, 12634, 12633, 91.7, 88.1, 4.6, 7.8, 1.7, 2.1, 12.4, 12.7, 2.4, 4.1, 50.2, 51.4, 56.2, 55.1, 2065, 524, 721)
df33306 <- createZIP("33306", 73.2, 72.1, 2285, 2157, 66367, 74385, 442, 510, 3111, 2767, 92.2, 93.6, 2.9, 2.2, 1.4, 1.0, 16.2, 15.3, 4.2, 1.5, 54.5, 56.5, 52.4, 44.4, 476, 311, 370)
df33308 <- createZIP("33308", 70.2, 68.9, 23703, 23595, 73390, 80445, 8065, 7917, 28428, 28406, 93.3, 94.0, 4.0, 3.8, 3.7, 3.8, 15.2, 15.6, 5.9, 5.7, 56.5, 56.5, 53.2, 54.7, 3780, 6840, 6520)
df33309 <- createZIP("33309", 58.6, 56.8, 15101, 14888, 61898, 68274, 1035, 1078, 40100, 37829, 48.4, 49.7, 48.1, 47.0, 2.6, 2.8, 27.2, 28.5, 7.2, 7.1, 37, 38.1, 25.1, 25.2, 3111, 314, 387)
df33311 <- createZIP("33311", 45.0, 46.1, 27714, 27466, 43007, 48020, 2977, 2772, 73388, 75435, 15.7, 16.1, 83.6, 83.3, 0.6, 0.7, 7.3, 7.9, 9.9, 8.3, 34.6, 34.2, 16.4, 17.2, 2473, 1103, 975)
df33312 <- createZIP("33312", 60.9, 62.8, 21379, 21687, 63240, 72955, 2889, 2838, 53855, 53409, 63.6, 66.1, 30.9, 29.7, 2.5, 1.8, 32.6, 33.1, 7.8, 6.2, 38, 41.0, 29.8, 29.8, 3917, 915, 944)
df33313 <- createZIP("33313", 43.1, 42.8, 25213, 24893, 42177, 43282, 3433, 2943, 63715, 62232, 17.0, 16.2, 80.0, 80.8, 2.2, 1.9, 11.3, 11.8, 8.1, 8.6, 34.6, 35.2, 15.1, 14.7, 2055, 2062, 1629)
df33314 <- createZIP("33314", 37.3, 35.2, 11483, 11906, 55317, 59765, 1048, 1219, 28730, 28978, 68.5, 68.3, 15.3, 13.8, 6.7, 7.6, 51.5, 49.9, 3.7, 5.0, 32.5, 32.1, 30.3, 31.0, 2208, 90, 178)
df33315 <- createZIP("33315", 59.5, 55.6, 6991, 6979, 81501, 88034, 1096, 1147, 13178, 13253, 91.5, 91.7, 5.6, 6.1, 1.3, 1.5, 28.5, 27.9, 2.6, 1.6, 40.6, 39.9, 35.3, 37.4, 1854, 397, 405)
df33316 <- createZIP("33316", 57.1, 57.0, 8498, 8842, 82973, 93063, 2946, 2958, 11194, 11304, 83.6, 84.9, 9.5, 9.6, 5.1, 5.0, 15.3, 14.1, 3.6, 3.1, 50.8, 53.3, 56, 56.8, 1790, 1954, 1870)
df33317 <- createZIP("33317", 75.4, 74.9, 13550, 13430, 75767, 79511, 788, 675, 37161, 36322, 64.3, 63.3, 26.9, 27.7, 5.0, 5.3, 33.5, 33.2, 6.9, 6.3, 42.3, 42.2, 34.6, 36.8, 3002, 147, 72)
df33319 <- createZIP("33319", 64.7, 65.6, 25170, 25008, 49770, 53705, 4437, 4042, 50703, 52000, 31.0, 30.2, 64.4, 65.3, 4.8, 4.5, 16.5, 17.1, 5.1, 6.4, 43.4, 42.4, 25.8, 26.2, 2862, 2364, 2236)
df33321 <- createZIP("33321", 73.4, 74.4, 23867, 23917, 54428, 60113, 2242, 2151, 50513, 50307, 66.6, 64.4, 25.4, 26.8, 3.4, 3.1, 38.0, 37.6, 6.2, 6.3, 48.2, 48.7, 27.9, 28.3, 3043, 1036, 908)
df33322 <- createZIP("33322", 84.1, 82.4, 21244, 21347, 55281, 59066, 2845, 2617, 40244, 40738, 66.6, 63.6, 25.4, 27.3, 6.8, 7.4, 34.5, 33.3, 5.7, 5.8, 53.3, 53.3, 32.4, 33.2, 3586, 1558, 1540)
df33323 <- createZIP("33323", 69.2, 73.1, 9104, 9285, 100478, 110395, 785, 951, 23099, 22506, 74.6, 70.4, 19.5, 20.4, 5.8, 6.0, 32.3, 33.4, 4.9, 4.6, 40.4, 41.9, 44.7, 47.0, 2927, 403, 470)
df33324 <- createZIP("33324", 57.9, 56.5, 22019, 22134, 73772, 80348, 2503, 2274, 49343, 49994, 73.1, 72.0, 18.8, 19.8, 7.3, 7.5, 30.3, 31.5, 5.1, 5.7, 38.4, 38.3, 45.5, 43.4, 5250, 890, 886)
df33325 <- createZIP("33325", 75.6, 76.1, 11155, 11199, 85607, 93060, 791, 712, 31410, 31546, 80.8, 80.4, 10.9, 11.6, 5.5, 5.3, 49.2, 50.2, 5.7, 5.1, 38.1, 38.3, 37.0, 38.0, 3244, 193, 148)
df33326 <- createZIP("33326", 71.7, 70.3, 12967, 12757, 99828, 107899, 2312, 1957, 31687, 31514, 88.5, 88.6, 5.1, 5.2, 5.2, 4.7, 54.5, 54.9, 3.7, 3.4, 41.3, 41.2, 57.0, 58.2, 3784, 1636, 1566)
df33327 <- createZIP("33327", 79.0, 77.6, 6617, 6640, 147103, 158184, 60, 56, 23593, 23734, 82.3, 81.2, 7.0, 7.1, 8.8, 8.3, 50.4, 54.3, 5.6, 4.7, 39.9, 39.0, 69.2, 69.3, 3712, 22, 0)
df33328 <- createZIP("33328", 80.6, 80.5, 10562, 10896, 108569, 120195, 622, 635, 28373, 29544, 84.7, 83.6, 6.6, 7.1, 7.0, 7.3, 29.8, 29.9, 4.5, 4.6, 39.4, 38.3, 46.5, 47.3, 3952, 192, 132)
df33330 <- createZIP("33330", 94.5, 94.9, 4848, 4788, 126782, 136839, 234, 342, 14450, 13619, 75.4, 77.0, 11.2, 10.9, 11.8, 12.1, 28.1, 26.7, 3.3, 2.2, 41.9, 44.2, 52.9, 56.1, 1652, 97, 120)
df33331 <- createZIP("33331", 84.2, 84.9, 7740, 7831, 128421, 136734, 286, 201, 25150, 25418, 84.1, 85.1, 5.8, 5.8, 9.3, 8.7, 47.3, 47.6, 3.6, 2.6, 42.2, 42.6, 57.6, 56.9, 3788, 87, 64)
df33332 <- createZIP("33332", 87.6, 87.2, 3519, 3524, 144625, 154652, 126, 63, 11484, 11809, 76.8, 81.2, 11.1, 5.9, 7.6, 10.4, 49.3, 48.3, 4.5, 3.9, 45.2, 44.9, 65.8, 65.2, 1436, 126, 63)
df33334 <- createZIP("33334", 57.7, 56.6, 14033, 14150, 61013, 65379, 1303, 1273, 29497, 31047, 77.4, 77.2, 19.1, 19.8, 2.5, 2.3, 29.6, 31.3, 6.6, 6.2, 42.5, 40.7, 35.1, 31.8, 2777, 453, 566)
df33351 <- createZIP("33351", 57.4, 57.8, 13527, 13908, 63941, 66611, 938, 1017, 34699, 35082, 46.0, 47.6, 46.5, 45.2, 7.2, 8.4, 27.3, 28.8, 4.5, 4.7, 36.5, 39.3, 32.2, 29.7, 2391, 58, 68)

# Combine rows to get regression data frame
editedRegdf <- bind_rows(df33301, df33304, df33305, df33306, df33308, df33309, df33311, df33312, df33313, df33314, df33315, df33316, df33317, df33319, df33321, df33322, df33323, df33324, df33325, df33326, df33327, df33328, df33330, df33331, df33332, df33334, df33351)

# Determine model choice between fixed and random effects using Hausman test

FEReg = plm(log_all ~ ABNBConcentration + SFCORECPI + MORTGAGE30US + UNRATE + DSPI + ACTLISCOU33100 + consumerIndex + sp500 + PercentVacant + HousingUnits + Population 
            + medianAge + percentCollegeEducated + UnemploymentRate + OwnerOccupancyRate + HouseholdMedianIncome + SnowBirdSeason,
            data = editedRegdf, index = c("ZIP","Date"), model="fd")

REReg = plm(log_all ~ ABNBConcentration + SFCORECPI + MORTGAGE30US + UNRATE + DSPI + ACTLISCOU33100 + consumerIndex + sp500 + PercentVacant + HousingUnits + Population 
            + medianAge + percentCollegeEducated + UnemploymentRate + OwnerOccupancyRate + HouseholdMedianIncome + SnowBirdSeason,
            data = editedRegdf, index = c("ZIP","Date"), model="random")

phtest(FEReg, REReg)


### FINAL REGRESSION AND RESULTS
FEReg = plm(log_all ~ ABNBConcentration + SFCORECPI + MORTGAGE30US + UNRATE + DSPI + ACTLISCOU33100 + consumerIndex + sp500 + PercentVacant + HousingUnits + Population 
            + medianAge + percentCollegeEducated + UnemploymentRate + OOrateListings + HouseholdMedianIncome,
            data = editedRegdf, index = c("ZIP","Date"), model="fd")
stargazer(FEReg, type="text")

