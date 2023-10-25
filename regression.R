regdf <- read.csv("https://github.com/mkarlthayer/ABNB_Thesis/blob/main/regdf.csv")

library(plm)
library(stargazer)
reg1 = plm(ZHVI1bed ~ ABNBConcentration + OwnerOccupancyRate + HouseholdMedianIncome + MORTGAGE30US + PercentVacant + Population + PercentWhiteNH + PercentAA + PercentAsian + PercentHispanic
           + medianAge + percentCollegeEducated + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec,
           data = regdf, index = c("ZIP","Date"), model="pooling")
reg2 = plm(ZHVI2bed ~ ABNBConcentration + OwnerOccupancyRate + HouseholdMedianIncome + MORTGAGE30US + PercentVacant + Population + PercentWhiteNH + PercentAA + PercentAsian + PercentHispanic
           + medianAge + percentCollegeEducated + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec,
           data = regdf, index = c("ZIP","Date"), model="pooling")
reg3 = plm(ZHVI3bed ~ ABNBConcentration + OwnerOccupancyRate + HouseholdMedianIncome + MORTGAGE30US + PercentVacant + Population + PercentWhiteNH + PercentAA + PercentAsian + PercentHispanic
           + medianAge + percentCollegeEducated + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec,
           data = regdf, index = c("ZIP","Date"), model="pooling")
reg4 = plm(ZHVI4bed ~ ABNBConcentration + OwnerOccupancyRate + HouseholdMedianIncome + MORTGAGE30US + PercentVacant + Population + PercentWhiteNH + PercentAA + PercentAsian + PercentHispanic
           + medianAge + percentCollegeEducated + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec,
           data = regdf, index = c("ZIP","Date"), model="pooling")
reg5 = plm(ZHVI5bed ~ ABNBConcentration + OwnerOccupancyRate + HouseholdMedianIncome + MORTGAGE30US + PercentVacant + Population + PercentWhiteNH + PercentAA + PercentAsian + PercentHispanic
           + medianAge + percentCollegeEducated + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec,
           data = regdf, index = c("ZIP","Date"), model="pooling")
stargazer(reg1, reg2, reg3, reg4, reg5, type = "text")
