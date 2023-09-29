library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggraph)
library(scales)
library(ggpattern)
library(plotly)
library(readxl)


data <- read.csv("~/Library/CloudStorage/Box-Box/1_My_projects/SU Han/forDiana.csv")
land <- read.csv("~/Library/CloudStorage/Box-Box/1_My_projects/SU Han/FAOSTAT_data_en_9-28-2023.csv")
countries <- read_excel("~/Library/CloudStorage/Box-Box/1_My_projects/SU Han/20221213_v3_DLFAOSTATCountriesList.xlsx")
land <- merge(land[land$Item %in% "Cropland", c("Area", "Unit", "Value")], countries[, c("Country", "ISO3 Code")], 
              by.x="Area", by.y="Country", on="left")
land$Value <- land$Value * 1000

data3 <- data[, c("iso3", "if_bws", "ifsmh",  "fsystem", "crop_name", "HarvestedArea", "production_t",
                  "Energy_kcal", "total_water_m3", "green_water_m3", "blue_water_m3", "Iron_mg", "Zinc_mg", 
                  "Vitamin.A..expressed.in.retinol.equivalents._mcg", "Folate..dietary.folate.equivalents..mcg._mcg",
                  "Vitamin.B12_mcg","Calcium_mg")]

data3$iso3 <- as.factor(data$iso3)

data3$if_bws <- as.factor(data3$if_bws)
levels(data3$if_bws) <- list("Water scarcity" = 1, "Water abundance" = 0)
data3$ifsmh <- as.factor(data3$ifsmh)
levels(data3$ifsmh) <- list("Small scale ag." = 1, "Large scale ag." = 0)
data3$fsystem <- as.factor(data3$fsystem)
levels(data3$fsystem) <- list("Irrigated" = "I", "Rainfed" = "H", "Rainfed" = "L", "Rainfed" = "S")
data3$crop_name <- as.factor(data3$crop_name)

data3$HarvestedArea <- as.numeric(data3$HarvestedArea)
data3$production_t <- as.numeric(data3$production_t)
data3$Energy_kcal <- as.numeric(data3$Energy_kcal)
data3$total_water_m3 <- as.numeric(data3$total_water_m3)
data3$green_water_m3 <- as.numeric(data3$green_water_m3)
data3$blue_water_m3 <- as.numeric(data3$blue_water_m3)
data3$Calcium <- as.numeric(data3$Calcium_mg)
data3$Vit_B12 <- as.numeric(data3$Vitamin.B12_mcg)
data3$Folate <- as.numeric(data3$Folate..dietary.folate.equivalents..mcg._mcg)
data3$Vit_A <- as.numeric(data3$Vitamin.A..expressed.in.retinol.equivalents._mcg)
data3$Zinc <- as.numeric(data3$Zinc_mg)
data3$Iron <- as.numeric(data3$Iron_mg)

data3 <- merge(data3, land[, c("Value", "ISO3 Code")], by.x="iso3", by.y="ISO3 Code", on="left")

data3 <- data3 %>% group_by(iso3, if_bws, ifsmh, fsystem) %>% #if_bws, ifsmh,  fsystem
  summarise(HarvestedArea = sum(HarvestedArea) ,
            production_t = sum(production_t) ,
            Energy_kcal = sum(Energy_kcal) ,
            total_water_m3 = sum(total_water_m3),
            green_water_m3 = sum(green_water_m3),
            blue_water_m3 = sum(blue_water_m3), 
            Calcium = sum(Calcium),
            Vit_B12 = sum(Vit_B12),
            Folate = sum(Folate),
            Vit_A = sum(Vit_A),
            Zinc = sum(Zinc), 
            Iron = sum(Iron),
            Value = sum(Value))

totalCropland = sum(data3$Value)

data3$percCropland <- data3$Value / totalCropland
data3$HarvAreaCropland <- data3$HarvestedArea / data3$Value
data3$prodCropland <- data3$production_t / data3$Value
data3$EnergyCropland <- data3$Energy_kcal / data3$Value
data3$WaterCropland <- data3$total_water_m3 / data3$Value
data3$green_water_pc <- data3$green_water_m3 / data3$total_water_m3 
data3$blue_water_pc <- data3$blue_water_m3 / data3$total_water_m3 
data3$CalciumCropland <- data3$Calcium / data3$Value
data3$Vit_B12Cropland <- data3$Vit_B12 / data3$Value
data3$FolateCropland <- data3$Folate / data3$Value
data3$Vit_ACropland <- data3$Vit_A / data3$Value
data3$ZincCropland <- data3$Zinc / data3$Value 
data3$IronCropland <- data3$Iron / data3$Value
data3$CalciumWater <- data3$Calcium / data3$total_water_m3
data3$Vit_B12Water <- data3$Vit_B12 / data3$total_water_m3
data3$FolateWater <- data3$Folate / data3$total_water_m3
data3$Vit_AWater <- data3$Vit_A / data3$total_water_m3
data3$ZincWater <- data3$Zinc / data3$total_water_m3 
data3$IronWater <- data3$Iron / data3$total_water_m3

data_total <- data3 %>% group_by(iso3) %>%
  summarise(HarvestedArea_Country = sum(HarvestedArea))
data3 <- left_join(data3, data_total, by="iso3")
data3$PercentageHarvestedArea <- data3$HarvestedArea / data3$HarvestedArea_Country * 100

data4 <- data3[data3$PercentageHarvestedArea > 0.4999, ]

data4 <- data4 %>% group_by(if_bws, ifsmh, fsystem) %>% #if_bws, ifsmh,  fsystem
  summarise(HarvestedArea = median(HarvAreaCropland, na.rm=T) ,
            production = median(prodCropland, na.rm=T) ,
            Energy_kcal = median(EnergyCropland, na.rm=T) ,
            WaterCropland = median(WaterCropland, na.rm=T),
            green_water_m3 = median(green_water_pc, na.rm=T),
            blue_water_m3 = median(blue_water_pc, na.rm=T), 
            Calcium = median(CalciumCropland, na.rm=T),
            Vit_B12 = median(Vit_B12Cropland, na.rm=T),
            Folate = median(FolateCropland, na.rm=T),
            Vit_A = median(Vit_ACropland, na.rm=T),
            Zinc = median(ZincCropland, na.rm=T), 
            Iron = median(IronCropland, na.rm=T),
            Calcium_water = median(CalciumWater, na.rm=T),
            Vit_B12_water = median(Vit_B12Water, na.rm=T),
            Folate_water = median(FolateWater, na.rm=T),
            Vit_A_water = median(Vit_AWater, na.rm=T),
            Zinc_water = median(ZincWater, na.rm=T), 
            Iron_water = median(IronWater, na.rm=T),
            Value = median(Value, na.rm=T), 
            PercentageHarvestedArea = median(PercentageHarvestedArea),
            cou = n())

