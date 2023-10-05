library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggraph)
library(scales)
library(ggpattern)
library(plotly)
library(readxl)


data <- read.csv("~/Library/CloudStorage/Box-Box/1_My_projects/SU Han/forDiana.csv")

data5 <- data[, c("iso3", "if_bws", "ifsmh",  "fsystem", "crop_name", "project_name", "Crop.group", "HarvestedArea", "production_t",
                  "Energy_kcal", "total_water_m3", "green_water_m3", "blue_water_m3", "Iron_mg", "Zinc_mg", 
                  "Vitamin.A..expressed.in.retinol.equivalents._mcg", "Folate..dietary.folate.equivalents..mcg._mcg",
                  "Vitamin.B12_mcg","Calcium_mg")]

data5$iso3 <- as.factor(data$iso3)
data5$if_bws <- as.factor(data5$if_bws)
levels(data5$if_bws) <- list("Water scarcity" = 1, "Water abundance" = 0)
data5$ifsmh <- as.factor(data5$ifsmh)
levels(data5$ifsmh) <- list("Small scale ag." = 1, "Large scale ag." = 0)
data5$fsystem <- as.factor(data5$fsystem)
levels(data5$fsystem) <- list("Irrigated" = "I", "Rainfed" = "H", "Rainfed" = "L", "Rainfed" = "S")
data5$crop_name <- as.factor(data5$crop_name)
data5$project_name <- as.factor(data5$project_name)
data5$Crop.group <- factor(data5$Crop.group, levels=c("Fibres", "FodderCrops", "Oilcrops", "Stimulates", 
                                                      "SugarCrops", "Rest", "Cereals",  "Pulses","RootsTubers", "Vegetables", "Fruits"))

#levels(data5$Crop.group) <- c("Fibres", "FodderCrops", "Oilcrops", "Stimulates", "SugarCrops", "Rest", "Cereals",  "Pulses",
#                              "RootsTubers", "Vegetables", "Fruits")

data5$HarvestedArea <- as.numeric(data5$HarvestedArea)
data5$production_t <- as.numeric(data5$production_t)
data5$Energy_kcal <- as.numeric(data5$Energy_kcal)
data5$total_water_m3 <- as.numeric(data5$total_water_m3)
data5$green_water_m3 <- as.numeric(data5$green_water_m3)
data5$blue_water_m3 <- as.numeric(data5$blue_water_m3)
data5$Calcium <- as.numeric(data5$Calcium_mg)
data5$Vit_B12 <- as.numeric(data5$Vitamin.B12_mcg)
data5$Folate <- as.numeric(data5$Folate..dietary.folate.equivalents..mcg._mcg)
data5$Vit_A <- as.numeric(data5$Vitamin.A..expressed.in.retinol.equivalents._mcg)
data5$Zinc <- as.numeric(data5$Zinc_mg)
data5$Iron <- as.numeric(data5$Iron_mg)

data5 <- data5 %>% group_by(iso3, if_bws, ifsmh, fsystem, project_name, Crop.group) %>% #if_bws, ifsmh,  fsystem
  summarise(HarvestedArea = sum(HarvestedArea) ,
            production_t = sum(production_t) ,
            Energy_kcal = sum(Energy_kcal) ,
            total_water_m3 = sum(total_water_m3),
            green_water_m3 = sum(green_water_m3),
            blue_water_m3 = sum(blue_water_m3), 
            Calcium = sum(Calcium),
            Folate = sum(Folate),
            Vit_A = sum(Vit_A),
            Zinc = sum(Zinc), 
            Iron = sum(Iron))

data6 <- data5 %>% group_by(iso3, if_bws, ifsmh, fsystem) %>% #if_bws, ifsmh,  fsystem
  summarise(HarvestedArea_3class = sum(HarvestedArea) ,
            production_3class = sum(production_t) ,
            Energy_3class = sum(Energy_kcal) ,
            total_water_3class = sum(total_water_m3),
            green_water_m3_3class = sum(green_water_m3),
            blue_water_m3_3class = sum(blue_water_m3), 
            Calcium_3class = sum(Calcium),
            Folate_3class = sum(Folate),
            Vit_A_3class = sum(Vit_A),
            Zinc_3class = sum(Zinc), 
            Iron_3class = sum(Iron))


data7 <- left_join(data5, data6, by=c("iso3", "if_bws", "ifsmh", "fsystem"))


#####

data7$HarvAreaPerc <- data7$HarvestedArea / data7$HarvestedArea_3class
data7$prodPerc <- data7$production_t / data7$production_3class
data7$EnergyPerc <- data7$Energy_kcal / data7$Energy_3class
data7$WaterPerc <- data7$total_water_m3 / data7$total_water_3class
data7$green_waterPerc <- data7$green_water_m3 / data7$total_water_3class
data7$blue_waterPerc <- data7$blue_water_m3 / data7$total_water_3class
data7$CalciumPerc <- data7$Calcium / data7$Calcium_3class
data7$FolatePerc <- data7$Folate / data7$Folate_3class
data7$Vit_APerc <- data7$Vit_A / data7$Vit_A_3class
data7$ZincPerc <- data7$Zinc / data7$Zinc_3class
data7$IronPerc <- data7$Iron / data7$Iron_3class

# data_total <- data7 %>% group_by(iso3) %>%
#   summarise(HarvestedArea_Country = sum(HarvestedArea))
# data7 <- left_join(data7, data_total, by="iso3")
# data7$PercentageHarvestedArea <- data7$HarvestedArea / data7$HarvestedArea_Country * 100
# 
# data7 <- data7[data7$PercentageHarvestedArea > 0.4999, ]

data7 <- data7 %>% group_by(if_bws, ifsmh, fsystem, project_name, Crop.group) %>% #if_bws, ifsmh,  fsystem
  summarise(HarvestedArea = median(HarvAreaPerc, na.rm=T) ,
            production = median(prodPerc, na.rm=T) ,
            Energy_kcal = median(EnergyPerc, na.rm=T) ,
            WaterCropland = median(WaterPerc, na.rm=T),
            green_water_m3 = median(green_waterPerc, na.rm=T),
            blue_water_m3 = median(blue_waterPerc, na.rm=T), 
            Calcium = median(CalciumPerc, na.rm=T),
            Folate = median(FolatePerc, na.rm=T),
            Vit_A = median(Vit_APerc, na.rm=T),
            Zinc = median(ZincPerc, na.rm=T), 
            Iron = median(IronPerc, na.rm=T))

View(data7)

data7$classe <- paste(data7$if_bws, data7$ifsmh, data7$fsystem)
data7$classe <- as.factor(data7$classe)
levels(data7$classe) <- list("WS_S_R" = "Water scarcity Small scale ag. Rainfed", 
                            "WS_L_R" = "Water scarcity Large scale ag. Rainfed",
                            "WS_S_I" = "Water scarcity Small scale ag. Irrigated",
                            "WS_L_I" = "Water scarcity Large scale ag. Irrigated", 
                            "WA_S_R" = "Water abundance Small scale ag. Rainfed",
                            "WA_L_R" = "Water abundance Large scale ag. Rainfed",
                            "WA_S_I" = "Water abundance Small scale ag. Irrigated",
                            "WA_L_I" = "Water abundance Large scale ag. Irrigated")

#grupos <- c("Fibres", "FodderCrops", "Oilcrops", "Stimulates", "SugarCrops", "Rest", "Cereals",  "Pulses",
#             "RootsTubers", "Vegetables", "Fruits")

#cosas <- c("Olive", "Wheat", "Maize", "Barley", "PearlMillet", "ArabicaCoffee", "Sorghum", 
#  "Sunflower", "Pigeonpea", "SmallMillet", "Cassava", "Oilpalm", "Bean", "Rapeseed", 
#  "Taro", "Cashewapple", "Sheanuts", "Groundnut", "Cotton", "Plantains", 
#  "MixedGrassesAndLegumes", "OtherGrassesForForage", "OtherForageProductsNec", 
#  "ForageSilageAlfalfa", "OtherLegumesForForage", "ForageMaize", "ForageAndSilageRyeGrass", 
#  "ForageAndSilageGreenOilseeds", "Pigeonpea", "Fonio", "Rice", "Cassava", "Potato", "Taro", 
#  "Sugarcane", "Quinoa", "Roots", "Mate", "Oat", "Coconut", "Sugarbeet", "Apple")
#data7$crops <- ifelse(data7$project_name %in% cosas, data7$project_name, "Other")

data7$project_name <- as.character(data7$project_name)
data7$crops <- ifelse(data7$HarvestedArea >= 0.01, data7$project_name, "Other")
data7 <- data7 %>% group_by(classe, Crop.group, crops) %>% summarise_if(is.numeric, sum, na.rm=TRUE)

data7 <- data7[order(-data7$HarvestedArea), ]

######


fig <- ggplot(data7, aes(fill=reorder(crops, -HarvestedArea), x=Crop.group, y=HarvestedArea)) +
  geom_bar(stat='identity', color = "black") + 
  facet_grid(.~classe) +
  theme_bw() + 
  scale_x_discrete(expand=expansion(mult=0.15)) + 
  ylab("Harvested area in class (%)") +
  #scale_y_continuous(name="Harvested area (ha)", sec.axis=sec_axis(~.*1, name="Production (ha)")) +
  #ylim(0, 2e09) +
  theme(
    legend.key.width = unit(1, "cm"), legend.title = element_blank(),
    legend.background = element_rect(color = "transparent"), legend.position = "none",
    legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(color=NA),  # remove the border around facets
    panel.spacing = unit(0, 'pt'),  # smoosh facets together
    strip.background = element_rect(color=NA, fill=NA),  # facet label format
    strip.placement = "outside", # get those facet labels outside the axis!
    strip.text.x.bottom = element_text(angle=45),
    axis.line = element_line(),  # add back in the axis lines
    axis.text.x = element_text(angle=90, size=6),  # removes x axis labels
    axis.ticks.x = element_blank(), # removes x axis ticks
    text = element_text(size=11)
  )

ggplotly(fig)

levels(data7$Crop.group)
