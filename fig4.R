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

area_ws <- data3 %>% group_by(iso3, if_bws) %>%
  summarise(HarvestedArea_ws = sum(HarvestedArea),
            total_water_ws = sum(total_water_m3))
#colnames(area_ws) <- c("iso3","if_bws", "HarvestedArea_ws")

data3 <- left_join(data3, area_ws, by=c("iso3", "if_bws"))


#####

data3$percCropland <- data3$Value / totalCropland
data3$HarvAreaCropland <- data3$HarvestedArea / data3$HarvestedArea_ws
data3$prodCropland <- data3$production_t / data3$HarvestedArea
data3$EnergyCropland <- data3$Energy_kcal / data3$HarvestedArea
data3$WaterCropland <- data3$total_water_m3 / data3$HarvestedArea
data3$green_water_pc <- data3$green_water_m3 / data3$total_water_ws
data3$blue_water_pc <- data3$blue_water_m3 / data3$total_water_ws
data3$CalciumCropland <- data3$Calcium / data3$HarvestedArea
data3$Vit_B12Cropland <- data3$Vit_B12 / data3$HarvestedArea
data3$FolateCropland <- data3$Folate / data3$HarvestedArea
data3$Vit_ACropland <- data3$Vit_A / data3$HarvestedArea
data3$ZincCropland <- data3$Zinc / data3$HarvestedArea 
data3$IronCropland <- data3$Iron / data3$HarvestedArea
data3$CalciumWater <- data3$Calcium / data3$total_water_m3
data3$Vit_B12Water <- data3$Vit_B12 / data3$total_water_m3
data3$FolateWater <- data3$Folate / data3$total_water_m3
data3$Vit_AWater <- data3$Vit_A / data3$total_water_m3
data3$ZincWater <- data3$Zinc / data3$total_water_m3
data3$IronWater <- data3$Iron / data3$total_water_m3
data3$CalciumProd <- data3$Calcium / data3$production_t
data3$Vit_B12Prod <- data3$Vit_B12 / data3$production_t
data3$FolateProd <- data3$Folate / data3$production_t
data3$Vit_AProd <- data3$Vit_A / data3$production_t
data3$ZincProd <- data3$Zinc / data3$production_t
data3$IronProd <- data3$Iron / data3$production_t

data_total <- data3 %>% group_by(iso3) %>%
  summarise(HarvestedArea_Country = sum(HarvestedArea))
data3 <- left_join(data3, data_total, by="iso3")
data3$PercentageHarvestedArea <- data3$HarvestedArea / data3$HarvestedArea_Country * 100

data4 <- data3[data3$PercentageHarvestedArea > 0.4999, ]

data4$class <- paste(data4$if_bws, data4$ifsmh, data4$fsystem)
data4$class <- as.factor(data4$class)
levels(data4$class) <- list("WS_S_R" = "Water scarcity Small scale ag. Rainfed", 
                            "WS_L_R" = "Water scarcity Large scale ag. Rainfed",
                            "WS_S_I" = "Water scarcity Small scale ag. Irrigated",
                            "WS_L_I" = "Water scarcity Large scale ag. Irrigated", 
                            "WA_S_R" = "Water abundance Small scale ag. Rainfed",
                            "WA_L_R" = "Water abundance Large scale ag. Rainfed",
                            "WA_S_I" = "Water abundance Small scale ag. Irrigated",
                            "WA_L_I" = "Water abundance Large scale ag. Irrigated")


ggplot(data4, aes(x=class, y=CalciumCropland, fill=class)) +
  geom_violin() + #trim = FALSE
  scale_fill_brewer(palette="Set3") +
  geom_boxplot(width=0.1, fill="white", outlier.size=0) + #outlier.colour="red", outlier.shape=8, outlier.size=4
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) +
  labs(x="", y = "Vit A per tonne") + #title="Plot of length  per dose",
  theme_bw() +
  theme(legend.position= "bottom") #axis.text.x = element_text(angle= 90))

# library(ggpubr)
# p <- ggdensity(data4, x = "Vit_AProd", fill = "class", 
#                palette = "jco", 
#                ggtheme = theme_light(), legend = "top")
# p



##
####

library(vegan)
a <- data4[, c("iso3", "class", "FolateProd", "Vit_AProd", "ZincProd", "IronProd", 
               "CalciumProd", "FolateCropland", "Vit_ACropland", 
               "ZincCropland", "IronCropland", "CalciumCropland")] #, "prodCropland", "EnergyCropland", "WaterCropland"
# "ZincWater", "IronWater", "CalciumWater", "FolateWater", "Vit_AWater",
ids <- a[, c("iso3", "class")]
ids$id <- paste(ids$iso3, ids$class)
a$iso3 <- NULL
a$class <- NULL
rownames(a) <- ids$id

a <- decostand(a, "normalize", 2)
#a <- vegdist(a, "euclidean")

b <- rda(a, scale=FALSE)
# U <- b$CA$v    # is equal to U above
# F <- b$CA$u
# stats::biplot(F, U, cex=1, main= "Hellinger tb-PCA (Crops, 1960)", col=c("#2f5061", "#e57f84"))
# screeplot(b, bstick = TRUE, npcs = 5, col="#2f5061", main="Screeplot")
# vegan::stressplot(b, k=2, p.col="#f2c3b1", l.col="#00556f", xlim=c(0, sqrt(2)), ylim=c(0, sqrt(2)), main="Shepard diagram") 

pc <- as.data.frame(b$CA$u)
pc$iso3 <- ids$iso3
pc$class <- ids$class

fig <- ggplot(pc, aes(x=PC1, y=PC2, color=class, label=iso3)) +
  geom_point() +
  stat_ellipse(aes(alpha=0.3)) + #aes(alpha=ifelse(type %in% "Regions", 0.0, 0.3)),  show.legend=FALSE) + 
  #coord_cartesian(xlim=c(-3.5,3.5), y=c(-3.5,3.5)) + 
  #ggtitle("NMDS/Hellinger distance, Crops, 1960") +
  #ggtitle(paste("NMDS/Hellinger distance - Stress =", round(hel.60.nmds$stress, 3))) +
  #scale_color_manual(values=c("#e57f8460", "#2f5061")) +
  #geom_text(aes(label=ifelse(type %in% "Regions", as.character(Name),'')),hjust=0,vjust=0) +
  theme_bw() #+ 
  #theme(legend.position="none") 
ggplotly(fig)

##
a <- data4[, c("iso3", "class", "FolateProd", "Vit_AProd", "ZincProd", "IronProd", 
               "CalciumProd",  "FolateCropland", "Vit_ACropland", 
               "ZincCropland", "IronCropland", "CalciumCropland")]
# "prodCropland", "EnergyCropland", "WaterCropland", "ZincWater", "IronWater", "CalciumWater", "FolateWater", "Vit_AWater",
ids <- a[, c("iso3", "class")]
ids$id <- paste(ids$iso3, ids$class)
a$iso3 <- NULL
a$class <- NULL
rownames(a) <- ids$id
a <- decostand(a, "normalize")

ver <- metaMDS(a, distance="euclidean", k=2)
ver <- as.data.frame(ver$points)

ver$iso3 <- ids$iso3
ver$class <- ids$class

fig <- ggplot(ver, aes(x=MDS1, y=MDS2, color=class, label=iso3)) +
  geom_point() +
  stat_ellipse(aes(alpha=0.3)) + #aes(alpha=ifelse(type %in% "Regions", 0.0, 0.3)),  show.legend=FALSE) + 
  #coord_cartesian(xlim=c(-3.5,3.5), y=c(-3.5,3.5)) + 
  #ggtitle("NMDS/Hellinger distance, Crops, 1960") +
  #ggtitle(paste("NMDS/Hellinger distance - Stress =", round(hel.60.nmds$stress, 3))) +
  #scale_color_manual(values=c("#e57f8460", "#2f5061")) +
  #geom_text(aes(label=ifelse(type %in% "Regions", as.character(Name),'')),hjust=0,vjust=0) +
  theme_bw() #+ 
  #theme(legend.position="none") 
ggplotly(fig)

##

a <- data4[, c("iso3", "class", "FolateProd", "Vit_AProd", "ZincProd", "IronProd", 
               "CalciumProd", "ZincWater", "IronWater", "CalciumWater", 
               "FolateWater", "Vit_AWater", "FolateCropland", "Vit_ACropland", 
               "ZincCropland", "IronCropland", "CalciumCropland")] #, "prodCropland", "EnergyCropland", "WaterCropland"
ids <- a[, c("iso3", "class")]
ids$id <- paste(ids$iso3, ids$class)
a$iso3 <- NULL
a$class <- NULL
rownames(a) <- ids$id

a <- decostand(a, "normalize", 2)
a <- vegdist(a, "euclidean")
clu <- hclust(a, method = "ward.D2")
dend <- as.dendrogram(clu)

par(mfrow = c(1, 1), cex=0.3)
plot(dend)
