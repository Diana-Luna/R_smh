library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggraph)
library(scales)
library(ggpattern)
library(plotly)


data <- read.csv("~/Library/CloudStorage/Box-Box/1_My_projects/SU Han/forDiana.csv")

data1 <- data[, c("iso3", "if_bws", "ifsmh",  "fsystem", "crop_name", "HarvestedArea")]

data1$iso3 <- as.factor(data$iso3)

data1$if_bws <- as.factor(data1$if_bws)
levels(data1$if_bws) <- list("Water scarcity" = 1, "Water abundance" = 0)
data1$ifsmh <- as.factor(data1$ifsmh)
levels(data1$ifsmh) <- list("Small scale ag." = 1, "Large scale ag." = 0)
data1$fsystem <- as.factor(data1$fsystem)
levels(data1$fsystem) <- list("Irrigated" = "I", "Rainfed" = "H", "Rainfed" = "L", "Rainfed" = "S")
data1$crop_name <- as.factor(data1$crop_name)

data1$HarvestedArea <- as.numeric(data1$HarvestedArea)

data1 <- data1 %>% group_by(iso3, if_bws, ifsmh,  fsystem) %>%
  summarise(HarvestedArea = sum(HarvestedArea))

data_total <- data1 %>% group_by(iso3, ifsmh) %>%
  summarise(HarvestedArea_Country = sum(HarvestedArea))

data1 <- left_join(data1, data_total, by=c("iso3", "ifsmh"))
data1$PercentageHarvestedArea <- data1$HarvestedArea / data1$HarvestedArea_Country * 100

data1[(data1$if_bws %in% "Water abundance"), "PercentageHarvestedArea"] <-  -1 * data1[(data1$if_bws %in% "Water abundance"), "PercentageHarvestedArea"]

#data1$HarvestedArea <- log1p(data1$HarvestedArea)
data1[(data1$if_bws %in% "Water abundance"), "HarvestedArea"] <-  -1 * data1[(data1$if_bws %in% "Water abundance"), "HarvestedArea"]

data1 <- data1 %>% arrange(PercentageHarvestedArea)
data1 <- data1 %>% arrange(HarvestedArea)


ordenados <- unique(data1[(data1$fsystem %in% "Rainfed") & (data1$ifsmh %in% "Small scale ag.") & (data1$if_bws %in% "Water scarcity"), "iso3"])
ordenados <- ordenados$iso3
ordenados <- as.character(ordenados)
a <- unique(data$iso3)
ordenados_t <- setdiff(a, ordenados)
ordenados <- append(ordenados, ordenados_t)


#ordenados <- c("EST", "LUX", "LVA", "TLS", "IRL", "CZE", "LTU", "FIN", "SVK", "AUT",
#               "POL", "DNK", "HRV", "SVN", "BIH", "NOR", 
#               "DEU", "PRY", "MWI", "SWE", "GBR", "RUS", "BRA", "FRA", "NLD", "ROU",
#               "ALB", "COL", "TZA", "USA", "BGR", "URY", "HUN", "ITA", "BEL", "PRT",
#               "MNG", "GHA", "PAN", "CRI", "UGA", "PER", "TJK", "ESP", "ZAF", "GRC",
#               "MEX", "NGA", "ETH", "IND", "KHM", "CYP", "MLI", "BFA", "NER")


ordenados <- c("LUX", "CYP", "PAN", "TLS", "MNG", "CRI",  "SVN", "ALB", "TJK", "BIH", "IRL", "HRV", "FRA", "DEU",
              "NOR", "PRT", "EST",  "URY", "SVK",  "BEL", "LVA", "AUT", "NLD", "FIN", "DNK",   "CZE", "SWE",  
              "LTU", "GBR", "PRY", "POL",
               "MWI",   "BGR", "HUN", "COL",  "GRC", "ITA",  "ROU", "KHM", "MLI", 
               "TZA", "GHA", "PER",  "BRA", "RUS", "BFA",  "UGA", "ESP",  "ZAF", "MEX", "USA", "ETH", "NER", "NGA",
               "IND")

#data1$iso3 <- factor(data$iso3, levels = ordenados)

######


  
ggplot(data1, aes(fill=ifsmh, y=HarvestedArea, x=ifsmh, pattern=fsystem)) + 
  geom_col_pattern(position="stack",
                   pattern_spacing = 0.2,  # had to fiddle to look right
                   pattern_frequency = 5,
                   #pattern_fill=NA,   # removes gray parts you see between pattern lines
                   pattern_angle = 45,
                   color='white', linewidth=1   # white lines between stacked bars
  ) +
  geom_hline(yintercept = c(25, 75, 100, -25, -75, -100), color="grey", size=0.1) + 
  geom_hline(yintercept = c(0), color="black", size=0.5) + 
  theme_bw() +
  # add facets and push facet labels to the bottom
  facet_grid(.~factor(iso3, levels=ordenados), switch = "x") +
  
  labs(y = "Harvested area under water scarcity by Ag. scale (%)", x="Country") +
  
  # adds some space on the x axis between Category_B values
  scale_x_discrete(expand=expansion(mult=1.0)) +  # adds space between Category_B
  
  scale_fill_manual(values=c("cyan3","brown2")) + #"darkgoldenrod1",
  scale_pattern_manual(values=c('stripe', 'none')) +
  guides(fill = guide_legend(override.aes = list(pattern = c("none", "none")))) +
  
  # format changes noted
  theme(
    legend.key.width = unit(1, "cm"), legend.title = element_blank(),
    legend.background = element_rect(color = "transparent"), legend.position = "right",
    legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(color=NA),  # remove the border around facets
    panel.spacing = unit(0, 'pt'),  # smoosh facets together
    strip.background = element_rect(color=NA, fill=NA),  # facet label format
    strip.placement = "outside", # get those facet labels outside the axis!
    strip.text.x.bottom = element_text(angle=90),
    axis.line = element_line(),  # add back in the axis lines
    axis.text.x = element_blank(),  # removes x axis labels
    axis.ticks.x = element_blank(), # removes x axis ticks
    text = element_text(size=11)
  )


# Harvested
bigbig <- c( "DEU", "RUS", "BRA", "FRA", "USA", "IND", "NGA")
ggplot(data1, #[(data1$iso3 %in% bigbig),]
       aes(fill=ifsmh, y=(HarvestedArea), x=ifsmh, pattern=fsystem)) + 
  geom_col_pattern(position="stack",
                   pattern_spacing = 0.2,  # had to fiddle to look right
                   pattern_frequency = 5,
                   #pattern_fill=NA,   # removes gray parts you see between pattern lines
                   pattern_angle = 45,
                   color='white', linewidth=1   # white lines between stacked bars
  ) +
  #geom_hline(yintercept = c(25, 75, 100, -25, -75, -100), color="grey", size=0.1) + 
  geom_hline(yintercept = c(0), color="black", size=0.5) + 
  theme_bw() +
  # add facets and push facet labels to the bottom
  facet_grid(.~factor(iso3, levels=ordenados), switch = "x") +
  
  labs(y = "Harvested area under water scarcity by Ag. scale (%)", x="Country") +
  
  # adds some space on the x axis between Category_B values
  scale_x_discrete(expand=expansion(mult=1.0)) +  # adds space between Category_B
  
  scale_fill_manual(values=c("cyan3","brown2")) + #"darkgoldenrod1",
  scale_pattern_manual(values=c('stripe', 'none')) +
  guides(fill = guide_legend(override.aes = list(pattern = c("none", "none")))) +
  
  # format changes noted
  theme(
    legend.key.width = unit(1, "cm"), legend.title = element_blank(),
    legend.background = element_rect(color = "transparent"), legend.position = "right",
    legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(color=NA),  # remove the border around facets
    panel.spacing = unit(0, 'pt'),  # smoosh facets together
    strip.background = element_rect(color=NA, fill=NA),  # facet label format
    strip.placement = "outside", # get those facet labels outside the axis!
    strip.text.x.bottom = element_text(angle=90),
    axis.line = element_line(),  # add back in the axis lines
    axis.text.x = element_blank(),  # removes x axis labels
    axis.ticks.x = element_blank(), # removes x axis ticks
    text = element_text(size=11)
  )

######

data2 <- data[, c("iso3", "if_bws", "ifsmh",  "fsystem", "crop_name", "HarvestedArea", "production_t",
                  "Energy_kcal", "total_water_m3", "green_water_m3", "blue_water_m3")]

data2$iso3 <- as.factor(data$iso3)

data2$if_bws <- as.factor(data2$if_bws)
levels(data2$if_bws) <- list("Water scarcity" = 1, "Water abundance" = 0)
data2$ifsmh <- as.factor(data2$ifsmh)
levels(data2$ifsmh) <- list("Small scale ag." = 1, "Large scale ag." = 0)
data2$fsystem <- as.factor(data2$fsystem)
levels(data2$fsystem) <- list("Irrigated" = "I", "Rainfed" = "H", "Rainfed" = "L", "Rainfed" = "S")
data2$crop_name <- as.factor(data2$crop_name)

data2$HarvestedArea <- as.numeric(data2$HarvestedArea)
data2$production_t <- as.numeric(data2$production_t)
data2$Energy_kcal <- as.numeric(data2$Energy_kcal)
data2$total_water_m3 <- as.numeric(data2$total_water_m3)
data2$green_water_m3 <- as.numeric(data2$green_water_m3)
data2$blue_water_m3 <- as.numeric(data2$blue_water_m3)

data2 <- data2 %>% group_by(iso3) %>% #if_bws, ifsmh,  fsystem
  summarise(HarvestedArea = sum(HarvestedArea) ,
            production_t = sum(production_t) ,
            Energy_kcal = sum(Energy_kcal) ,
            total_water_m3 = sum(total_water_m3),
            green_water_m3 = sum(green_water_m3),
            blue_water_m3 = sum(blue_water_m3))


FIG <- ggplot(data2, aes(x=iso3)) +
  geom_point(aes(y=HarvestedArea), color="#F4B9B8") +
  geom_point(aes(y=production_t), color="#887BB0") +
  geom_point(aes(y=Energy_kcal / 1000000), color="#85D2D0") +  #"#FFF4BD"
  # add facets and push facet labels to the bottom
  facet_grid(.~factor(iso3, levels=ordenados), switch = "x") +
  #labs(y = "Harvested area under water scarcity by Ag. scale (%)", x="Country") +
  scale_x_discrete(expand=expansion(mult=1.0)) + 
  #scale_y_continuous(name="Harvested area (ha)", sec.axis=sec_axis(~.*1, name="Production (ha)")) +
  #ylim(0, 2e09) +
  theme(
    legend.key.width = unit(1, "cm"), legend.title = element_blank(),
    legend.background = element_rect(color = "transparent"), legend.position = "right",
    legend.direction = "vertical", legend.spacing.y = unit(0.4, 'cm'),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(color=NA),  # remove the border around facets
    panel.spacing = unit(0, 'pt'),  # smoosh facets together
    strip.background = element_rect(color=NA, fill=NA),  # facet label format
    strip.placement = "outside", # get those facet labels outside the axis!
    strip.text.x.bottom = element_text(angle=90),
    axis.line = element_line(),  # add back in the axis lines
    axis.text.x = element_blank(),  # removes x axis labels
    axis.ticks.x = element_blank(), # removes x axis ticks
    text = element_text(size=11)
  )

ggplotly(FIG)

