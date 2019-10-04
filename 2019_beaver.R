###############################################################
# North American Beaver Lodge
# 20190903
# SPT, AMS
###############################################################

# Clear memory
rm(list=ls())

# Load packages
library(ggplot2)
library(ggthemes)
library(pastecs)
library(scales)
library(activity)
library(gtable)
library(grid)
library(circular)
library(Hmisc)
library(corrgram)
library(vegan)
library(devtools)
library(ggpmisc)
library(reshape)
library(data.table)
library(tidyr)
library(lubridate)
library(plyr)
library(dplyr)
library(esquisse)
library(zoo)
library(naniar)
library(stringr)

# Set working drive
setwd("/Users/simontye/Documents/Research/Projects/Castor_canadensis/2019_Beaver")

# Load files
beaver <- read.csv("Tye_Beaver.csv")
weather <- read.csv("Tye_Weather.csv")

# Reformat dates
beaver$Date <- as.Date(beaver$Date, "%m/%d/%y")
weather$Date <- as.Date(weather$Date, "%m/%d/%y")

# Group beavers into age classes
beaver$Beaver_Adult <- (beaver$Beaver_Male + beaver$Beaver_Female + beaver$Beaver_Unknown)

# Calculate number of images per day
date <- table(beaver$Date)
date <- as.data.frame.table(date) 
colnames(date)[c(1,2)] <- c("Date", "Images")
date$Date <- as.Date(date$Date, "%Y-%m-%d")
beaver <- join(beaver, date, by = 'Date')

# Remove days with less than 75% of expected images (>108)
beaver <- subset(beaver, Images > 108)

# Combine beaver and weather datasets
data <- merge(beaver, weather)

# Add ID column for image number
data <- data %>% mutate(ID = row_number())

# Add week column to main dataframe
data$Week <- strftime(data$Date, format = "%U")

# Combine "Ice" and "Snow" for Lake_Condition
data$Lake_Frozen  <- ifelse(data$Pond_Condition == "Snow", 1,
                            ifelse(data$Pond_Condition == "Ice", 1,
                                   ifelse(data$Pond_Condition == "Clear", 0, 0)))

# Remove Lodge_Condition column
data[,c("Pond_Condition", "Lodge_Condition")] <- NULL

# Remove unnecessary dataframes
rm(beaver, date)

###############################################################
### Beaver Activity - Data
###############################################################

# Subset main dataframe by beaver observations
beaver <- subset(data, select = c(ID, Images, Date, Week, Month, Time, Hour,
                                  Temp, Precip, Discharge, Lake_Frozen, Height,
                                  Beaver_Adult, Beaver_Kit))

# Create dataframe of daily totals
beaver.day <- beaver %>%
  group_by(Date) %>%
    summarize(
      Beaver_Adult = (sum(Beaver_Adult) / mean(Images)) * 100,
      Beaver_Kit   = (sum(Beaver_Kit) / mean(Images)) * 100,
      Discharge    = mean(Discharge),
      Lake_Frozen  = mean(Lake_Frozen))

# Add missing dates
beaver.day <- complete(data = beaver.day, Date = seq.Date(min(Date), max(Date), by = "day"))

# Add column for missing dates
beaver.day <- add_label_missings(data = beaver.day, Beaver_Adult, Beaver_Kit, missing = "-1", complete = "NA")
beaver.day$any_missing <- as.numeric(as.character(beaver.day$any_missing))

# Create dataframe of weekly totals
beaver.week <- beaver %>%
  group_by(Week) %>%
  summarize(
    Beaver_Adult = (sum(Beaver_Adult) / mean(Images)) * 100,
    Beaver_Kit   = (sum(Beaver_Kit) / mean(Images)) * 100,
    Discharge    = mean(Discharge),
    Lake_Frozen  = mean(Lake_Frozen))

# Create dataframe of monthly totals
beaver.month <- beaver %>%
  group_by(Month) %>%
  summarize(
    Beaver_Adult = (sum(Beaver_Adult) / mean(Images)) * 100,
    Beaver_Kit   = (sum(Beaver_Kit) / mean(Images)) * 100,
    Discharge    = mean(Discharge),
    Lake_Frozen  = mean(Lake_Frozen))

# Assign whether lake is clear / frozen based on the majority of daily observations
beaver.day$Lake_Frozen  <- ifelse(beaver.day$Lake_Frozen > 0, -0.35, NA)
beaver.week$Lake_Frozen <- ifelse(beaver.week$Lake_Frozen > 0, -0.35, NA)
beaver.week$Week <- as.factor(as.character(beaver.week$Week))
beaver.week$Lake_Frozen <- as.numeric(as.logical(beaver.week$Lake_Frozen))

# Reorder weeks and months
beaver.week$Week <- factor(beaver.week$Week, levels = c("48", "49", "50", "51", "00", "01", "02", "03", "04",
                                                        "05", "06", "07", "08", "09", "10", "11", "12", "13",
                                                        "14", "15", "16", "17", "18", "19", "20", "21", "22",
                                                        "23", "24", "25", "26", "27", "28", "29", "30", "31",
                                                        "32", "33", "34", "35", "36"))

beaver.month$Month <- factor(beaver.month$Month, levels = c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9"))

###############################################################
### Beaver Activity - Figures
###############################################################

# AMS: Beaver activity (as proportion of images per day (left axis) compared to discharge from a USGS gauge (right axis).
# Dark / light bars are adults/kits respectively.
# Dark green dots are days when the lake was frozen.
# Light green dots are days w/ missing data (stipulations listed in paper)
# Need to find a better way to represent missing data
# Main takeaway = they appear to be more active (above water) when water levels are high
# They are in a lake, but the water levels flucutate with the river (only separated by a raised gravel road)

# Daily observations with points for frozen land and missing data
ggplot(data = beaver.day, aes(x = Date)) +
  geom_bar(aes(y = Beaver_Kit), position = "dodge", stat = "identity", fill = "burlywood3", size = 1, width = 1) +
  geom_bar(aes(y = Beaver_Adult), position = "dodge", stat = "identity", fill = "burlywood4", size = 1, width = 1) +
  geom_point(aes(y = Lake_Frozen), color = "#006837", size = 1) +
  geom_point(aes(y = any_missing), color = "#78c679", size = 1) +
  geom_line(data = weather, aes(y = Discharge / 15), color = "#43a2ca", size = 0.5) +
  geom_hline(yintercept = 0, size = .25) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(-1, 17), sec.axis = sec_axis(~.* 15, name = expression("Discharge (m"^"3"*"/sec)")), expand = c(0.01, 0.01)) + # -0.5 w/ geom_points
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y.left = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))) +
  labs(x = "Month", y = "Percent of images")

###############################################################
### Lodge Maintenance - Materials - Data
###############################################################

# Subset beaver dataframe by lodge materials
lodge.1 <- subset(data, select = c(ID, Date, Month, Week, Time, Hour, Temp, Precip, Discharge, Height, Lake_Frozen, Maintenance_1, Maintenance_1V, Maintenance_1H))
lodge.2 <- subset(data, select = c(ID, Date, Month, Week, Time, Hour, Temp, Precip, Discharge, Height, Lake_Frozen, Maintenance_2, Maintenance_2V, Maintenance_2H))
lodge.3 <- subset(data, select = c(ID, Date, Month, Week, Time, Hour, Temp, Precip, Discharge, Height, Lake_Frozen, Maintenance_3, Maintenance_3V, Maintenance_3H))

# Rename columns
colnames(lodge.1)[c(12)] <- c("Material")
colnames(lodge.1)[c(13)] <- c("Material_V")
colnames(lodge.1)[c(14)] <- c("Material_H")

colnames(lodge.2)[c(12)] <- c("Material")
colnames(lodge.2)[c(13)] <- c("Material_V")
colnames(lodge.2)[c(14)] <- c("Material_H")

colnames(lodge.3)[c(12)] <- c("Material")
colnames(lodge.3)[c(13)] <- c("Material_V")
colnames(lodge.3)[c(14)] <- c("Material_H")

# Merge dataframes
lodge <- rbind(lodge.1, lodge.2, lodge.3)

# Remove unnecessary dataframes
rm(lodge.1, lodge.2, lodge.3)

# Create separate materials dataset for later use
materials <- lodge

# Create columns for each material type
lodge$Herb    <- ifelse(lodge$Material == "Herbaceous", 1, 0)
lodge$Mud     <- ifelse(lodge$Material == "Mud", 1, 0)
lodge$Wood    <- ifelse(lodge$Material == "Woody", 1, 0)
lodge$Unknown <- ifelse(lodge$Material == "Unknown", 1, 0)

# Aggregate rows by ID (image number)
lodge <- aggregate(.~ID + Date + Month + Week +
                     Temp + Precip + Discharge + Height +
                     Herb + Mud + Wood + Unknown,
                     data = lodge, sum)

# Combine all materials into one column
lodge$All_Materials <- (lodge$Herb +
                          lodge$Mud +
                          lodge$Wood +
                          lodge$Unknown)

# Create dataframe of daily totals
lodge.day <- lodge %>%
  group_by(Date) %>%
  summarize(
    Herb          = sum(Herb),
    Mud           = sum(Mud),
    Wood          = sum(Wood),
    Unknown       = sum(Unknown),
    All_Materials = sum(All_Materials),
    Lake_Frozen   = mean(Lake_Frozen))

# Create dataframe of weekly totals
lodge.week <- lodge %>%
  group_by(Week) %>%
  summarize(
    Herb          = sum(Herb),
    Mud           = sum(Mud),
    Wood          = sum(Wood),
    Unknown       = sum(Unknown),
    All_Materials = sum(All_Materials),
    Lake_Frozen   = mean(Lake_Frozen))

# Create dataframe of monthly totals
lodge.month <- lodge %>%
  group_by(Month) %>%
  summarize(
    Herb          = sum(Herb),
    Mud           = sum(Mud),
    Wood          = sum(Wood),
    Unknown       = sum(Unknown),
    All_Materials = sum(All_Materials),
    Lake_Frozen   = mean(Lake_Frozen))

# Remove unnecessary columns
lodge[,c("Top_L", "Top_M", "Top_R",
             "Mid_L", "Mid_M", "Mid_R",
             "Bot_L", "Bot_M", "Bot_R")] <- NULL

# Remove unnecessary columns
lodge.day[,c("Top_L", "Top_M", "Top_R",
             "Mid_L", "Mid_M", "Mid_R",
             "Bot_L", "Bot_M", "Bot_R")] <- NULL

# Remove unnecessary columns
lodge.week[,c("Top_L", "Top_M", "Top_R",
              "Mid_L", "Mid_M", "Mid_R",
              "Bot_L", "Bot_M", "Bot_R")] <- NULL

# Remove unnecessary columns
lodge.month[,c("Top_L", "Top_M", "Top_R",
               "Mid_L", "Mid_M", "Mid_R",
               "Bot_L", "Bot_M", "Bot_R")] <- NULL

# Reorder weeks and months
lodge.week$Week <- factor(lodge.week$Week, levels = c("48", "49", "50", "51", "00", "01", "02", "03", "04",
                                                        "05", "06", "07", "08", "09", "10", "11", "12", "13",
                                                        "14", "15", "16", "17", "18", "19", "20", "21", "22",
                                                        "23", "24", "25", "26", "27", "28", "29", "30", "31",
                                                        "32", "33", "34", "35", "36"))

lodge.month$Month <- factor(lodge.month$Month, levels = c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9"))

###############################################################
### Lodge Maintenance - Materials - Figures
###############################################################

# AMS: Lodge maintenance (as proportion of images per day (left axis) compared to air temp a NOAA station (right axis).
# Dark / light bars are adults/kits respectively.
# Dark green dots are days when the lake was frozen.
# Light green dots are days w/ missing data (stipulations listed in paper)
# Need to find a better way to represent missing data
# Main takeaway = they add materials as late and early as they can

# Daily observations with points for frozen land and missing data
ggplot(data = lodge.day, aes(x = Date)) +
  geom_bar(aes(y = All_Materials), position = "dodge", stat = "identity", fill = "burlywood4", size = 1, width = 1) +
  geom_point(data = beaver.day, aes(y = Lake_Frozen), color = "#006837", size = 1) +
  geom_point(data = beaver.day, aes(y = any_missing), color = "#78c679", size = 1) +
  geom_line(data = weather, aes(y = Temp / 3), color = "#43a2ca", size = .5) +
  geom_hline(yintercept = 0, size = .25) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(-5, 13), sec.axis = sec_axis(~.* 3, name = expression("Air temperature (°C)")), expand = c(0.01, 0.01)) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y.left = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))) +
  labs(x = "Month",y = "Percent of images")

###############################################################
### Lodge Maintenance - Placement - Data
###############################################################

# Create columns for each placement location
materials$Top_L  <- ifelse(materials$Material_V == "1" & materials$Material_H == "-1", 1, 0)
materials$Top_M  <- ifelse(materials$Material_V == "1" & materials$Material_H == "0", 1, 0)
materials$Top_R  <- ifelse(materials$Material_V == "1" & materials$Material_H == "1", 1, 0)

materials$Mid_L  <- ifelse(materials$Material_V == "0" & materials$Material_H == "-1", 1, 0)
materials$Mid_M  <- ifelse(materials$Material_V == "0" & materials$Material_H == "0", 1, 0)
materials$Mid_R  <- ifelse(materials$Material_V == "0" & materials$Material_H == "1", 1, 0)

materials$Bot_L  <- ifelse(materials$Material_V == "-1" & materials$Material_H == "-1", 1, 0)
materials$Bot_M  <- ifelse(materials$Material_V == "-1" & materials$Material_H == "0", 1, 0)
materials$Bot_R  <- ifelse(materials$Material_V == "-1" & materials$Material_H == "1", 1, 0)

# First transformation
materials$Vertical <- ifelse(materials$Top_L == "1", "Top",
                             ifelse(materials$Top_M == "1", "Top",
                                    ifelse(materials$Top_R == "1", "Top",
                                           ifelse(materials$Mid_L == "1", "Middle",
                                                  ifelse(materials$Mid_M == "1", "Middle",
                                                         ifelse(materials$Mid_R == "1", "Middle",
                                                                ifelse(materials$Bot_L == "1", "Bottom",
                                                                       ifelse(materials$Bot_M == "1", "Bottom",
                                                                              ifelse(materials$Bot_R == "1", "Bottom", 0)))))))))

materials$Horizontal <- ifelse(materials$Top_L == "1", "Left",
                               ifelse(materials$Top_M == "1", "Middle",
                                      ifelse(materials$Top_R == "1", "Right",
                                             ifelse(materials$Mid_L == "1", "Left",
                                                    ifelse(materials$Mid_M == "1", "Middle",
                                                           ifelse(materials$Mid_R == "1", "Right",
                                                                  ifelse(materials$Bot_L == "1", "Left",
                                                                         ifelse(materials$Bot_M == "1", "Middle",
                                                                                ifelse(materials$Bot_R == "1", "Right", 0)))))))))
# Change materials to characters
#materials$Vertical <- as.factor(as.character(materials$Vertical))
#materials$Horizontal <- as.factor(as.character(materials$Horizontal))

# Remove unnecessary columns
materials[,c("Material_V", "Material_H",
         "Top_L", "Top_M", "Top_R",
         "Mid_L", "Mid_M", "Mid_R",
         "Bot_L", "Bot_M", "Bot_R",
         "Date", "Week", "Time", "Hour",
         "Temp", "Precip", "Height",
         "Lake_Frozen", "Discharge",
         "Material", "ID")] <- NULL

# Remove NAs
materials <- na.omit(materials)

#AMS: Need to finish this portion, someday. I originally transformed it in Excel and am having
# trouble doing so in R. Not pertinent to our paper though

###############################################################
### Lodge Maintenance - Placement - Figures
###############################################################

ggplot(data = materials, aes(x = Horizontal, y = Vertical)) +
  geom_tile(aes(fill = as.factor(Horizontal))) +
  facet_wrap(. ~ Month)

###############################################################
### Animal Activity - Data
###############################################################

# Subset main dataframe by animal columns
animal.1 <- subset(data, select = c(ID, Date, Week, Month, Time, Hour,
                                    Lake_Frozen, Temp, Precip, Discharge, Height,
                                    Animal_1, Animal_1_Count))
animal.2 <- subset(data, select = c(ID, Date, Week, Month, Time, Hour,
                                    Lake_Frozen, Temp, Precip, Discharge, Height,
                                    Animal_2, Animal_2_Count))
animal.3 <- subset(data, select = c(ID, Date, Week, Month, Time, Hour,
                                    Lake_Frozen, Temp, Precip, Discharge, Height,
                                    Animal_3, Animal_3_Count))

# Rename columns
colnames(animal.1)[c(12, 13)] <- c("Animal","Animal_Count")
colnames(animal.2)[c(12, 13)] <- c("Animal","Animal_Count")
colnames(animal.3)[c(12, 13)] <- c("Animal","Animal_Count")

# Merge dataframes
animals <- rbind(animal.1, animal.2, animal.3)

# Remove unnecessary dataframes
rm(animal.1, animal.2, animal.3)

# Species list
names(table(animals$Animal)) 

# Separate organism and species into different columns (Warning from "Unknown" organisms (No species name available))
animals <- separate(data = animals, col = "Animal", into = c("Organism", "Species"), sep = "_")

# Remove NAs
animals <- na.omit(animals)

# Create column for each species
animals$AmericanBullfrog <- ifelse(animals$Species == "AmericanBullfrog" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$AmericanCoot <- ifelse(animals$Species == "AmericanCoot" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$BeltedKingfisher <- ifelse(animals$Species == "BeltedKingfisher" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$BlackCrownedNightHeron <- ifelse(animals$Species == "BlackCrownedNightHeron" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$BlueWingedTeal <- ifelse(animals$Species == "BlueWingedTeal" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$CanadaGoose <- ifelse(animals$Species == "CanadaGoose" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$CattleEgret <- ifelse(animals$Species == "CattleEgret" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$EasternKingbird <- ifelse(animals$Species == "EasternKingbird" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$EuropeanStarling <- ifelse(animals$Species == "EuropeanStarling" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$Grackle <- ifelse(animals$Species == "Grackle" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$GreatBlueHeron <- ifelse(animals$Species == "GreatBlueHeron" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$GreatEgret <- ifelse(animals$Species == "GreatEgret" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$GreatHornedOwl <- ifelse(animals$Species == "GreatHornedOwl" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$GreenHeron <- ifelse(animals$Species == "GreenHeron" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$InteriorLeastTern <- ifelse(animals$Species == "InteriorLeastTern" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$LittleBlueHeron <- ifelse(animals$Species == "LittleBlueHeron" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$Meadowlark <- ifelse(animals$Species == "Meadowlark" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$NorthernHarrier <- ifelse(animals$Species == "NorthernHarrier" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$RedTailedHawk <- ifelse(animals$Species == "RedTailedHawk" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$RedWingedBlackbird <- ifelse(animals$Species == "RedWingedBlackbird" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$WoodDuck <- ifelse(animals$Species == "WoodDuck" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$YellowCrownedNightHeron <- ifelse(animals$Species == "YellowCrownedNightHeron" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$YellowHeadedBlackbird <- ifelse(animals$Species == "YellowHeadedBlackbird" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$YellowWarbler <- ifelse(animals$Species == "YellowWarbler" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$Muskrat <- ifelse(animals$Species == "Muskrat" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$NorthernRiverOtter <- ifelse(animals$Species == "NorthernRiverOtter" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$Raccoon <- ifelse(animals$Species == "Raccoon" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$Small <- ifelse(animals$Species == "Small" & animals$Animal_Count > 0, animals$Animal_Count, 0)
animals$PaintedTurtle <- ifelse(animals$Species == "PaintedTurtle" & animals$Animal_Count > 0, animals$Animal_Count, 0)

# Redo organism column
animals$Amphibian <- ifelse(animals$Organism == "Amphibian", 1, 0)
animals$Bird      <- ifelse(animals$Organism == "Bird", 1, 0)
animals$Mammal    <- ifelse(animals$Organism == "Mammal", 1, 0)
animals$Reptile   <- ifelse(animals$Organism == "Reptile", 1, 0)

############################
############################
############################
############################
############################
############################
############################

# Create column for all animal observations
animals$All_Animals <- (animals$All_Amphibians +
                          animals$All_Birds +
                          animals$All_Mammals +
                          animals$All_Reptiles +
                          animals$Unknown)

# Create amphibian dataframe
animals$All_Amphibians <- (animals$AmericanBullfrog +
                             animals$Amphibian)

amphibians <- subset(animals, select = c(Date, Week, Month, Time, Hour,
                                         Lake_Frozen, Temp, Precip, Discharge, Height,
                                         Organism, Species,  All_Amphibians, AmericanBullfrog, Amphibian))

# Create bird dataframe
animals$All_Birds <- (animals$AmericanCoot +
                        animals$BeltedKingfisher +
                        animals$Bird +
                        animals$BlackCrownedNightHeron +
                        animals$BlueWingedTeal +
                        animals$CanadaGoose +
                        animals$CattleEgret +
                        animals$EasternKingbird +
                        animals$EuropeanStarling +
                        animals$Grackle +
                        animals$GreatBlueHeron +
                        animals$GreatEgret +
                        animals$GreatHornedOwl +
                        animals$GreenHeron +
                        animals$InteriorLeastTern +
                        animals$LittleBlueHeron +
                        animals$Meadowlark +
                        animals$NorthernHarrier +
                        animals$RedTailedHawk +
                        animals$RedWingedBlackbird +
                        animals$WoodDuck +
                        animals$YellowCrownedNightHeron +
                        animals$YellowHeadedBlackbird +
                        animals$YellowWarbler)

birds <- subset(animals, select = c(ID, Date, Week, Month, Time, Hour,
                                    Lake_Frozen, Temp, Precip, Discharge, Height,
                                    Organism, Species,  All_Birds, AmericanCoot, BeltedKingfisher,
                                    Bird, BlackCrownedNightHeron, BlueWingedTeal, CanadaGoose,
                                    CattleEgret, EasternKingbird, EuropeanStarling, Grackle,
                                    GreatBlueHeron, GreatEgret, GreatHornedOwl, GreenHeron,
                                    InteriorLeastTern, Meadowlark, NorthernHarrier, RedTailedHawk,
                                    RedWingedBlackbird, WoodDuck, YellowCrownedNightHeron,
                                    YellowHeadedBlackbird, YellowWarbler))

birds$Group <- ifelse(birds$Species == "AmericanCoot", "Waterfowl",
                      ifelse(birds$Species == 'BeltedKingfisher', 'Predator',
                             ifelse(birds$Species == 'BlackCrownedNightHeron', 'Heron',
                                    ifelse(birds$Species == 'BlueWingedTeal', 'Waterfowl',
                                           ifelse(birds$Species == 'BlackCrownedNightHeron', 'Heron',
                                                  ifelse(birds$Species == 'CanadaGoose', 'Waterfowl',
                                                         ifelse(birds$Species == 'CattleEgret', 'Heron',
                                                                ifelse(birds$Species == 'EasternKingbirds', 'Passerine',
                                                                       ifelse(birds$Species == 'EuropeanStarling', 'Passerine',
                                                                              ifelse(birds$Species == 'Grackle', 'Passerine',
                                                                                     ifelse(birds$Species == 'GreatBlueHeron', 'Heron',
                                                                                            ifelse(birds$Species == 'GreatEgret', 'Heron',
                                                                                                   ifelse(birds$Species == 'GreatHornedOwl', 'Predator',
                                                                                                          ifelse(birds$Species == 'GreenHeron', 'Heron',
                                                                                                                 ifelse(birds$Species == 'InteriorLeastTern', 'Other',
                                                                                                                        ifelse(birds$Species == 'Meadowlark', 'Passerine',
                                                                                                                               ifelse(birds$Species == 'NorthernHarrier', 'Predator',
                                                                                                                                      ifelse(birds$Species == 'RedTailedHawk', 'Predator',
                                                                                                                                             ifelse(birds$Species == 'RedWingedBlackird', 'Passerine',
                                                                                                                                                    ifelse(birds$Species == 'YellowHeadedBlackbirds', 'Passerine',
                                                                                                                                                           ifelse(birds$Species == 'YellowWarbler', 'Passerine', 'Other')))))))))))))))))))))
                      
# Create mammal dataframe
animals$All_Mammals <- (animals$Mammal +
                          animals$Muskrat +
                          animals$NorthernRiverOtter +
                          animals$Raccoon +
                          animals$Small)

# Create reptile dataframe
animals$All_Reptiles <- (animals$PaintedTurtle +
                           animals$Reptile)

animals$All_Animals <- (animals$All_Amphibians +
                          animals$All_Birds +
                          animals$All_Mammals +
                          animals$All_Reptiles +
                          animals$Unknown)

# Create organisms dataframe
#organisms <- animals[,c(1:13,48:52)]

# Remove unnecessary columns
animals[,c("Animal_Count")] <- NULL

###############################################################
### Animal Activity - Figures
###############################################################

ggplot(data = animals, aes(x = Week)) +
  geom_bar(aes(y = All_Reptiles), position = "dodge", stat = "identity", fill = "green", size = 0.8) +
  geom_bar(aes(y = All_Mammals), position = "dodge", stat = "identity", fill = "orange", size = 0.8) +
  geom_bar(aes(y = All_Birds), position = "dodge", stat = "identity", fill = "blue", size = 0.8) +
  geom_bar(aes(y = All_Amphibians), position = "dodge", stat = "identity", fill = "red", size = 0.8) +
  geom_line(aes(y = Discharge / 20)) +
  #scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  #scale_y_continuous(limits = c(0, 15), sec.axis = sec_axis(~.* 20, name = expression("Discharge (m"^"3"*"/sec)"))) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black")) +
  labs(x = "Month",y = "Percent of images")

ggplot(data = animals, aes(x = Month)) +
  geom_bar(aes(y = All_Birds, color = Species), position = "dodge", stat = "identity", size = 0.8) +
  geom_line(aes(y = Discharge / 20)) +
  #scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  #scale_y_continuous(limits = c(0, 15), sec.axis = sec_axis(~.* 20, name = expression("Discharge (m"^"3"*"/sec)"))) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black")) +
  labs(x = "Month",y = "Percent of images")

ggplot(data = birds, aes(x = Month)) +
  geom_bar(aes(y = All_Birds, fill = Group), position = "dodge", stat = "identity", size = 0.8) +
  geom_line(aes(y = Discharge / 20)) +
  #scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  #scale_y_continuous(limits = c(0, 15), sec.axis = sec_axis(~.* 20, name = expression("Discharge (m"^"3"*"/sec)"))) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black")) +
  labs(x = "Month",y = "Percent of images")

###############################################################
###############################################################
###############################################################

animals[,c("Date", "Week", "Month", "Time", "Hour",
           "Lake_Frozen", "Temp", "Precip", "Discharge", "Height",
           "Organism", "Species", "Unknown",
           "Amphibian", "Bird", "Mammal", "Reptile",
           "All_Amphibians", "All_Birds", "All_Mammals",
           "All_Reptiles", "All_Animals")] <- NULL

species <- aggregate(.~ID + AmericanBullfrog + AmericanCoot + BeltedKingfisher + BlackCrownedNightHeron +
                        BlueWingedTeal + CanadaGoose + CattleEgret + EasternKingbird + EuropeanStarling +
                        Grackle + GreatBlueHeron + GreatEgret + GreatHornedOwl + GreenHeron + InteriorLeastTern +
                        LittleBlueHeron + Meadowlark + NorthernHarrier + RedTailedHawk + RedWingedBlackbird +
                        WoodDuck + YellowCrownedNightHeron + YellowHeadedBlackbird + YellowWarbler + Muskrat +
                        NorthernRiverOtter + Raccoon + PaintedTurtle, data = animals, sum)


#species2 <- cast(animals, ~ID + AmericanBullfrog + AmericanCoot + BeltedKingfisher + BlackCrownedNightHeron +
                   #BlueWingedTeal + CanadaGoose + CattleEgret + EasternKingbird + EuropeanStarling +
                   #Grackle + GreatBlueHeron + GreatEgret + GreatHornedOwl + GreenHeron + InteriorLeastTern +
                   #LittleBlueHeron + Meadowlark + NorthernHarrier + RedTailedHawk + RedWingedBlackbird +
                   #WoodDuck + YellowCrownedNightHeron + YellowHeadedBlackbird + YellowWarbler + Muskrat +
                   #NorthernRiverOtter + Raccoon + PaintedTurtle, sum)

# Model 1
sa1 <- specaccum(species)
mod1 <- fitspecaccum(sa1, "lomolino")
coef(mod1)
fitted(mod1)
plot(sa1)
plot(mod1, add = TRUE, col = 2, lwd = 2)

# Model 2
sa1 <- specaccum(species, "random")
plot(sa1, col = "hotpink")
boxplot(mod2, col = "yellow", border = "blue", lty = 1, cex = 0.3, add = TRUE)
sapply(mod2$models, AIC)

ggplot(mod2) +
  geom_line(aes(x = sites, y = richness))

## S3 method for class 'specaccum':
plot(species, add = FALSE, ci = 2, ci.type = c("bar", "line", "polygon"), 
     col = par("fg"), ci.col = col, ci.lty = 1, xlab = "ID")

plot(species, add = FALSE, ci = 2, ci.type = c("bar", "line", "polygon"), 
     col = par("fg"), ci.col = col, ci.lty = 1, xlab = "ID")

## S3 method for class 'specaccum':
boxplot(X)

###############################################################
###############################################################
###############################################################

#species <- na.omit(species)

species$Species <- ifelse(species$Species == "GreatHornedOwl", "1",
                      ifelse(species$Species == 'Small', '2',
                             ifelse(species$Species == 'Muskrat', '3',
                                    ifelse(species$Species == 'AmericanCoot', '4',
                                           ifelse(species$Species == 'NorthernRiverOtter', '5',
                                                  ifelse(species$Species == 'RedWingedBlackbird', '6',
                                                         ifelse(species$Species == 'BeltedKingfisher', '7',
                                                                ifelse(species$Species == 'CanadaGoose', '8',
                                                                       ifelse(species$Species == 'BlueWingedTeal', '9',
                                                                              ifelse(species$Species == 'NorthernHarrier', '10',
                                                                                     ifelse(species$Species == 'Raccoon', '11',
                                                                                            ifelse(species$Species == 'RedTailedHawk', '12',
                                                                                                   ifelse(species$Species == 'Grackle', '13',
                                                                                                          ifelse(species$Species == 'YellowHeadedBlackbird', '14',
                                                                                                                 ifelse(species$Species == 'PaintedTurtle', '15',
                                                                                                                        ifelse(species$Species == 'CattleEgret', '16',
                                                                                                                               ifelse(species$Species == 'InteriorLeastTern', '17',
                                                                                                                                      ifelse(species$Species == 'EasternKingbird', '18',
                                                                                                                                             ifelse(species$Species == 'EuropeanStarling', '19',
                                                                                                                                                    ifelse(species$Species == 'LittleBlueHeron', '20',
                                                                                                                                                           ifelse(species$Species == 'GreenHeron', '21',
                                                                                                                                                                  ifelse(species$Species == 'YellowCrownedNightHeron', '22',
                                                                                                                                                                         ifelse(species$Species == 'GreatBlueHeron', '23',
                                                                                                                                                                                ifelse(species$Species == 'WoodDuck', '24',
                                                                                                                                                                                       ifelse(species$Species == 'AmericanBullfrog', '25',
                                                                                                                                                                                              ifelse(species$Species == 'YellowWarbler', '26',
                                                                                                                                                                                                     ifelse(species$Species == 'Meadowlark', '27','0')))))))))))))))))))))))))))
# Change from character to numeric
species$Species <- as.numeric(as.character(species$Species))

# Add ID column for image number
species <- species %>% mutate(ID = row_number())

# Reformat date to days since image acquistion began
species$Date <- julian(species$Date, origin=as.Date("2015-12-03"))

# Subset first observations of each species
species.images <- slice(species, c(14, 127, 4781, 5770, 5693, 8039, 8162, 8252, 9816, 10196,
                                   10353, 10466, 11036, 11575, 11666, 12474, 16123, 17326,
                                   19227, 20372, 20381, 20673, 21229, 23067, 27270, 28061, 30184))

species.day <- slice(species, c(14, 127, 4781, 5770, 5693, 8039, 8162, 8252, 9816, 10196,
                  10353, 10466, 11036, 11575, 11666, 12474, 16123, 17326,
                  19227, 20372, 20381, 20673, 21229, 23067, 27270, 28061, 30184))

# Remove NAs
species <- na.omit(species)

# Remove unnecessary columns and reorder necessary columns
species.accum.day    <- species.day[,-3]
species.accum.images <- species.images[,-1]
species.accum.images <- species.accum.images[c("ID", "Species")]

# Model 1
sa1 <- specaccum(species)
mod1 <- fitspecaccum(sa1, "lomolino")
coef(mod1)
fitted(mod1)
plot(sa1)
plot(mod1, add = TRUE, col = 2, lwd = 2)

# Model 2
mod2 <- specaccum(species, "arrhenius")
plot(mod2, col = "hotpink")
boxplot(sp2, col = "yellow", border = "blue", lty = 1, cex = 0.3, add = TRUE)
sapply(mod2$models, AIC)

## S3 method for class 'specaccum':
plot(species.images.accum, add = FALSE, ci = 2, ci.type = c("bar", "line", "polygon"), 
     col = par("fg"), ci.col = col, ci.lty = 1, xlab = "Species")

plot(species.day.accum, add = FALSE, ci = 2, ci.type = c("bar", "line", "polygon"), 
     col = par("fg"), ci.col = col, ci.lty = 1, xlab = "Days")

## S3 method for class 'specaccum':
boxplot(X)

###############################################################
###############################################################
###############################################################

species <- animals()
data(BCI, BCI.env)
head(BCI.env)

