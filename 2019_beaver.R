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
library(insol)
library(aspace)

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

# Reformat columns for date and number of images
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

##### NEED TO ACCOUNT FOR IMAGE / DAY # ADD IN IMAGES COLUMN

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

##### NEED TO ACCOUNT FOR IMAGE / DAY # ADD IN IMAGES COLUMN

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
animal.1 <- subset(data, select = c(ID, Images, Date, Week, Month, Time, Hour,
                                    Lake_Frozen, Temp, Precip, Discharge, Height,
                                    Animal_1, Animal_1_Count, Beaver_Count))

animal.2 <- subset(data, select = c(ID, Images, Date, Week, Month, Time, Hour,
                                    Lake_Frozen, Temp, Precip, Discharge, Height,
                                    Animal_2, Animal_2_Count))
animal.2$Beaver_Count <- rep(0)

animal.3 <- subset(data, select = c(ID, Images, Date, Week, Month, Time, Hour,
                                    Lake_Frozen, Temp, Precip, Discharge, Height,
                                    Animal_3, Animal_3_Count))

animal.3$Beaver_Count <- rep(0)

# Rename columns
colnames(animal.1)[c(13:15)] <- c("Animal","Animal_Count", "Beaver")
colnames(animal.2)[c(13:15)] <- c("Animal","Animal_Count", "Beaver")
colnames(animal.3)[c(13:15)] <- c("Animal","Animal_Count", "Beaver")

# Merge dataframes
animals <- rbind(animal.1, animal.2, animal.3)

# Remove unnecessary dataframes
rm(animal.1, animal.2, animal.3)

# Species list
names(table(animals$Animal)) 

# Separate organism and species into different columns (Warning from "Unknown" organisms (No species name available))
animals <- separate(data = animals, col = "Animal", into = c("Organism", "Species"), sep = "_")

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

# Add organism columns
animals$Amphibian <- ifelse(animals$Organism == "Amphibian", animals$Animal_Count, 0)
animals$Bird      <- ifelse(animals$Organism == "Bird", animals$Animal_Count, 0)
animals$Mammal    <- ifelse(animals$Organism == "Mammal", animals$Animal_Count, 0)
animals$Reptile   <- ifelse(animals$Organism == "Reptile", animals$Animal_Count, 0)

# Remove unnecessary columns
animals[,c("Organism", "Animal_Count")] <- NULL

# Create amphibian dataframe
amphibians <- subset(animals, select = c(Date, Images, Week, Month, Time, Hour,
                                         Lake_Frozen, Temp, Precip, Discharge, Height,
                                         Amphibian, AmericanBullfrog))

# Create bird dataframe
birds <- subset(animals, select = c(ID, Images, Date, Week, Month, Time, Hour,
                                    Lake_Frozen, Temp, Precip, Discharge, Height, Species,
                                    Bird, AmericanCoot, BeltedKingfisher,
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
mammals <- subset(animals, select = c(Date, Images, Week, Month, Time, Hour,
                                      Lake_Frozen, Temp, Precip, Discharge, Height,
                                      Mammal, Muskrat, NorthernRiverOtter,
                                      Raccoon, Small))

# Create reptile dataframe
reptiles <- subset(animals, select = c(Date, Images, Week, Month, Time, Hour,
                                       Lake_Frozen, Temp, Precip, Discharge, Height,
                                       Reptile, PaintedTurtle))

# Replace NAs with 0s
animals[is.na(animals)] <- 0

# Create dataframe of daily totals
animals.day <- animals %>%
  group_by(Date) %>%
  summarize(
    Temp                      = (mean(Temp)),
    Discharge                 = (mean(Discharge)),
    Beaver                    = (sum(Beaver) / mean(Images)) * 100,
    AmericanBullfrog          = (sum(AmericanBullfrog) / mean(Images)) * 100,
    AmericanCoot              = (sum(AmericanCoot) / mean(Images)) * 100,
    BeltedKingfisher          = (sum(BeltedKingfisher) / mean(Images)) * 100,
    BlackCrownedNightHeron    = (sum(BlackCrownedNightHeron) / mean(Images)) * 100,
    BlueWingedTeal            = (sum(BlueWingedTeal) / mean(Images)) * 100,
    CanadaGoose               = (sum(CanadaGoose) / mean(Images)) * 100,
    CattleEgret               = (sum(CattleEgret) / mean(Images)) * 100,
    EasternKingbird           = (sum(EasternKingbird) / mean(Images)) * 100,
    EuropeanStarling          = (sum(EuropeanStarling) / mean(Images)) * 100,
    Grackle                   = (sum(Grackle) / mean(Images)) * 100,
    GreatBlueHeron            = (sum(GreatBlueHeron) / mean(Images)) * 100,
    GreatEgret                = (sum(GreatEgret) / mean(Images)) * 100,
    GreatHornedOwl            = (sum(GreatHornedOwl) / mean(Images)) * 100,
    GreenHeron                = (sum(GreenHeron) / mean(Images)) * 100,
    InteriorLeastTern         = (sum(InteriorLeastTern) / mean(Images)) * 100,
    LittleBlueHeron           = (sum(LittleBlueHeron) / mean(Images)) * 100,
    Meadowlark                = (sum(Meadowlark) / mean(Images)) * 100,
    NorthernHarrier           = (sum(NorthernHarrier) / mean(Images)) * 100,
    RedTailedHawk             = (sum(RedTailedHawk) / mean(Images)) * 100,
    RedWingedBlackbird        = (sum(RedWingedBlackbird) / mean(Images)) * 100,
    WoodDuck                  = (sum(WoodDuck) / mean(Images)) * 100,
    YellowCrownedNightHeron   = (sum(YellowCrownedNightHeron) / mean(Images)) * 100,
    YellowHeadedBlackbird     = (sum(YellowHeadedBlackbird) / mean(Images)) * 100,
    YellowWarbler             = (sum(YellowWarbler) / mean(Images)) * 100,
    Muskrat                   = (sum(Muskrat) / mean(Images)) * 100,
    NorthernRiverOtter        = (sum(NorthernRiverOtter) / mean(Images)) * 100,
    Raccoon                   = (sum(Raccoon) / mean(Images)) * 100,
    Small                     = (sum(Small) / mean(Images)) * 100,
    PaintedTurtle             = (sum(PaintedTurtle) / mean(Images)) * 100,
    Amphibian                 = (sum(Amphibian) / mean(Images)) * 100,
    Bird                      = (sum(Bird) / mean(Images)) * 100,
    Mammal                    = (sum(Mammal) / mean(Images)) * 100,
    Reptile                   = (sum(Reptile) / mean(Images)) * 100)

###############################################################
### Animal Activity - Figures
###############################################################

#### Not working??

ggplot(data = animals.day, aes(x = Date)) +
  geom_bar(aes(y = Amphibian), position = "dodge", stat = "identity", fill = "green", size = 0.8) +
  geom_bar(aes(y = Bird), position = "dodge", stat = "identity", fill = "blue", size = 0.8) +
  geom_bar(aes(y = Mammal), position = "dodge", stat = "identity", fill = "orange", size = 0.8) +
  geom_bar(aes(y = Reptile), position = "dodge", stat = "identity", fill = "darkgoldenrod", size = 0.8) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black")) +
  labs(x = "Month",y = "Percent of images")

###############################################################
### ANNUAL CLOCK FOR BEAVER PAPER
###############################################################

# Add Julian days to animals.day
animals.day$Date   <- as.POSIXct(animals.day$Date, format="%Y-%m-%d")
animals.day$Julian <- JD(animals.day$Date, inverse = FALSE)
animals.day$Julian <- (animals.day$Julian - 2457358.5)

# Convert Julian days to degrees
animals.day$Degrees <- (animals.day$Julian/366)*360

# Convery degrees to radians
animals.day$Radians <- as_radians(animals.day$Degrees)

###############################################################
# Beavers

beaver.radians <- subset(animals.day, select = c(Beaver, Radians))
beaver.radians$Radians <- ifelse(beaver.radians$Beaver > 0, beaver.radians$Radians, NA)
beaver.radians <- na.omit(beaver.radians)
beaver.radians <- as.data.frame(beaver.radians$Radians)
beaver.radians <- circular(beaver.radians, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")


pdf("annual_beavers.pdf", width = 12, height = 12)

plot.circular(beaver.radians, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(beaver.radians, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(beaver.radians, bw = 40), zero = pi/2, rotation = "clock")

#beaver.annual.fa <- fitact(beaver.radians, wt = NULL, reps = 1000, bw = NULL, adj = 1, sample = c("data"))

dev.off()

###############################################################
# Amphibians

amphibians.radians <- subset(animals.day, select = c(Amphibian, Radians))
amphibians.radians$Radians <- ifelse(amphibians.radians$Amphibian > 0, amphibians.radians$Radians, NA)
amphibians.radians <- na.omit(amphibians.radians)
amphibians.radians <- as.data.frame(amphibians.radians$Radians)
amphibians.radians <- circular(amphibians.radians, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

pdf("annual_amphibians.pdf", width = 12, height = 12)

plot.circular(amphibians.radians, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(amphibians.radians, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(amphibians.radians, bw = 40), zero = pi/2, rotation = "clock")

#amphibians.annual.fa <- fitact(amphibians.radians, wt = NULL, reps = 1000, bw = NULL, adj = 1, sample = c("data"))

dev.off()

###############################################################
# Birds

birds.radians <- subset(animals.day, select = c(Bird, Radians))
birds.radians$Radians <- ifelse(birds.radians$Bird > 0, birds.radians$Radians, NA)
birds.radians <- na.omit(birds.radians)
birds.radians <- as.data.frame(birds.radians$Radians)
birds.radians <- circular(birds.radians, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

pdf("annual_birds.pdf", width = 12, height = 12)

plot.circular(birds.radians, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(birds.radians, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(birds.radians, bw = 40), zero = pi/2, rotation = "clock")

#birds.annual.fa <- fitact(birds.radians, wt = NULL, reps = 1000, bw = NULL, adj = 1, sample = c("data"))

dev.off()

###############################################################
# Mammals

mammals.radians <- subset(animals.day, select = c(Mammal, Radians))
mammals.radians$Radians <- ifelse(mammals.radians$Mammal > 0, mammals.radians$Radians, NA)
mammals.radians <- na.omit(mammals.radians)
mammals.radians <- as.data.frame(mammals.radians$Radians)
mammals.radians <- circular(mammals.radians, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

pdf("annual_mammals.pdf", width = 12, height = 12)

plot.circular(mammals.radians, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(mammals.radians, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(mammals.radians, bw = 40), zero = pi/2, rotation = "clock")

#mammals.annual.fa <- fitact(mammals.radians, wt = NULL, reps = 1000, bw = NULL, adj = 1, sample = c("data"))

dev.off()

###############################################################
# Reptiles

reptiles.radians <- subset(animals.day, select = c(Reptile, Radians))
reptiles.radians$Radians <- ifelse(reptiles.radians$Reptile > 0, reptiles.radians$Radians, NA)
reptiles.radians <- na.omit(reptiles.radians)
reptiles.radians <- as.data.frame(reptiles.radians$Radians)
reptiles.radians <- circular(reptiles.radians, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

pdf("annual_reptiles.pdf", width = 12, height = 12)

plot.circular(reptiles.radians, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(reptiles.radians, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(reptiles.radians, bw = 40), zero = pi/2, rotation = "clock")

#reptiles.annual.fa <- fitact(reptiles.radians, wt = NULL, reps = 1000, bw = NULL, adj = 1, sample = c("data"))

dev.off()

###############################################################
### DAILY CLOCK FOR BEAVER PAPER
###############################################################

# Convert time of day to radians
animals.hour <- subset(animals, select = c(Time, Beaver, Amphibian, Bird, Mammal, Reptile))
animals.hour$Time <- as.POSIXct(animals.hour$Time, format="%H:%M:%S", units = "mins")
animals.hour$Time <- strptime(animals.hour$Time, "%Y-%m-%d %H:%M:%S")
animals.hour$Time <- ymd_hms(animals.hour$Time)
animals.hour$Time <- as.numeric(format(animals.hour$Time, "%H")) +
  (as.numeric(format(animals.hour$Time, "%M")) / 60)
animals.hour$Time <- (animals.hour$Time * 60 / 229.1831180523)

# Rename Time to Radians (so very similar code works for both clocks)
colnames(animals.hour)[c(1)] <- c("Radians")

###############################################################
# Beavers

beaver.radians2 <- subset(animals.hour, select = c(Beaver, Radians))
beaver.radians2$Radians <- ifelse(beaver.radians2$Beaver > 0, beaver.radians2$Radians, NA)
beaver.radians2 <- na.omit(beaver.radians2)
beaver.radians2 <- as.data.frame(beaver.radians2$Radians)
beaver.radians2 <- circular(beaver.radians2, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

pdf("daily_beavers.pdf", width = 12, height = 12)

plot.circular(beaver.radians2, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(beaver.radians2, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(beaver.radians2, bw = 40), zero = pi/2, rotation = "clock")

#beaver.annual.fa <- fitact(beaver.radians2, wt = NULL, reps = 1000, bw = NULL, adj = 1, sample = c("data"))

dev.off()

###############################################################
# Amphibians

amphibians.radians2 <- subset(animals.hour, select = c(Amphibian, Radians))
amphibians.radians2$Radians <- ifelse(amphibians.radians2$Amphibian > 0, amphibians.radians2$Radians, NA)
amphibians.radians2 <- na.omit(amphibians.radians2)
amphibians.radians2 <- as.data.frame(amphibians.radians2$Radians)
amphibians.radians2 <- circular(amphibians.radians2, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

pdf("daily_amphibians.pdf", width = 12, height = 12)

plot.circular(amphibians.radians2, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(amphibians.radians2, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(amphibians.radians2, bw = 40), zero = pi/2, rotation = "clock")

#amphibians.annual.fa <- fitact(amphibians.radians2, wt = NULL, reps = 1000, bw = NULL, adj = 1, sample = c("data"))

dev.off()

###############################################################
# Birds

birds.radians2 <- subset(animals.hour, select = c(Bird, Radians))
birds.radians2$Radians <- ifelse(birds.radians2$Bird > 0, birds.radians2$Radians, NA)
birds.radians2 <- na.omit(birds.radians2)
birds.radians2 <- as.data.frame(birds.radians2$Radians)
birds.radians2 <- circular(birds.radians2, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

pdf("daily_birds.pdf", width = 12, height = 12)

plot.circular(birds.radians2, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(birds.radians2, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(birds.radians2, bw = 40), zero = pi/2, rotation = "clock")

#birds.annual.fa <- fitact(birds.radians2, wt = NULL, reps = 1000, bw = NULL, adj = 1, sample = c("data"))

dev.off()

###############################################################
# Mammals

mammals.radians2 <- subset(animals.hour, select = c(Mammal, Radians))
mammals.radians2$Radians <- ifelse(mammals.radians2$Mammal > 0, mammals.radians2$Radians, NA)
mammals.radians2 <- na.omit(mammals.radians2)
mammals.radians2 <- as.data.frame(mammals.radians2$Radians)
mammals.radians2 <- circular(mammals.radians2, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

pdf("daily_mammals.pdf", width = 12, height = 12)

plot.circular(mammals.radians2, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(mammals.radians2, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(mammals.radians2, bw = 40), zero = pi/2, rotation = "clock")

#mammals.annual.fa <- fitact(mammals.radians2, wt = NULL, reps = 1000, bw = NULL, adj = 1, sample = c("data"))

dev.off()

###############################################################
# Reptiles

reptiles.radians2 <- subset(animals.hour, select = c(Reptile, Radians))
reptiles.radians2$Radians <- ifelse(reptiles.radians2$Reptile > 0, reptiles.radians2$Radians, NA)
reptiles.radians2 <- na.omit(reptiles.radians2)
reptiles.radians2 <- as.data.frame(reptiles.radians2$Radians)
reptiles.radians2 <- circular(reptiles.radians2, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

pdf("daily_reptiles.pdf", width = 12, height = 12)

plot.circular(reptiles.radians2, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(reptiles.radians2, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)
daily.reptiles.pdf <- lines(density.circular(reptiles.radians2, bw = 40), zero = pi/2, rotation = "clock")

#reptiles.annual.fa <- fitact(reptiles.radians2, wt = NULL, reps = 1000, bw = NULL, adj = 1, sample = c("data"))

dev.off()

###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################








