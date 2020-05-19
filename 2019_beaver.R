###############################################################
# North American Beaver Lodge
# 20191026
# SPT
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
library(lazyeval)
library(iNEXT)
library(plotrix)
library(maptools)
library(purrr)
library(PMCMRplus)

# Set working drive
setwd("/Users/simontye/Documents/Research/Projects/Castor_canadensis/2019_Beaver")

# Load files
beaver <- read.csv("Tye_Beaver.csv")
weather <- read.csv("Tye_Weather.csv")

# Reformat dates
beaver$Date <- as.Date(beaver$Date, "%m/%d/%y")
weather$Date <- as.Date(weather$Date, "%m/%d/%y")

# Group beavers age classes
beaver$Beaver <- (beaver$Beaver_Male + beaver$Beaver_Female + beaver$Beaver_Unknown + beaver$Beaver_Kit)

# Reformat columns for date and number of images
date <- table(beaver$Date)
date <- as.data.frame.table(date) 
colnames(date)[c(1,2)] <- c("Date", "Images")
date$Date <- as.Date(date$Date, "%Y-%m-%d")
beaver <- join(beaver, date, by = 'Date')

# 1) To see all vertebrate observations, keep this portion exlcluded (#),
# then run the code to line 133, which produces a species list
# 2) To run the full analysis with days w/ less than 75% of images
# (> 108 out of the 144 possible), remove the # and run the code.

# Remove days with less than 75% of expected images (>108)
beaver <- subset(beaver, Images > 108)

# Add missing dates
beaver <- complete(data = beaver, Date = seq.Date(min(Date), max(Date), by = "day"))

# Add column for missing dates
beaver <- add_label_missings(data = beaver, Beaver, missing = "-1", complete = "NA")
beaver$any_missing <- as.numeric(as.character(beaver$any_missing))
beaver$Missing_Data <- beaver$any_missing

# Combine beaver and weather dataframes
beaver <- merge(beaver, weather)

# Remove unnecessary dataframes
rm(weather, date)

# Add ID column for image number
beaver <- beaver %>% mutate(ID = row_number())

# Add week column to main beaverframe
beaver$Week <- strftime(beaver$Date, format = "%U")

# Remove Lodge_Condition column
beaver[,c("Filename", "Beaver_Male", "Beaver_Female", "Beaver_Unknown", "Beaver_Kit",
        "Animal", "Amphibian", "Bird", "Mammal", "Reptile",
        "Lake_Frozen", "Lodge_Condition", "Pond_Condition",
        "Precip", "Discharge", "Height", "Temp", "Missing_Data",
        "Hour", "Week", "Maintenance",
        "Maintenance_1", "Maintenance_1V", "Maintenance_1H",
        "Maintenance_2", "Maintenance_2V", "Maintenance_2H",
        "Maintenance_3", "Maintenance_3V", "Maintenance_3H")] <- NULL

###################################################
###################################################
###################################################

# Subset beaver dataframe by lodge materials
beaver1 <- subset(beaver, select = c(ID, Date, Month, Time, Images,
                                     Animal_1, Animal_1_Count))
beaver2 <- subset(beaver, select = c(ID, Date, Month, Time, Images,
                                     Animal_2, Animal_2_Count))
beaver3 <- subset(beaver, select = c(ID, Date, Month, Time, Images,
                                     Animal_3, Animal_3_Count))
beaver4 <- subset(beaver, select = c(ID, Date, Month, Time, Images,
                                     Beaver_Count))

# Avoid tripiling of observations when joining by creating dummy columns with 0s
beaver1$Beaver_Count <- rep(0)
beaver2$Beaver_Count <- rep(0)
beaver3$Beaver_Count <- rep(0)
beaver4$Animal       <- rep(0)
beaver4$Animal_Count <- rep(0)

# Rename columns so dataframes can be merged
colnames(beaver1)[c(6:8)] <- c("Animal", "Animal_Count", "Beaver")
colnames(beaver2)[c(6:8)] <- c("Animal", "Animal_Count", "Beaver")
colnames(beaver3)[c(6:8)] <- c("Animal", "Animal_Count", "Beaver")
colnames(beaver4)[c(6:8)] <- c("Beaver", "Animal", "Animal_Count")

# Merge dataframes
beaver <- rbind(beaver1, beaver2, beaver3, beaver4)

# Remove unnecessary dataframes
rm(beaver1, beaver2, beaver3, beaver4)

# Species list
names(table(beaver$Animal))

# Separate organism and species into different columns (Note: warning is due to "Unknown" organisms as no species name is available)
beaver <- separate(data = beaver, col = "Animal", into = c("Organism", "Species"), sep = "_")

# Create column for each species (w/o Animal_Count taken into account for observation calculations)
beaver$AmericanBullfrog        <- ifelse(beaver$Species == "AmericanBullfrog" & beaver$Animal_Count > 0, 1, 0)
beaver$AmericanCoot            <- ifelse(beaver$Species == "AmericanCoot" & beaver$Animal_Count > 0, 1, 0)
beaver$BeltedKingfisher        <- ifelse(beaver$Species == "BeltedKingfisher" & beaver$Animal_Count > 0, 1, 0)
beaver$BlackCrownedNightHeron  <- ifelse(beaver$Species == "BlackCrownedNightHeron" & beaver$Animal_Count > 0, 1, 0)
beaver$BlueWingedTeal          <- ifelse(beaver$Species == "BlueWingedTeal" & beaver$Animal_Count > 0, 1, 0)
beaver$CanadaGoose             <- ifelse(beaver$Species == "CanadaGoose" & beaver$Animal_Count > 0, 1, 0)
beaver$CattleEgret             <- ifelse(beaver$Species == "CattleEgret" & beaver$Animal_Count > 0, 1, 0)
beaver$CommonGrackle           <- ifelse(beaver$Species == "Grackle" & beaver$Animal_Count > 0, 1, 0)
beaver$EasternKingbird         <- ifelse(beaver$Species == "EasternKingbird" & beaver$Animal_Count > 0, 1, 0)
beaver$EuropeanStarling        <- ifelse(beaver$Species == "EuropeanStarling" & beaver$Animal_Count > 0, 1, 0)
beaver$GreatBlueHeron          <- ifelse(beaver$Species == "GreatBlueHeron" & beaver$Animal_Count > 0, 1, 0)
beaver$GreatHornedOwl          <- ifelse(beaver$Species == "GreatHornedOwl" & beaver$Animal_Count > 0, 1, 0)
beaver$GreenHeron              <- ifelse(beaver$Species == "GreenHeron" & beaver$Animal_Count > 0, 1, 0)
beaver$LittleBlueHeron         <- ifelse(beaver$Species == "LittleBlueHeron" & beaver$Animal_Count > 0, 1, 0)
beaver$Meadowlark              <- ifelse(beaver$Species == "Meadowlark" & beaver$Animal_Count > 0, 1, 0)
beaver$NorthernHarrier         <- ifelse(beaver$Species == "NorthernHarrier" & beaver$Animal_Count > 0, 1, 0)
beaver$RedTailedHawk           <- ifelse(beaver$Species == "RedTailedHawk" & beaver$Animal_Count > 0, 1, 0)
beaver$RedWingedBlackbird      <- ifelse(beaver$Species == "RedWingedBlackbird" & beaver$Animal_Count > 0, 1, 0)
beaver$SnowyEgret              <- ifelse(beaver$Species == "SnowyEgret" & beaver$Animal_Count > 0, 1, 0)
beaver$WoodDuck                <- ifelse(beaver$Species == "WoodDuck" & beaver$Animal_Count > 0, 1, 0)
beaver$YellowCrownedNightHeron <- ifelse(beaver$Species == "YellowCrownedNightHeron" & beaver$Animal_Count > 0, 1, 0)
beaver$YellowHeadedBlackbird   <- ifelse(beaver$Species == "YellowHeadedBlackbird" & beaver$Animal_Count > 0, 1, 0)
beaver$YellowWarbler           <- ifelse(beaver$Species == "YellowWarbler" & beaver$Animal_Count > 0, 1, 0)
beaver$Muskrat                 <- ifelse(beaver$Species == "Muskrat" & beaver$Animal_Count > 0, 1, 0)
beaver$NorthernRiverOtter      <- ifelse(beaver$Species == "NorthernRiverOtter" & beaver$Animal_Count > 0, 1, 0)
beaver$Raccoon                 <- ifelse(beaver$Species == "Raccoon" & beaver$Animal_Count > 0, 1, 0)
beaver$Small                   <- ifelse(beaver$Species == "Small" & beaver$Animal_Count > 0, 1, 0)
beaver$PaintedTurtle           <- ifelse(beaver$Species == "PaintedTurtle" & beaver$Animal_Count > 0, 1, 0)

# Replace NAs with 0s
beaver[is.na(beaver)] <- 0

# Calculate number of observations for each species
sum(beaver$AmericanBullfrog)
sum(beaver$AmericanCoot)
sum(beaver$BeltedKingfisher)
sum(beaver$BlackCrownedNightHeron)
sum(beaver$BlueWingedTeal)
sum(beaver$CanadaGoose)
sum(beaver$CattleEgret)
sum(beaver$CommonGrackle)
sum(beaver$EasternKingbird)
sum(beaver$EuropeanStarling)
sum(beaver$GreatBlueHeron)
sum(beaver$GreatHornedOwl)
sum(beaver$GreenHeron)
sum(beaver$LittleBlueHeron)
sum(beaver$Meadowlark)
sum(beaver$NorthernHarrier)
sum(beaver$RedTailedHawk)
sum(beaver$RedWingedBlackbird)
sum(beaver$SnowyEgret)
sum(beaver$WoodDuck)
sum(beaver$YellowCrownedNightHeron)
sum(beaver$YellowHeadedBlackbird)
sum(beaver$YellowWarbler)
sum(beaver$Muskrat)
sum(beaver$Beaver)
sum(beaver$NorthernRiverOtter)
sum(beaver$Raccoon)
sum(beaver$Small)
sum(beaver$PaintedTurtle)

# Add organism columns by 1) merging data from Organism_Species separation and 2) remove "Unknown" observations
beaver$Amphibian <- ifelse(beaver$Organism == "Amphibian", 1, 0)
beaver$Bird      <- ifelse(beaver$Organism == "Bird", 1, 0)
beaver$Mammal    <- ifelse(beaver$Organism == "Mammal", 1, 0)
beaver$Reptile   <- ifelse(beaver$Organism == "Reptile", 1, 0)
beaver$Beaver    <- ifelse(beaver$Beaver > 0, 1, 0)
beaver$Organism  <- ifelse(beaver$Organism == "Unknown", 0, beaver$Organism)
beaver$Count     <- ifelse(beaver$Amphibian == 1, 1,
                           ifelse(beaver$Bird == 1, 1,
                                  ifelse(beaver$Mammal == 1, 1,
                                         ifelse(beaver$Reptile == 1, 1,
                                                ifelse(beaver$Beaver == 1, 1, 0)))))

# Calculate number of observations for each organism
sum(beaver$Amphibian)
sum(beaver$Bird)
sum(beaver$Mammal)
sum(beaver$Reptile)
sum(beaver$Beaver)

# Remove unnecessary columns
beaver[,c("Animal_Count")] <- NULL

# Change data type of columns
beaver$Beaver <- as.numeric(beaver$Beaver)

# Summarize data by beaver$Date
beaver.day <- beaver %>%
  group_by(Date) %>%
    summarize(
      Month                     = mean(Month),
      Images                    = mean(Images),
      AmericanBullfrog          = (sum(AmericanBullfrog) / mean(Images)) * 100,
      AmericanCoot              = (sum(AmericanCoot) / mean(Images)) * 100,
      BeltedKingfisher          = (sum(BeltedKingfisher) / mean(Images)) * 100,
      BlackCrownedNightHeron    = (sum(BlackCrownedNightHeron) / mean(Images)) * 100,
      BlueWingedTeal            = (sum(BlueWingedTeal) / mean(Images)) * 100,
      CanadaGoose               = (sum(CanadaGoose) / mean(Images)) * 100,
      CattleEgret               = (sum(CattleEgret) / mean(Images)) * 100,
      CommonGrackle             = (sum(CommonGrackle) / mean(Images)) * 100,
      EasternKingbird           = (sum(EasternKingbird) / mean(Images)) * 100,
      EuropeanStarling          = (sum(EuropeanStarling) / mean(Images)) * 100,
      GreatBlueHeron            = (sum(GreatBlueHeron) / mean(Images)) * 100,
      GreatHornedOwl            = (sum(GreatHornedOwl) / mean(Images)) * 100,
      GreenHeron                = (sum(GreenHeron) / mean(Images)) * 100,
      LittleBlueHeron           = (sum(LittleBlueHeron) / mean(Images)) * 100,
      Meadowlark                = (sum(Meadowlark) / mean(Images)) * 100,
      NorthernHarrier           = (sum(NorthernHarrier) / mean(Images)) * 100,
      RedTailedHawk             = (sum(RedTailedHawk) / mean(Images)) * 100,
      RedWingedBlackbird        = (sum(RedWingedBlackbird) / mean(Images)) * 100,
      SnowyEgret                = (sum(SnowyEgret) / mean(Images)) * 100,
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
      Beaver                    = (sum(Beaver) / mean(Images)) * 100,
      Bird                      = (sum(Bird) / mean(Images)) * 100,
      Mammal                    = (sum(Mammal) / mean(Images)) * 100,
      Reptile                   = (sum(Reptile) / mean(Images)) * 100)

# Summarize data by beaver$Month
beaver.month <- beaver.day %>%
  group_by(Month) %>%
  summarize(
    Beaver          = (sum(Beaver) / sum(Images)) * 100,
    Amphibian       = (sum(Amphibian) / sum(Images)) * 100,
    Bird            = (sum(Bird) / sum(Images)) * 100,
    Mammal          = (sum(Mammal) / sum(Images)) * 100,
    Reptile         = (sum(Reptile) / sum(Images)) * 100) 

# Stacked beaver and organism observations by month for annual activity figure
beaver.stacked <- stack(beaver.month, select=c("Beaver", "Amphibian", "Bird", "Mammal", "Reptile"))

# Rename months
beaver.stacked$Month <- rep(1:10, 5)
beaver.stacked$Month <- ifelse(beaver.stacked$Month == 10, 12, beaver.stacked$Month)

# Rename columns
colnames(beaver.stacked)[1:2] <- c("Percent", "Organism")

# Reorder months in study period
beaver.day$Month   <- factor(beaver.day$Month, levels = c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
beaver.month$Month <- factor(beaver.month$Month, levels = c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
beaver.stacked$Month <- factor(beaver.stacked$Month, levels = c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9"))

# Reorder organisms so they stack inversely (Beaver, Amphibian, etc.)
beaver.stacked$Organism <- factor(beaver.stacked$Organism, levels = c("Reptile", "Mammal", "Bird", "Amphibian", "Beaver"))

# Create vector of color values (same colors from clock figure made in Illustrator)
Colors <- c("#b1d689", "#ffa017", "#99c3e8", "#ffe045", "#a18167")

###############################################################
### Animal Activity - Figures
###############################################################

###############################################################
### Annual activity patterns as a stacked bar graph (combined with figure of daily activity patterns in Illustrator)

ggplot(data = beaver.stacked, aes(x = Month, y = Percent, fill = Organism)) +
  geom_bar(position = "stack", stat = "identity", color = "black", size = .25) +
  scale_y_continuous(breaks=c(0, 5, 10, 15, 20), limits = c(0, 20)) +
  scale_fill_manual(values = Colors) +
  theme_bw(base_size = 30) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y.left = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))) +
  labs(x = "Month",y = "Percent of images")

###############################################################
### Annual activity pattern for each organism as separate bar graphs and vertically stacked

# Beavers
annual.beaver <- ggplot(data = beaver.month, aes(x = Month)) +
  geom_bar(aes(y = Beaver), position = "dodge", stat = "identity", fill = "#a18167", size = 0.8) +
  scale_y_continuous(breaks=c(0, 5, 10, 15), limits = c(0, 15)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y.left = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))) +
  labs(x = "",y = "")

# Amphibians
annual.amphibian <- ggplot(data = beaver.month, aes(x = Month)) +
  geom_bar(aes(y = Amphibian), position = "dodge", stat = "identity", fill = "#fff22e", size = 0.8) +
  scale_y_continuous(breaks=c(0, 5, 10, 15), limits = c(0, 15)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y.left = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))) +
  labs(x = "",y = "")

# Birds
annual.bird <- ggplot(data = beaver.month, aes(x = Month)) +
  geom_bar(aes(y = Bird), position = "dodge", stat = "identity", fill = "#99c3e8", size = 0.8) +
  scale_y_continuous(breaks=c(0, 5, 10, 15), limits = c(0, 15)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y.left = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))) +
  labs(x = "",y = "Percent of images")

# Mammals
annual.mammal <- ggplot(data = beaver.month, aes(x = Month)) +
  geom_bar(aes(y = Mammal), position = "dodge", stat = "identity", fill = "#ffa017", size = 0.8) +
  scale_y_continuous(breaks=c(0, 5, 10, 15), limits = c(0, 15)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y.left = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))) +
  labs(x = "",y = "")

# Reptiles
annual.reptile <- ggplot(data = beaver.month, aes(x = Month)) +
  geom_bar(aes(y = Reptile), position = "dodge", stat = "identity", fill = "#b1d689", size = 0.8) +
  scale_y_continuous(breaks=c(0, 5, 10, 5), limits = c(0, 15)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25), axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"), axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y.left = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))) +
  labs(x = "Month",y = "")

# Combine and align plots  (7x16 in)
annual.beaver2    <- ggplotGrob(annual.beaver)
annual.amphibian2 <- ggplotGrob(annual.amphibian)
annual.bird2      <- ggplotGrob(annual.bird)
annual.mammal2    <- ggplotGrob(annual.mammal)
annual.reptile2   <- ggplotGrob(annual.reptile)
annual.all        <- rbind(annual.beaver2, annual.amphibian2, annual.bird2, annual.mammal2,
                           annual.reptile2, size = "first")
annual.all$widths <- unit.pmax(annual.beaver2$widths, annual.amphibian2$widths, annual.bird2$widths,
                               annual.mammal2$widths, annual.reptile2$widths)
grid.newpage()
grid.draw(annual.all)

###############################################################
### Daily activity - Clock figure
###############################################################

# Add Julian days to animals.day
beaver$Date   <- as.POSIXct(beaver$Date, format="%Y-%m-%d")
beaver$Julian <- JD(beaver$Date, inverse = FALSE)
beaver$Julian <- (beaver$Julian - 2457358.5)

# Convert Julian days to degrees
beaver$Degrees <- (beaver$Julian/366)*360

# Convery degrees to radians
beaver$Radians <- as_radians(beaver$Degrees)

###############################################################
# Beavers

beaver.radians <- subset(beaver, select = c(Beaver, Radians))
beaver.radians$Radians <- ifelse(beaver.radians$Beaver > 0, beaver.radians$Radians, NA)
beaver.radians <- na.omit(beaver.radians)
beaver.radians <- as.data.frame(beaver.radians$Radians)
beaver.radians <- circular(beaver.radians, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

### MJ: The "pdf..." and "dev.off" lines are blocked off (#).
###     Activating these lines creates and exports PDFs of the figures.
###     The clocks are disporotional in RStudio but are normal once
###     exported as 12 x 12 in. PDF

#pdf("annual_beavers.pdf", width = 12, height = 12)

plot.circular(beaver.radians, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(beaver.radians, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(beaver.radians, bw = 40), zero = pi/2, rotation = "clock")

#dev.off()

###############################################################
# Amphibians

amphibians.radians <- subset(beaver, select = c(Amphibian, Radians))
amphibians.radians$Radians <- ifelse(amphibians.radians$Amphibian > 0, amphibians.radians$Radians, NA)
amphibians.radians <- na.omit(amphibians.radians)
amphibians.radians <- as.data.frame(amphibians.radians$Radians)
amphibians.radians <- circular(amphibians.radians, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

#pdf("annual_amphibians.pdf", width = 12, height = 12)

plot.circular(amphibians.radians, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(amphibians.radians, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(amphibians.radians, bw = 40), zero = pi/2, rotation = "clock")

#dev.off()

###############################################################
# Birds

birds.radians <- subset(beaver, select = c(Bird, Radians))
birds.radians$Radians <- ifelse(birds.radians$Bird > 0, birds.radians$Radians, NA)
birds.radians <- na.omit(birds.radians)
birds.radians <- as.data.frame(birds.radians$Radians)
birds.radians <- circular(birds.radians, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

#pdf("annual_birds.pdf", width = 12, height = 12)

plot.circular(birds.radians, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(birds.radians, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(birds.radians, bw = 40), zero = pi/2, rotation = "clock")

#dev.off()

###############################################################
# Mammals

mammals.radians <- subset(beaver, select = c(Mammal, Radians))
mammals.radians$Radians <- ifelse(mammals.radians$Mammal > 0, mammals.radians$Radians, NA)
mammals.radians <- na.omit(mammals.radians)
mammals.radians <- as.data.frame(mammals.radians$Radians)
mammals.radians <- circular(mammals.radians, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

#pdf("annual_mammals.pdf", width = 12, height = 12)

plot.circular(mammals.radians, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(mammals.radians, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(mammals.radians, bw = 40), zero = pi/2, rotation = "clock")

#dev.off()

###############################################################
# Reptiles

reptiles.radians <- subset(beaver, select = c(Reptile, Radians))
reptiles.radians$Radians <- ifelse(reptiles.radians$Reptile > 0, reptiles.radians$Radians, NA)
reptiles.radians <- na.omit(reptiles.radians)
reptiles.radians <- as.data.frame(reptiles.radians$Radians)
reptiles.radians <- circular(reptiles.radians, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

#pdf("annual_reptiles.pdf", width = 12, height = 12)

plot.circular(reptiles.radians, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(reptiles.radians, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(reptiles.radians, bw = 40), zero = pi/2, rotation = "clock")

#dev.off()

###############################################################
### Annual activity - Clock figure
###############################################################

# Convert time of day to radians
beaver.hour <- subset(beaver, select = c(Time, Beaver, Amphibian, Bird, Mammal, Reptile))
beaver.hour$Time <- as.POSIXct(beaver.hour$Time, format="%H:%M:%S", units = "mins")
beaver.hour$Time <- strptime(beaver.hour$Time, "%Y-%m-%d %H:%M:%S")
beaver.hour$Time <- ymd_hms(beaver.hour$Time)
beaver.hour$Time <- as.numeric(format(beaver.hour$Time, "%H")) +
  (as.numeric(format(beaver.hour$Time, "%M")) / 60)
beaver.hour$Time <- (beaver.hour$Time * 60 / 229.1831180523)

# Rename Time to Radians (so very similar code works for both clocks)
colnames(beaver.hour)[c(1)] <- c("Radians")

###############################################################
# Beavers

beaver.radians2 <- subset(beaver.hour, select = c(Beaver, Radians))
beaver.radians2$Radians <- ifelse(beaver.radians2$Beaver > 0, beaver.radians2$Radians, NA)
beaver.radians2 <- na.omit(beaver.radians2)
beaver.radians2 <- as.data.frame(beaver.radians2$Radians)
beaver.radians2 <- circular(beaver.radians2, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

#pdf("daily_beavers.pdf", width = 12, height = 12)

plot.circular(beaver.radians2, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(beaver.radians2, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(beaver.radians2, bw = 40), zero = pi/2, rotation = "clock")

#dev.off()

###############################################################
# Amphibians

amphibians.radians2 <- subset(beaver.hour, select = c(Amphibian, Radians))
amphibians.radians2$Radians <- ifelse(amphibians.radians2$Amphibian > 0, amphibians.radians2$Radians, NA)
amphibians.radians2 <- na.omit(amphibians.radians2)
amphibians.radians2 <- as.data.frame(amphibians.radians2$Radians)
amphibians.radians2 <- circular(amphibians.radians2, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

#pdf("daily_amphibians.pdf", width = 12, height = 12)

plot.circular(amphibians.radians2, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(amphibians.radians2, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(amphibians.radians2, bw = 40), zero = pi/2, rotation = "clock")

#dev.off()

###############################################################
# Birds

birds.radians2 <- subset(beaver.hour, select = c(Bird, Radians))
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

#dev.off()

###############################################################
# Mammals

mammals.radians2 <- subset(beaver.hour, select = c(Mammal, Radians))
mammals.radians2$Radians <- ifelse(mammals.radians2$Mammal > 0, mammals.radians2$Radians, NA)
mammals.radians2 <- na.omit(mammals.radians2)
mammals.radians2 <- as.data.frame(mammals.radians2$Radians)
mammals.radians2 <- circular(mammals.radians2, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

#pdf("daily_mammals.pdf", width = 12, height = 12)

plot.circular(mammals.radians2, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(mammals.radians2, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(mammals.radians2, bw = 40), zero = pi/2, rotation = "clock")

#dev.off()

###############################################################
# Reptiles

reptiles.radians2 <- subset(beaver.hour, select = c(Reptile, Radians))
reptiles.radians2$Radians <- ifelse(reptiles.radians2$Reptile > 0, reptiles.radians2$Radians, NA)
reptiles.radians2 <- na.omit(reptiles.radians2)
reptiles.radians2 <- as.data.frame(reptiles.radians2$Radians)
reptiles.radians2 <- circular(reptiles.radians2, units = "radians", template = "clock24", modulo = "2pi", zero = 0, rotation = "clock")

#pdf("daily_reptiles.pdf", width = 12, height = 12)

plot.circular(reptiles.radians2, pch = 16, cex = .7, stack = TRUE, axes = TRUE, sep = 0.04, shrink = 2.5,
              bins = 225, ticks = TRUE, tcl = 0.05, zero = pi/2, template = "clock24")

rose.diag(reptiles.radians2, bins = 24, col = "dark gray", prop = 1.5,add = TRUE, rotation = "clock",
          zero = pi/2, axes = FALSE)

lines(density.circular(reptiles.radians2, bw = 40), zero = pi/2, rotation = "clock")

#dev.off()

###############################################################
### Statistical analysis - Species accumulation curves
###############################################################

# Summarize observations by day
beaver.species.accum <- beaver %>%
  group_by(Date) %>%
  summarize(
    AmericanBullfrog          = (sum(AmericanBullfrog)),
    AmericanCoot              = (sum(AmericanCoot)),
    BeltedKingfisher          = (sum(BeltedKingfisher)),
    BlackCrownedNightHeron    = (sum(BlackCrownedNightHeron)),
    BlueWingedTeal            = (sum(BlueWingedTeal)),
    CanadaGoose               = (sum(CanadaGoose)),
    CattleEgret               = (sum(CattleEgret)),
    CommonGrackle             = (sum(CommonGrackle)),
    EasternKingbird           = (sum(EasternKingbird)),
    EuropeanStarling          = (sum(EuropeanStarling)),
    GreatBlueHeron            = (sum(GreatBlueHeron)),
    GreatHornedOwl            = (sum(GreatHornedOwl)),
    GreenHeron                = (sum(GreenHeron)),
    LittleBlueHeron           = (sum(LittleBlueHeron)),
    Meadowlark                = (sum(Meadowlark)),
    NorthernHarrier           = (sum(NorthernHarrier)),
    RedTailedHawk             = (sum(RedTailedHawk)),
    RedWingedBlackbird        = (sum(RedWingedBlackbird)),
    SnowyEgret                = (sum(SnowyEgret)),
    WoodDuck                  = (sum(WoodDuck)),
    YellowCrownedNightHeron   = (sum(YellowCrownedNightHeron)),
    YellowHeadedBlackbird     = (sum(YellowHeadedBlackbird)),
    YellowWarbler             = (sum(YellowWarbler)),
    Muskrat                   = (sum(Muskrat)),
    NorthernRiverOtter        = (sum(NorthernRiverOtter)),
    Raccoon                   = (sum(Raccoon)),
    Small                     = (sum(Small)),
    PaintedTurtle             = (sum(PaintedTurtle)))

# Create separate dataframes by organism
amphibians.species.accum <- subset(beaver.species.accum, select = c(AmericanBullfrog))
birds.species.accum      <- subset(beaver.species.accum, select = c(AmericanCoot, BeltedKingfisher, BlackCrownedNightHeron,
                                                                    BlueWingedTeal, CanadaGoose, CattleEgret, CommonGrackle,
                                                                    EasternKingbird, EuropeanStarling, GreatBlueHeron,
                                                                    GreatHornedOwl, GreenHeron, LittleBlueHeron, Meadowlark,
                                                                    NorthernHarrier, RedTailedHawk, RedWingedBlackbird,
                                                                    SnowyEgret, WoodDuck, YellowCrownedNightHeron,
                                                                    YellowHeadedBlackbird, YellowWarbler))
mammals.species.accum    <- subset(beaver.species.accum, select = c(Muskrat, NorthernRiverOtter, Raccoon, Small))
reptiles.species.accum   <- subset(beaver.species.accum, select = c(PaintedTurtle))

# Remove date columns for vegan package to function
beaver.species.accum$Date     <- NULL
amphibians.species.accum$Date <- NULL
birds.species.accum$Date      <- NULL
mammals.species.accum$Date    <- NULL
reptiles.species.accum$Date   <- NULL

# Estimate species richness (Jackknife 1 and Jackknife 2 are recommended for incidence data from camera traps (Hortel et al. 2016)
species.accum  <- specpool(beaver.species.accum, smallsample = TRUE)

# Species richness estimates over time
species.accum2 <- poolaccum(beaver.species.accum)

# Transform data for ggplot2
species.accum3 <- summary(species.accum2) %>%
  map(as.data.frame) %>%
  map(rename, V = 2) %>%
  bind_rows(.id = "index")

# Subset out simple species richness, Jackknife 1, and Jackknife 2 estimates
species.accum4 <- subset(species.accum3, index == c("S", "jack1", "jack2"))

# Rename indices
species.accum4$index <- ifelse(species.accum4$index == "S", "Species richness",
                               ifelse(species.accum4$index == "jack1", "First-order Jackknife",
                                      ifelse(species.accum4$index == "jack2", "Second-order Jackknife", NA)))

# Change column names
colnames(species.accum4)[c(4:5)] <- c("ci_lower", "ci.upper")

# Change to numeric
species.accum4$N        <- as.numeric(species.accum4$N)
species.accum4$V        <- as.numeric(species.accum4$V)
species.accum4$ci_upper <- as.numeric(species.accum4$ci.upper)
species.accum4$ci_lower <- as.numeric(species.accum4$ci_lower)

# Create color pallete (ColorBrewer, 3 divergent colors)
cols <- c("Species richness" = "#b2df8a", "First-order Jackknife" = "#a6cee3", "Second-order Jackknife" = "#1f78b4")

# Test plot
ggplot(data = species.accum4, aes(x= N, color = index)) +
  #geom_smooth(aes(y = ci_upper, group = index), formula = "y ~ x", method = "loess", level = 0, size = 0.5, se = FALSE) +
  #geom_smooth(aes(y = ci_lower, group = index), formula = "y ~ x", method = "loess", level = 0, size = 0.5, se = FALSE) +
  geom_line(aes(y = V, group = index), size = 1) +
  scale_color_manual(values = cols, aesthetics = c("color")) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black", size = .25, lineend = "square"),
        axis.ticks = element_line(color = "black", size = .25),
        axis.title = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y.left = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        legend.key = element_rect(fill = NA),
        legend.background = element_rect(color = "black", size = 0.25)) +
  labs(x = "Days", y = "Species richness")

###############################################################
### Statistical analysis - Daily activity patterns
###############################################################

# Convert to numeric
beaver.fit     <- as.numeric(beaver.radians[,1])
amphibians.fit <- as.numeric(amphibians.radians[,1])
birds.fit      <- as.numeric(birds.radians[,1])
mammals.fit    <- as.numeric(mammals.radians[,1])
reptiles.fit   <- as.numeric(reptiles.radians[,1])

# Create activity pattern estimates (model for sample size > 100; data for sample size < 100)
beaver.fit     <- fitact(beaver.fit, sample = "model", reps = 1000)
amphibians.fit <- fitact(amphibians.fit, sample = "model", reps = 1000)
birds.fit      <- fitact(birds.fit, sample = "model", reps = 1000)
mammals.fit    <- fitact(mammals.fit, sample = "data", reps = 1000)
reptiles.fit   <- fitact(reptiles.fit, sample = "model", reps = 1000)

# Wald test
wald.test <- compareAct(list(beaver.fit,
                             amphibians.fit,
                             birds.fit,
                             mammals.fit,
                             reptiles.fit))

# Print results
wald.test

#1v2  0.158476950 0.01563671 102.7167332 0.0000000000 ###
#1v3 -0.050540320 0.01845415   7.5004630 0.0061683133 ###
#1v4  0.022452246 0.04255572   0.2783580 0.5977794667
#1v5 -0.054041579 0.02618064   4.2608436 0.0390005482 ###
#2v3 -0.209017270 0.01096145 363.6040595 0.0000000000 ###
#2v4 -0.136024704 0.03988216  11.6326358 0.0006480466 ###
#2v5 -0.212518529 0.02156441  97.1222220 0.0000000000 ###
#3v4  0.072992567 0.04106867   3.1589007 0.0755140178
#3v5 -0.003501259 0.02368696   0.0218489 0.8824897642
#4v5 -0.076493825 0.04507223   2.8802795 0.0896704538

###############################################################
### Statistical analysis - Annual activity patterns
###############################################################

# Create separate seasons dataframe
beaver.seasons <- beaver

# Summarize data by beaver$Date
beaver.seasons <- beaver.seasons %>%
  group_by(Month) %>%
  summarize(
    Amphibian                 = sum(Amphibian) / sum(Images) * 100,
    Beaver                    = sum(Beaver) / sum(Images) * 100,
    Bird                      = sum(Bird) / sum(Images) * 100,
    Mammal                    = sum(Mammal) / sum(Images) * 100,
    Reptile                   = sum(Reptile) / sum(Images) * 100)

# Create seasons column
beaver.seasons$Season[1:10] <- c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "NA", "Winter")

# Remove NAs (shortened fall study period) and month column
beaver.seasons <- beaver.seasons[-c(9),]
beaver.seasons$Month <- NULL

# Reformat seasons dataset
beaver.seasons <- reshape2::melt(beaver.seasons, id=c("Season"))
colnames(beaver.seasons)[c(2:3)] <- c("Organism", "Frequency")

# Two-way ANOVA
seasons.anova <- aov(Frequency ~ Season * Organism, data = beaver.seasons)

# Summary statistics
summary(seasons.anova)

# Tukey HSD test
TukeyHSD(seasons.anova)

#Fit: aov(formula = Frequency ~ Season * Organism, data = beaver.seasons)

#Season
#diff          lwr           upr     p adj
#Summer-Spring -0.001388966 -0.004105770  0.0013278383 0.4280786
#Winter-Spring -0.003763966 -0.006480770 -0.0010471619 0.0050996 ###
#Winter-Summer -0.002375000 -0.005091804  0.0003418038 0.0957823

#Organism
#diff          lwr          upr     p adj
#Beaver-Amphibian   0.0019509499 -0.002175789  0.006077689 0.6500382
#Bird-Amphibian     0.0081181669  0.003991428  0.012244906 0.0000297 ###
#Mammal-Amphibian  -0.0001931809 -0.004319920  0.003933558 0.9999186
#Reptile-Amphibian  0.0007884502 -0.003338289  0.004915189 0.9805658
#Bird-Beaver        0.0061672170  0.002040478  0.010293956 0.0013265 ###
#Mammal-Beaver     -0.0021441308 -0.006270870  0.001982608 0.5660545
#Reptile-Beaver    -0.0011624997 -0.005289239  0.002964239 0.9232890
#Mammal-Bird       -0.0083113478 -0.012438087 -0.004184609 0.0000204 ###
#Reptile-Bird      -0.0073297167 -0.011456456 -0.003202978 0.0001392 ###
#Reptile-Mammal     0.0009816311 -0.003145108  0.005108370 0.9571239

# Season:Organism
#diff           lwr           upr     p adj
#Spring:Beaver-Spring:Amphibian     5.233302e-03 -3.847352e-03  1.431396e-02 0.7105510
#Spring:Bird-Spring:Amphibian       1.407642e-02  4.995763e-03  2.315707e-02 0.0002596 ###
#Spring:Mammal-Spring:Amphibian     3.979464e-04 -8.682708e-03  9.478601e-03 1.0000000
#Spring:Reptile-Spring:Amphibian    2.522427e-03 -6.558227e-03  1.160308e-02 0.9990481
#Spring:Bird-Spring:Beaver          8.843115e-03 -2.375397e-04  1.792377e-02 0.0623095
#Spring:Mammal-Spring:Beaver       -4.835356e-03 -1.391601e-02  4.245299e-03 0.8036574
#Spring:Reptile-Spring:Beaver      -2.710875e-03 -1.179153e-02  6.369779e-03 0.9979887
#Spring:Mammal-Spring:Bird         -1.367847e-02 -2.275912e-02 -4.597816e-03 0.0004027 ###
#Spring:Reptile-Spring:Bird        -1.155399e-02 -2.063464e-02 -2.473335e-03 0.0040776 ###
#Spring:Reptile-Spring:Mammal       2.124481e-03 -6.956174e-03  1.120514e-02 0.9998572

#Summer:Beaver-Summer:Amphibian    -9.643762e-04 -1.004503e-02  8.116278e-03 1.0000000
#Summer:Bird-Summer:Amphibian       8.940455e-03 -1.401991e-04  1.802111e-02 0.0569694
#Summer:Mammal-Summer:Amphibian    -1.466200e-03 -1.054685e-02  7.614454e-03 0.9999984
#Summer:Reptile-Summer:Amphibian   -1.570765e-04 -9.237731e-03  8.923578e-03 1.0000000
#Summer:Bird-Summer:Beaver          9.904831e-03  8.241771e-04  1.898549e-02 0.0225225 ###
#Summer:Mammal-Summer:Beaver       -5.018240e-04 -9.582478e-03  8.578830e-03 1.0000000
#Summer:Reptile-Summer:Beaver       8.072997e-04 -8.273355e-03  9.887954e-03 1.0000000
#Summer:Mammal-Summer:Bird         -1.040666e-02 -1.948731e-02 -1.326001e-03 0.0135675 ###
#Summer:Reptile-Summer:Bird        -9.097532e-03 -1.817819e-02 -1.687732e-05 0.0492152 ###
#Summer:Reptile-Summer:Mammal       1.309124e-03 -7.771531e-03  1.038978e-02 0.9999996

#Winter:Beaver-Winter:Amphibian     1.583924e-03 -7.496731e-03  1.066458e-02 0.9999958
#Winter:Bird-Winter:Amphibian       1.337629e-03 -7.743026e-03  1.041828e-02 0.9999995
#Winter:Mammal-Winter:Amphibian     4.887111e-04 -8.591943e-03  9.569366e-03 1.0000000
#Winter:Reptile-Winter:Amphibian   -2.818926e-18 -9.080654e-03  9.080654e-03 1.0000000
#Winter:Bird-Winter:Beaver         -2.462951e-04 -9.326950e-03  8.834359e-03 1.0000000
#Winter:Mammal-Winter:Beaver       -1.095213e-03 -1.017587e-02  7.985442e-03 1.0000000
#Winter:Reptile-Winter:Beaver      -1.583924e-03 -1.066458e-02  7.496731e-03 0.9999958
#Winter:Mammal-Winter:Bird         -8.489174e-04 -9.929572e-03  8.231737e-03 1.0000000
#Winter:Reptile-Winter:Bird        -1.337629e-03 -1.041828e-02  7.743026e-03 0.9999995
#Winter:Reptile-Winter:Mammal      -4.887111e-04 -9.569366e-03  8.591943e-03 1.0000000

# Create separate dataframes to rank seasons for each organism
amphibian.seasons <- subset(beaver.seasons, Organism == "Amphibian")
bird.seasons      <- subset(beaver.seasons, Organism == "Bird")
mammal.seasons    <- subset(beaver.seasons, Organism == "Mammal")
reptile.seasons   <- subset(beaver.seasons, Organism == "Reptile")
beaver.seasons    <- subset(beaver.seasons, Organism == "Beaver")

# Change season to factor
amphibian.seasons$Season <- as.factor(amphibian.seasons$Season)
bird.seasons$Season      <- as.factor(bird.seasons$Season)
mammal.seasons$Season    <- as.factor(mammal.seasons$Season)
reptile.seasons$Season   <- as.factor(reptile.seasons$Season)
beaver.seasons$Season    <- as.factor(beaver.seasons$Season)

# Steele-Dwass multiple comparisons
dscfAllPairsTest(Frequency ~ Season, data = amphibian.seasons)
dscfAllPairsTest(Frequency ~ Season, data = bird.seasons)
dscfAllPairsTest(Frequency ~ Season, data = mammal.seasons)
dscfAllPairsTest(Frequency ~ Season, data = reptile.seasons)
dscfAllPairsTest(Frequency ~ Season, data = beaver.seasons)

# One-way ANOVAs by organism
amphibian.anova <- aov(Frequency ~ Season, data = amphibian.seasons)
bird.anova      <- aov(Frequency ~ Season, data = bird.seasons)
mammal.anova    <- aov(Frequency ~ Season, data = mammal.seasons)
reptile.anova   <- aov(Frequency ~ Season, data = reptile.seasons)
beaver.anova    <- aov(Frequency ~ Season, data = beaver.seasons)

# Summary statistics
summary(amphibian.anova)
summary(bird.anova)
summary(mammal.anova)
summary(reptile.anova)
summary(beaver.anova)

# Tukey HSD tests
TukeyHSD(amphibian.anova)
TukeyHSD(bird.anova)
TukeyHSD(mammal.anova)
TukeyHSD(reptile.anova)
TukeyHSD(beaver.anova)

################################################################




