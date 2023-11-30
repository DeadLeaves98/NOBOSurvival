# Title: 
#              ---- NonBreeding Season Adult Survival ----
#              ---------- Encounter History -----------
# Date Created: Autumn Randall 
# Reference: Breeding season known fate data prep 
# Reference Author: Dr. DJ McNeil 
# Last Edited: 10/26/2023
#########################################################################

library(dplyr); library(raster); library(tidyr); library(stringr);library(rgdal); 
library(rgeos); library(sf); library(lubridate); library(stringr)

# DJ pathway
# nobo1 <- read.csv("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/NOBOtelemetry_all_26sept2023_asr.csv")
# Autumn pathway
nobo1 <- read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Telemetry Data/NOBOtelemetry_all_26sept2023_asr.csv")

nrow(nobo1) # 35990
length(unique(nobo1$Bird.ID)) # 961 unique birds

# remove columns we do not need
nobo1 <- dplyr::select(nobo1, -GlobalID, -Time, -Burn.Status, -Habitat.Type,
                       -Associated.Bird..1, -Associated.Bird..2, -Associated.Bird..3,
                       -Associated.Bird..4, -Associated.Bird..5, -Associated.Bird..6,
                       -Associated.Bird..7, -Associated.Bird..8, -Associated.Bird..9,
                       -Associated.Bird..10, -Parent.Present., -Enter.Adult.ID,
                       -Comments, -CreationDate, -Creator, -EditDate, -Editor)

nobo1 <- subset(nobo1, Location.Type != "Chick") # remove chicks
nobo1 <- arrange(nobo1, ObjectID) # re-order rows in order of objectID

#### FIXING/ADJUSTING DATES ####

# reformat date
# Some of the dates contain only a 3 digit year column within the month of october 2022 (EX:"mm/d/202") 
### TO NOTE: I MANUALLY FIXED THESE IN EXCEL THE DATES SO THAT THEY ALL SAY 2022     ###

nobo1$Date = as.POSIXct(nobo1$Date, format = "%m/%d/%Y") # as.Date could not handle single digit date format 
nobo1$Date <- as.Date(nobo1$Date , format = "%m-%d-%Y")
#View(nobo1)

# modifying the dates to something easier to handle
nobodates <- data.frame("Date" = nobo1$Date)
nobodates$Date <- str_replace(nobodates$Date, " AM", "") #replace AM with nothing
nobodates$Date <- str_replace(nobodates$Date, " PM", "") #replace PM with nothing
nobodates <- separate(nobodates, Date, into = c("date", "time"), sep = " ") # split up day and time
# WARNING MESSAGE! "Expected 2 pieces. Missing pieces..." this is b/c some records have no times - just dates
nobodates <- separate(nobodates, date, into = c("year", "month", "day"), sep = "-") # split up month, day, year
unique(nobodates$year) # 2022 and 2023 

nobodates <- separate(nobodates, time, into = c("hour", "min"), sep = ":") # split up hour, minute, and second
nobo2 <- cbind(nobo1, nobodates)

# make month, day, and year numeric
nobo2$month <- as.numeric(nobo2$month); nobo2$day <- as.numeric(nobo2$day); nobo2$year <- as.numeric(nobo2$year)
nobo2$CombinedDate0 <- paste0(nobo2$day, "_", nobo2$month)
length(unique(nobo2$month)) #10

# add ordinal date using the lookup table
lookuptable1 <- read.csv("E:/NOBO Project Data/Analyses/NonBreeding Season/Nonbreeding_Survival/week_lookuptable.csv")
# lookuptable1 <- read.csv("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/week_lookuptable_10oct2023.csv")
nobo_ord <- data.frame("ObjectID" = nobo2$ObjectID,"CombinedDate0" = nobo2$CombinedDate0)
merge1 <- merge(x = nobo_ord, by.x = "CombinedDate0", y = lookuptable1, by.y = "day_month", all.x = TRUE)
merge1 <- merge1 %>% arrange(ObjectID) # re-order columns...  NO FUCKING CLUE why merge() re-orders our columns...
nobo2 <-cbind(nobo2, merge1[,5:8]) # adding breeding season, ordinal, and week
nobo1 <- nobo2 # turn it back into nobo1

length(unique(nobo1$month)) # all 12 months 

#### Chick removal ####
# Some chicks were entered as broods and we need to remove from the data 
nrow(nobo1) #29710 
length(unique(nobo1$Bird.ID)) #821
nobo1$chick = data.frame("band" = substr(nobo1$Bird.ID, 9, 11))

nobosub2022 = nobo1[nobo1$Date >= "2022-01-01" & nobo1$Date <= "2022-12-31", ]
length(unique(nobosub2022$Bird.ID)) #375
nobosub2022 = subset(nobosub2022, chick!=225)
length(unique(nobosub2022$Bird.ID)) #366 

nobosub2023 = nobo1[nobo1$Date >= "2023-01-01" & nobo1$Date <= "2023-12-31", ]
length(unique(nobosub2023$Bird.ID)) #435
nobosub2023 = subset(nobosub2023, chick!=235)
length(unique(nobosub2023$Bird.ID)) #425

nobo2 = rbind(nobosub2022, nobosub2023)
length(unique(nobo2$Bird.ID)) #777
nobo1 = nobo2

#check to see if there are still 12 months 
nrow(nobo1) # 28506
length(unique(nobo1$month)) # 

# 162.603_220346 should actually be called 162.376_220019 so we need to fix that
nobo1$Bird.ID <- str_replace(nobo1$Bird.ID , "162.603_220346", "162.376_220019")

#### Add "encounter" as 0 (censor), 1 (alive), or 2 (dead)
unique(nobo1$Bird.Status)
# alive
alives <- subset(nobo1, Bird.Status == "Alive & Active" | Bird.Status == "Nest" | Bird.Status == "Alive & Inactive" |
                   Bird.Status == "Brood" | Bird.Status == "Alive - Mort Check Only" | Bird.Status == "Suspected Nest") # 13201 rows
# fate (also includes some censors)
fates <- subset(nobo1, Bird.Status == "Fate") # all birds with status = "Fate" = fate
fatedead <- subset(fates, Fate != "Censor") # all fate birds where fate is not "censor", fatedead
fatecensor <- subset(fates, Fate == "Censor") # all fate birds where fate IS "censor", censor
nrow(fatedead) + nrow(fatecensor) == nrow(fates) # make sure the math adds up... should say TRUE
# censors
censors <- subset(nobo1, Bird.Status == "Suspected Fate - RF" | Bird.Status == "Suspected Fate - DNH" | Bird.Status == "Suspected Fate - RIP") # 351 rows
nrow(alives) + nrow(fatedead) + nrow(fatecensor) + nrow(censors) == nrow(nobo1) # 28506 rows
# add encounter
alives$encounter = 1
fatedead$encounter = 2
fatecensor$encounter = 0
censors$encounter = 0
nobo1 <- rbind(alives, fatedead, fatecensor, censors)
nobo1 <- nobo1 %>% arrange(ObjectID) # re-order columns

length(unique(nobo1$Bird.ID)) #776 -- removed the bird called "162.603_220346"


# clean nobo1 up by removing extra columns
nobo1 <- dplyr::select(nobo1, -day, -hour, -min, -CombinedDate0) # removed the "-Date" and "-month" so that I can make sure it includes only nonbreeding 
nrow(nobo1) # 28506
length(unique(nobo1$Bird.ID)) #776
length(unique(nobo1$month))

# filter to be both non-breeding seasons 
nonbreeding1 = subset(nobo1, breedingseason == 0) # filters for BOTH nonbreeding seasons 
nrow(nonbreeding1) # 4677
length(unique(nonbreeding1$Bird.ID)) #350... seems kinda low?? pivot table suggested there should be more like 410 birds
length(unique(nonbreeding1$month)) #6 months 


# code to find out where  and 164.310_230208 disappear to...
#mbrd1 <- data.frame("a" = unique(nonbreeding1$Bird.ID))
subset(nonbreeding1, Bird.ID == "164.310_230208")

# Making a column for "nonbreeding season ID" (0 or 1)
# 0 = winter of 2021-2022 [but we have no 2021 data]
# 1 = winter of 2022-2023... straightforward


nonbreedingsub0 = nonbreeding1[nonbreeding1$Date >= "2022-01-01" & nonbreeding1$Date <= "2022-03-31", ] # no data for winter of 2021
length(unique(nonbreedingsub0$month))
nonbreedingsub1 = nonbreeding1[nonbreeding1$Date >= "2022-10-01" & nonbreeding1$Date <= "2023-03-31", ] # this is from oct 10 2022 - march 31 2023 
length(unique(nonbreedingsub1$month)) # lacking months 11 and 12 (nov and dec)

#explore nonbreeding1 again.... 
# View(nonbreeding1)
x = nonbreeding1[nonbreeding1$Date >= "2022-10-01" & nonbreeding1$Date <= "2022-12-31", ]
#View(x)

nonbreedingsub0$year = 0
nonbreedingsub1$year = 1

test = rbind(nonbreedingsub0, nonbreedingsub1)
test # double checked to see if it ran properly 
nonbreeding1 = test #change back to the OG name 
nrow(nonbreeding1) #4672
length(unique(nonbreeding1$Bird.ID)) # n = 348; 164.310_230208 was fated on APRIL 1st [its only loc point], 162.685_231011 has 0 loc during nonbreeding season
length(unique(nonbreeding1$month)) # 4... why does this remove two months of data still... 

# mbrd1$b <- c(unique(nonbreeding1$Bird.ID), 0, 0)
# write.csv(mbrd1, "C:/Users/ASRA248/Desktop/mbrd1.csv", row.names = FALSE)

## Extract covariate values at each point
nonbreeding_sp <- SpatialPoints(coords = data.frame("x" = nonbreeding1$x, "y" = nonbreeding1$y)) # convert DF to Spatial Points
crs(nonbreeding_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)

courses <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/shapefiles/OrtonCourses.shp")
# courses <- readOGR("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/shapefiles/OrtonCourses.shp")
#plot(courses)
crs(courses) # NAD83

# read in rasters
#list.files("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/")
#list.files("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters")

#NDVI <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/2019NDVI.tif")
#perc_mpine <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/PercentMaturePine.tif")
#perc_grassy <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/PercentGrassy.tif")
#perc_decid <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/PercentDeciduous.tif")
#perc_bf <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/PercentBroodField.tif")
#perc_water <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/PercentWater.tif")
#DTN_road <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/DTN_road.tif")
#DTN_mpine <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/DTN_maturepine.tif")
#DTN_grassy <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/DTN_grassy.tif")
#DTN_decid <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/DTN_deciduous.tif")
#DTN_bf <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/DTN_broodfield1.tif")
#DTN_water <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/DTN_water.tif")
#burn2022 <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/BurnStat2022.tif")
#burn2023 <- raster("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters/BurnStat2023.tif")

# list.files("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/rasters")

NDVI <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/2019NDVI.tif")
perc_mpine <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentMaturePine.tif")
perc_grassy <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentGrassy.tif")
perc_decid <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentDeciduous.tif")
perc_bf <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentBroodField.tif")
perc_water <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentWater.tif")
DTN_road <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_road.tif")
DTN_mpine <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_maturepine.tif")
DTN_grassy <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_grassy.tif")
DTN_decid <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_deciduous.tif")
DTN_bf <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_broodfield1.tif")
DTN_water <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_water.tif")
burn2022 <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/BurnStat2022.tif")
burn2023 <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/BurnStat2023.tif")

# Adding binary burn status
burnBinary2022 <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/rasters/BurnStat2022_binary.tif")
burnBinary2023 <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/rasters/BurnStat2023_binary.tif")

# reproject "breeding" to match NDVI (and percent cover raster) and extract
nonbreeding_sp1 <- spTransform(nonbreeding_sp, crs(NDVI)) # transform to NAD83
# plot(NDVI)
# plot(breeding_sp1, add = TRUE)
ndv1_ex <- raster::extract(x = NDVI, y = nonbreeding_sp1)
perc_mpine_ex <- raster::extract(x = perc_mpine, y = nonbreeding_sp1)
perc_grassy_ex <- raster::extract(x = perc_grassy, y = nonbreeding_sp1)
perc_decid_ex <- raster::extract(x = perc_decid, y = nonbreeding_sp1)
perc_bf_ex <- raster::extract(x = perc_bf, y = nonbreeding_sp1)
perc_water_ex <- raster::extract(x = perc_water, y = nonbreeding_sp1)

# reproject "breeding" to match DTN rasters and extract
nonbreeding_sp2 <- spTransform(nonbreeding_sp, crs(DTN_road)) # transform to pseudo-mercator
plot(DTN_road); plot(nonbreeding_sp2, add = TRUE)
DTN_road_ex <- raster::extract(x = DTN_road, y = nonbreeding_sp2)
DTN_mpine_ex <- raster::extract(x = DTN_mpine, y = nonbreeding_sp2)
DTN_grassy_ex <- raster::extract(x = DTN_grassy, y = nonbreeding_sp2)
DTN_decid_ex <- raster::extract(x = DTN_decid, y = nonbreeding_sp2)
DTN_bf_ex <- raster::extract(x = DTN_bf, y = nonbreeding_sp2)
DTN_water_ex <- raster::extract(x = DTN_water, y = nonbreeding_sp2)

# reproject "nonbreeding" to match "courses" shapefile and extract with over()
crs(courses) # NAD83
crs(nonbreeding_sp)#WGS 84
nonbreeding_sp3 <- spTransform(nonbreeding_sp, crs(courses)) # transform to pseudo-mercator

plot(courses); plot(nonbreeding_sp3, add = TRUE) # plot to check if they match
course_ex <- over(x = nonbreeding_sp3, y = courses) # extract course for each point 

# add columns to breeding and export
nonbreeding1$ndvi <- ndv1_ex
nonbreeding1$perc_mpine <- perc_mpine_ex
nonbreeding1$perc_grassy <- perc_grassy_ex
nonbreeding1$perc_decid <- perc_decid_ex
nonbreeding1$perc_bf <- perc_bf_ex
nonbreeding1$perc_water <- perc_water_ex
nonbreeding1$DTN_road <- DTN_road_ex
nonbreeding1$DTN_mpine <- DTN_mpine_ex
nonbreeding1$DTN_grassy <- DTN_grassy_ex
nonbreeding1$DTN_decid <- DTN_decid_ex
nonbreeding1$DTN_bf <- DTN_bf_ex
nonbreeding1$DTN_water <- DTN_water_ex
nonbreeding1$course <- course_ex$course 

# fix NAs in course ID
sum(is.na(nonbreeding1$course)) # 5
nonbreeding1$course[is.na(nonbreeding1$course)] <- "unknown"

# Creating "bush4" from courseID (billjones, darkbranch, and fencecove are "1" for bush4)
nonbreeding1$bush4 <- ifelse(nonbreeding1$course == "billjones" |
                            nonbreeding1$course == "darkbranch" | 
                            nonbreeding1$course == "fencecove", 1, 0)
unique(nonbreeding1$bush4)

# Creating "blower" from courseID (billjones, bigbay, and bluepond are "1" for blower)
nonbreeding1$blower <- ifelse(nonbreeding1$course == "billjones" |
                            nonbreeding1$course == "bigbay" | 
                            nonbreeding1$course == "bluepond", 1, 0)
unique(nonbreeding1$blower)

#  replace NAs in NDVI and percent_...; DTN- should always have values
  # sum(is.na(nonbreeding1$perc_mpine))
nonbreeding1$ndvi[is.na(nonbreeding1$ndvi)] <- 0
nonbreeding1$perc_mpine[is.na(nonbreeding1$perc_mpine)] <- 0
nonbreeding1$perc_grassy[is.na(nonbreeding1$perc_grassy)] <- 0
nonbreeding1$perc_decid[is.na(nonbreeding1$perc_decid)] <- 0
nonbreeding1$perc_bf[is.na(nonbreeding1$perc_bf)] <- 0
nonbreeding1$perc_water[is.na(nonbreeding1$perc_water)] <- 0
#View(breeding1)
nrow(nonbreeding1) # 4672

unique(nonbreeding1$course)

# assign "burn status" and "perc_burn" to each individual point
# For the nonbreeding season I will need to subset by date for each year 
# the nonbreeding season overlaps 'year' [winter of 2022 - 2023] 
# Burn map changes each year so will need to subset the data to correspond with the given year for each map

# subset to seperate by year 
nonbreedingsub2022= nonbreeding1[nonbreeding1$Date >= "2022-01-01" & nonbreeding1$Date <= "2022-12-31", ] # year 2022


nonbreedingsub2023= nonbreeding1[nonbreeding1$Date >= "2023-01-01" & nonbreeding1$Date <= "2023-12-31", ] # year 2023

birds22_sp <- SpatialPoints(coords = data.frame("x" = nonbreedingsub2022$x, "y" = nonbreedingsub2022$y)) # convert DF to Spatial Points
birds23_sp <- SpatialPoints(coords = data.frame("x" = nonbreedingsub2023$x, "y" = nonbreedingsub2023$y)) # convert DF to Spatial Points

crs(birds22_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc.)
crs(birds23_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc.)

birds22_sp = spTransform(birds22_sp, crs(burn2022)) # reproject birds 2022 and 2023 and extract
birds23_sp = spTransform(birds23_sp, crs(burn2023))
crs(birds22_sp)

# extract "percent burn"
perc_burn22_ex <- raster::extract(x = burn2022, y = birds22_sp) 
perc_burn23_ex <- raster::extract(x = burn2023, y = birds23_sp)

# extract "burn_status" aka binary burn stat
plot(burnBinary2022); plot(birds22_sp, add = TRUE) # just making sure it looks OK
burn_stat22_ex <- raster::extract(x = burnBinary2022, y = birds22_sp) 
burn_stat23_ex <- raster::extract(x = burnBinary2023, y = birds23_sp)

# add columns 
nonbreedingsub2022$perc_burn <- perc_burn22_ex # percent burn, year 2022
nonbreedingsub2022$burn_stat <- burn_stat22_ex # burn status, year 2022

nonbreedingsub2023$perc_burn <- perc_burn23_ex # percent burn, year 2023
nonbreedingsub2023$burn_stat <- burn_stat23_ex # burn status, year 2023

# replace any NA
nonbreedingsub2022$perc_burn[is.na(nonbreedingsub2022$perc_burn)] <- 0
nonbreedingsub2023$perc_burn[is.na(nonbreedingsub2023$perc_burn)] <- 0
nonbreedingsub2022$burn_stat[is.na(nonbreedingsub2022$burn_stat)] <- 0
nonbreedingsub2023$burn_stat[is.na(nonbreedingsub2023$burn_stat)] <- 0
unique(nonbreedingsub2022$perc_burn); unique(nonbreedingsub2022$burn_stat)
unique(nonbreedingsub2023$perc_burn); unique(nonbreedingsub2023$burn_stat)

# now rbind them back together and call it nonbreeding1 
dat00 = rbind(nonbreedingsub2022,nonbreedingsub2023)
nrow(dat00) #4672
length(unique(dat00$Bird.ID)) #348 
nonbreeding1 = dat00 # Rename to the OG name 

#### For() loop that creates input file
birdlist <- unique(nonbreeding1$Bird.ID) # the list of birds to be processed
length(unique(nonbreeding1$Bird.ID)) # how many? n = 394
nonbreeding1 <- nonbreeding1[order(nonbreeding1$year),] # reorder by year
nonbreeding1 <- nonbreeding1[order(nonbreeding1$week),] # then reorder by week
length(unique(nonbreeding1$week)) #.27

nrow(nonbreeding1)

# adjusted for nonbreeding weeks: Week 1 - 13 and week 40 - 53
inp_df <- data.frame("birdID" = NA, 
                     "wk40" = NA, "wk41" = NA,
                     "wk42" = NA, "wk43" = NA, "wk44" = NA, "wk45" = NA,
                     "wk46" = NA, "wk47" = NA, "wk48" = NA, "wk49" = NA,
                     "wk50" = NA, "wk51" = NA, "wk52" = NA, "wk53" = NA,
                     "wk1" = NA, "wk2" = NA, "wk3" = NA, "wk4" = NA,
                     "wk5" = NA, "wk6" = NA, "wk7" = NA, "wk8" = NA,
                     "wk9" = NA, "wk10" = NA, "wk11" = NA, "wk12" = NA, "wk13" = NA,
                     "ndvi" = NA, "perc_mpine" = NA, "perc_grassy" = NA, "perc_decid" = NA, 
                     "perc_bf" = NA, "perc_water" = NA, "DTN_road" = NA, "DTN_mpine" = NA, 
                     "DTN_grassy" = NA, "DTN_decid" = NA, "DTN_bf" = NA, "DTN_water" = NA,
                     "perc_burn" = NA,
                     "course" = NA, "year" = NA, "x" = NA, "y" = NA, 
                     "TrackedAlive" = NA)

NBweeks <- c(40:53, 1:13) # make a "sequential" list of weeks

#View(subset(nonbreeding1, Bird.ID == "166.346_204659")) # this bird is a good "test" bird

for(i in 1:length(birdlist)){ # for every bird in the bird list...
  #i = 119 # bird 119 is our golden child, "166.346_204659"
  bird_i_data <- subset(nonbreeding1, Bird.ID == birdlist[i]) # subset data to get bird i's data
  bird_i_vals <- c() # make object to hold values
  # bird_i_data # print bird i's data if we want...
  
   for(j in 1:length(NBweeks)){ # for every week in the week list (NBweeks)...
    # this generates a detection history of sorts; 0 = not tracked, 1 = alive, 2 = dead, NA = not tracked
    #j = 47
    week_j = NBweeks[j] # pull week number from the sequential list above
    week_j_data <- subset(bird_i_data, week == week_j)
    val_j <- ifelse(nrow(week_j_data) == 0, NA, max(week_j_data$encounter)) # if no data, give NA, if data, take max of encounter
    bird_i_vals <- c(bird_i_vals, val_j)
    bird_i_vals
    }
  
  # add course
  meanX <- mean(bird_i_data$x) # average the x coords
  meanY <- mean(bird_i_data$y) # average the y coords
  mean_loc_i <- SpatialPoints(coords = data.frame("x" = meanX, "y" = meanY)) # convert DF to Spatial Points
  crs(mean_loc_i) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
  mean_loc_i <- spTransform(mean_loc_i, crs(courses))

  # plot(courses); plot(mean_loc_i, add = TRUE)
  course_i <- over(x = mean_loc_i, y = courses)
  course_i <- course_i$course 

  # add "YEAR"
  year_i = bird_i_data$year[1] # take the first value for Year

  # combine all pieces into a new row
  newrow <- data.frame("birdID" = birdlist[i], 
                       "wk40" = bird_i_vals[1], "wk41" = bird_i_vals[2], 
                       "wk42" = bird_i_vals[3], "wk43" = bird_i_vals[4],
                       "wk44" = bird_i_vals[5], "wk45" = bird_i_vals[6],
                       "wk46" = bird_i_vals[7], "wk47" = bird_i_vals[8],
                       "wk48" = bird_i_vals[9], "wk49" = bird_i_vals[10],
                       "wk50" = bird_i_vals[11], "wk51" = bird_i_vals[12],
                       "wk52" = bird_i_vals[13], "wk53" = bird_i_vals[14],
                       "wk1" = bird_i_vals[15], "wk2" = bird_i_vals[16],
                       "wk3" = bird_i_vals[17], "wk4" = bird_i_vals[18],
                       "wk5" = bird_i_vals[19], "wk6" = bird_i_vals[20],
                       "wk7" = bird_i_vals[21], "wk8" = bird_i_vals[22], 
                       "wk9" = bird_i_vals[23], "wk10" = bird_i_vals[24],
                       "wk11" = bird_i_vals[25], "wk12" = bird_i_vals[26],
                       "wk13" = bird_i_vals[27],
                       "ndvi" = mean(bird_i_data$ndvi), "perc_mpine" = mean(bird_i_data$perc_mpine), 
                       "perc_grassy" = mean(bird_i_data$perc_grassy), "perc_decid" = mean(bird_i_data$perc_decid), 
                       "perc_bf" = mean(bird_i_data$perc_bf), "perc_water" = mean(bird_i_data$perc_water), 
                       "DTN_road" = mean(bird_i_data$DTN_road), "DTN_mpine" = mean(bird_i_data$DTN_mpine), 
                       "DTN_grassy" = mean(bird_i_data$DTN_grassy), "DTN_decid" = mean(bird_i_data$DTN_decid), 
                       "DTN_bf" = mean(bird_i_data$DTN_bf), "DTN_water" = mean(bird_i_data$DTN_water),
                       "perc_burn" = mean(bird_i_data$perc_burn),  
                        "course" = course_i, "year" = year_i, "x" = meanX, "y" = meanY, 
                        "TrackedAlive" = sum(lengths(regmatches(bird_i_vals, gregexpr("1", bird_i_vals)))))

  # add new row to "inp_df"
  inp_df <- rbind(inp_df, newrow)
  }

 ######################### Remove birds with "0" tracked alive occasions
inp_df <- inp_df[2:(nrow(inp_df)), ] # remove the blank row at the top 
x = sum(inp_df$TrackedAlive == 0)
x # 51 birds not tracked at all 
inp_df <- subset(inp_df, TrackedAlive > 0)

# turn NA into "unknown"
inp_df$course[is.na(inp_df$course)]  <- "unknown"

######################### adding individual covariates ----
bird_data <- read.csv("E:/NOBO Project Data/Analyses/Trap Data/TrapData_08_29_2023 (1)/Trap_Data_0_28Oct2023.csv")
#   TO NOTE: check to make sure sex and age are only using 'Juvenile' / 'Adult' and 'Female' / 'Male' 
#             I manually adjusted within excel 


bird_data$ID <- paste0(bird_data$Frequency_at_Release, "_", bird_data$Band.ID) # creates a bird id column (xxx.xxx_xxxxxxx)
bird_data <- bird_data[,c(6:9, 26)] # removes unnecessary columns 

inp_df2 <- merge(x = inp_df, by.x = "birdID", y = bird_data, by.y = "ID", all.x = TRUE)
#View(inp_df2)
length(unique(inp_df2$birdID)) # 394 unique birds

# check for duplicates 
sum(duplicated(inp_df2$birdID)) # says 0 duplicates. Autumn found 12 at one point - unclear where they webt
d = duplicated(inp_df2$birdID) # this is here in case duplicates occur
d # this will show the locations

# remove the duplicates (11 or 12?) that are caused by birds being recaptured and reentered w/o changing freq
test1 = inp_df2[!duplicated(inp_df2$birdID), ]
length(unique(test1$birdID)) #

# check to see if it worked
nrow(inp_df2) # 406
nrow(test1) # 394 -- aka removed the 11 duplicates 

# convert back to the original df name now that i know I didnt fuck it up 
inp_df2 = test1

#View(test1)
# making sex and age into binary values
inp_df2$sex <- ifelse(inp_df2$Sex == "Male", 0, 1)
inp_df2$age <- ifelse(inp_df2$Age == "Juvenile", 0, 1)
#View(inp_df2)

# Removes more unneeded columns 
inp_df2 <- dplyr::select(inp_df2, -Sex, -Age, -TrackedAlive, -course, -Immature, -x, -y)

### Fixing NAs in birds' individual covariates (sex, age, etc.) ----
colSums(is.na(inp_df2)) # look at number of nas in each column 

inp_df2[,"Weight"][inp_df2[,"Weight"] == 0] <- round(mean(inp_df2[,"Weight"], na.rm = TRUE),0) # turn weights of 0 into mean weight
inp_df2$Weight[is.na(inp_df2$Weight)] <- round(mean(inp_df2[,"Weight"], na.rm = TRUE),0) # turn weights of NA into mean weight

# assume birds with age = NA are actually juveniles
inp_df2$age[is.na(inp_df2$age)] <- 0 # turn age of NA to 0

# assume birds with sex = NA are actually females # ------------------
inp_df2$sex[is.na(inp_df2$sex)] <- 1 # turn sex of NA to 1 




nrow(inp_df2)

######################### creating encounter history ----
neweo <- inp_df2$birdID# new encounter occasions
for(i in 2:28){ # col 1 is bird ID... 2-28 are the encounter occasions
  #i = 2
  dat <- as.data.frame(inp_df2[,i]) # isolate individual occasion
  dat <- ifelse(is.na(dat), "00",                       # if NA, make 00
                ifelse(dat == "0", "00",                # if 0, make 00
                       ifelse(dat == "1", "10", "11"))) # if 1, make 10, otherwise 11
  name_i <- paste0("wk", i+14)
  newcol <- data.frame(name_i = dat)
  names(newcol) <- name_i
  neweo <- cbind(neweo, newcol)
  }

# make encounter history
# adjusted to 27 weeks (oct 1 - mar 31)
eh <- paste0(neweo[,2],neweo[,3],neweo[,4],neweo[,5],neweo[,6],neweo[,7],neweo[,8],neweo[,9],neweo[,10],
             neweo[,11],neweo[,12],neweo[,13],neweo[,14],neweo[,15],neweo[,16],neweo[,17],neweo[,18],neweo[,19],
             neweo[,20],neweo[,21],neweo[,22],neweo[,23],neweo[,24],neweo[,25],neweo[,26],neweo[,27],neweo[,28])
eh <- data.frame("eh" = eh)
#View(eh)


#########################################################################
######################################################################### Bohemoth for() loops ----
######################################################################### generates time-specific
######################################################################### covariates
#########################################################################

length(unique(inp_df2$birdID))

BigDF <- data.frame() # data.frame to hold the time-varying covariates
CovariateNames <- c("ndvi", "perc_mpine", "perc_grassy", "perc_decid", 
                    "perc_bf", "perc_water", "DTN_road", "DTN_mpine", 
                    "DTN_grassy","DTN_decid", "DTN_bf", "DTN_water",
                    "perc_burn", "bush4", "blower", "burn_stat")

  for(i in 1:length(unique(inp_df2$birdID))){ # For each bird ....................
  # i = 343 # bird 144 is our golden child, "166.346_204659"
  sub <- subset(nonbreeding1, Bird.ID == unique(inp_df2$birdID)[i]) # subset bird i
  covariateList <- list() # create blank list to eventually store completed covariate data.frames
  
  for(j in 1:length(CovariateNames)){ # For each covariate .....................
    # j = 1
    covName <- CovariateNames[j] # isolate jth covariate name
    txt1 <- paste0("bird_i_cov_j <- data.frame('", covName, "' = sub$", covName,")")
    eval(parse(text = txt1)) # create data.frame with the jth covariate as the only column
    bird_i_cov_j$week <- sub$week # add week to "bird_i_cov_j"
    bird_i_cov_j$ordinal <- sub$ordinal # add ordinal to "bird_i_cov_j"
    covHistory <- c() # create blank vector to hold values of the jth covariate
    
    for(k in 1:length(NBweeks)){  # For each week ..........................................
      # k = 47
      newval <- subset(bird_i_cov_j, week == NBweeks[k]) # subset to week k
      newval <- ifelse(nrow(newval) == 0, # ifelse() that gives the mean value to any weeks with no cov value
                       mean(bird_i_cov_j[,1]), # assign mean if nrow = 0
                       subset(newval, ordinal == max(newval$ordinal))[1,1]) # otherwise take the max in week k
      # the prev ifelse() gives the mean for missing values; 
      # we don't want this for burn_stat, bush4, and blower
      newval <- ifelse(j >= 14 & newval<1 & newval>0, 0, newval) # for covs 14, 15, and 16, non-binary values become 0
      newval <- round(newval, digits = 2) # round to two digits
      covHistory <- c(covHistory, newval) # add newval to covHistory
    }
    
    covHistoryDF <- data.frame("wk40" = 1, "wk41" = 1, "wk42" = 1,             # create DF to hold covHistory
                               "wk43" = 1, "wk44" = 1, "wk45" = 1, "wk46" = 1, # values set to 1 as a placeholder
                               "wk47" = 1, "wk48" = 1, "wk49" = 1, "wk50" = 1,
                               "wk51" = 1, "wk52" = 1, "wk53" = 1, "wk1" = 1,
                               "wk2" = 1, "wk3" = 1, "wk4" = 1, "wk5" = 1,
                               "wk6" = 1, "wk7" = 1, "wk8" = 1, "wk9" = 1,
                               "wk10" = 1, "wk11" = 1, "wk12" = 1, "wk13" = 1)
    covHistoryDF <- rbind(covHistoryDF, covHistory) # add covHistory with rbind()
    covHistoryDF <- covHistoryDF[2,] # remove row of 1's
    covariateList[[j]] <- covHistoryDF # add covHistory data.frame() to covariateList!
    # covariateList
  }
  
  names(covariateList) <- CovariateNames # name all elements in covariateList
  covariateList
  
  # BEHEMOTH: ADDING ALL COVARIATES ----
  # creating a big data.frame to hold all of the columns for bird i's data
  Bird_i_BigDF <- data.frame("birdID" = unique(inp_df2$birdID)[i],
                             "ndvi14" = covariateList$ndvi$wk40,                                   # ndvi
                             "ndvi15" = covariateList$ndvi$wk41, "ndvi16" = covariateList$ndvi$wk42, 
                             "ndvi17" = covariateList$ndvi$wk43, "ndvi18" = covariateList$ndvi$wk44, 
                             "ndvi19" = covariateList$ndvi$wk45, "ndvi20" = covariateList$ndvi$wk46,
                             "ndvi21" = covariateList$ndvi$wk47, "ndvi22" = covariateList$ndvi$wk48, 
                             "ndvi23" = covariateList$ndvi$wk49, "ndvi24" = covariateList$ndvi$wk50,
                             "ndvi25" = covariateList$ndvi$wk51, "ndvi26" = covariateList$ndvi$wk52, 
                             "ndvi27" = covariateList$ndvi$wk53, "ndvi28" = covariateList$ndvi$wk1,
                             "ndvi29" = covariateList$ndvi$wk2, "ndvi30" = covariateList$ndvi$wk3, 
                             "ndvi31" = covariateList$ndvi$wk4, "ndvi32" = covariateList$ndvi$wk5,
                             "ndvi33" = covariateList$ndvi$wk6, "ndvi34" = covariateList$ndvi$wk7, 
                             "ndvi35" = covariateList$ndvi$wk8, "ndvi36" = covariateList$ndvi$wk9,
                             "ndvi37" = covariateList$ndvi$wk10, "ndvi38" = covariateList$ndvi$wk11,
                             "ndvi39" = covariateList$ndvi$wk12, "ndvi40" = covariateList$ndvi$wk13,
                             
                             "perc_mpine14" = covariateList$perc_mpine$wk40,                                   # perc_mpine
                             "perc_mpine15" = covariateList$perc_mpine$wk41, "perc_mpine16" = covariateList$perc_mpine$wk42, 
                             "perc_mpine17" = covariateList$perc_mpine$wk43, "perc_mpine18" = covariateList$perc_mpine$wk44, 
                             "perc_mpine19" = covariateList$perc_mpine$wk45, "perc_mpine20" = covariateList$perc_mpine$wk46,
                             "perc_mpine21" = covariateList$perc_mpine$wk47, "perc_mpine22" = covariateList$perc_mpine$wk48, 
                             "perc_mpine23" = covariateList$perc_mpine$wk49, "perc_mpine24" = covariateList$perc_mpine$wk50,
                             "perc_mpine25" = covariateList$perc_mpine$wk51, "perc_mpine26" = covariateList$perc_mpine$wk52, 
                             "perc_mpine27" = covariateList$perc_mpine$wk53, "perc_mpine28" = covariateList$perc_mpine$wk1,
                             "perc_mpine29" = covariateList$perc_mpine$wk2, "perc_mpine30" = covariateList$perc_mpine$wk3, 
                             "perc_mpine31" = covariateList$perc_mpine$wk4, "perc_mpine32" = covariateList$perc_mpine$wk5,
                             "perc_mpine33" = covariateList$perc_mpine$wk6, "perc_mpine34" = covariateList$perc_mpine$wk7, 
                             "perc_mpine35" = covariateList$perc_mpine$wk8, "perc_mpine36" = covariateList$perc_mpine$wk9,
                             "perc_mpine37" = covariateList$perc_mpine$wk10, "perc_mpine38" = covariateList$perc_mpine$wk11,
                             "perc_mpine39" = covariateList$perc_mpine$wk12, "perc_mpine40" = covariateList$perc_mpine$wk13,
                             
                             "perc_grassy14" = covariateList$perc_grassy$wk40,                                   # perc_grassy
                             "perc_grassy15" = covariateList$perc_grassy$wk41, "perc_grassy16" = covariateList$perc_grassy$wk42, 
                             "perc_grassy17" = covariateList$perc_grassy$wk43, "perc_grassy18" = covariateList$perc_grassy$wk44, 
                             "perc_grassy19" = covariateList$perc_grassy$wk45, "perc_grassy20" = covariateList$perc_grassy$wk46,
                             "perc_grassy21" = covariateList$perc_grassy$wk47, "perc_grassy22" = covariateList$perc_grassy$wk48, 
                             "perc_grassy23" = covariateList$perc_grassy$wk49, "perc_grassy24" = covariateList$perc_grassy$wk50,
                             "perc_grassy25" = covariateList$perc_grassy$wk51, "perc_grassy26" = covariateList$perc_grassy$wk52, 
                             "perc_grassy27" = covariateList$perc_grassy$wk53, "perc_grassy28" = covariateList$perc_grassy$wk1,
                             "perc_grassy29" = covariateList$perc_grassy$wk2, "perc_grassy30" = covariateList$perc_grassy$wk3, 
                             "perc_grassy31" = covariateList$perc_grassy$wk4, "perc_grassy32" = covariateList$perc_grassy$wk5,
                             "perc_grassy33" = covariateList$perc_grassy$wk6, "perc_grassy34" = covariateList$perc_grassy$wk7, 
                             "perc_grassy35" = covariateList$perc_grassy$wk8, "perc_grassy36" = covariateList$perc_grassy$wk9,
                             "perc_grassy37" = covariateList$perc_grassy$wk10, "perc_grassy38" = covariateList$perc_grassy$wk11,
                             "perc_grassy39" = covariateList$perc_grassy$wk12, "perc_grassy40" = covariateList$perc_grassy$wk13,
                             
                             "perc_decid14" = covariateList$perc_decid$wk40,                                   # perc_decid
                             "perc_decid15" = covariateList$perc_decid$wk41, "perc_decid16" = covariateList$perc_decid$wk42, 
                             "perc_decid17" = covariateList$perc_decid$wk43, "perc_decid18" = covariateList$perc_decid$wk44, 
                             "perc_decid19" = covariateList$perc_decid$wk45, "perc_decid20" = covariateList$perc_decid$wk46,
                             "perc_decid21" = covariateList$perc_decid$wk47, "perc_decid22" = covariateList$perc_decid$wk48, 
                             "perc_decid23" = covariateList$perc_decid$wk49, "perc_decid24" = covariateList$perc_decid$wk50,
                             "perc_decid25" = covariateList$perc_decid$wk51, "perc_decid26" = covariateList$perc_decid$wk52, 
                             "perc_decid27" = covariateList$perc_decid$wk53, "perc_decid28" = covariateList$perc_decid$wk1,
                             "perc_decid29" = covariateList$perc_decid$wk2, "perc_decid30" = covariateList$perc_decid$wk3, 
                             "perc_decid31" = covariateList$perc_decid$wk4, "perc_decid32" = covariateList$perc_decid$wk5,
                             "perc_decid33" = covariateList$perc_decid$wk6, "perc_decid34" = covariateList$perc_decid$wk7, 
                             "perc_decid35" = covariateList$perc_decid$wk8, "perc_decid36" = covariateList$perc_decid$wk9,
                             "perc_decid37" = covariateList$perc_decid$wk10, "perc_decid38" = covariateList$perc_decid$wk11,
                             "perc_decid39" = covariateList$perc_decid$wk12, "perc_decid40" = covariateList$perc_decid$wk13,
                             
                             "perc_bf14" = covariateList$perc_bf$wk40,                                   # perc_bf
                             "perc_bf15" = covariateList$perc_bf$wk41, "perc_bf16" = covariateList$perc_bf$wk42, 
                             "perc_bf17" = covariateList$perc_bf$wk43, "perc_bf18" = covariateList$perc_bf$wk44, 
                             "perc_bf19" = covariateList$perc_bf$wk45, "perc_bf20" = covariateList$perc_bf$wk46,
                             "perc_bf21" = covariateList$perc_bf$wk47, "perc_bf22" = covariateList$perc_bf$wk48, 
                             "perc_bf23" = covariateList$perc_bf$wk49, "perc_bf24" = covariateList$perc_bf$wk50,
                             "perc_bf25" = covariateList$perc_bf$wk51, "perc_bf26" = covariateList$perc_bf$wk52, 
                             "perc_bf27" = covariateList$perc_bf$wk53, "perc_bf28" = covariateList$perc_bf$wk1,
                             "perc_bf29" = covariateList$perc_bf$wk2, "perc_bf30" = covariateList$perc_bf$wk3, 
                             "perc_bf31" = covariateList$perc_bf$wk4, "perc_bf32" = covariateList$perc_bf$wk5,
                             "perc_bf33" = covariateList$perc_bf$wk6, "perc_bf34" = covariateList$perc_bf$wk7, 
                             "perc_bf35" = covariateList$perc_bf$wk8, "perc_bf36" = covariateList$perc_bf$wk9,
                             "perc_bf37" = covariateList$perc_bf$wk10, "perc_bf38" = covariateList$perc_bf$wk11,
                             "perc_bf39" = covariateList$perc_bf$wk12, "perc_bf40" = covariateList$perc_bf$wk13,
                             
                             "perc_water14" = covariateList$perc_water$wk40,                                   # perc_water
                             "perc_water15" = covariateList$perc_water$wk41, "perc_water16" = covariateList$perc_water$wk42, 
                             "perc_water17" = covariateList$perc_water$wk43, "perc_water18" = covariateList$perc_water$wk44, 
                             "perc_water19" = covariateList$perc_water$wk45, "perc_water20" = covariateList$perc_water$wk46,
                             "perc_water21" = covariateList$perc_water$wk47, "perc_water22" = covariateList$perc_water$wk48, 
                             "perc_water23" = covariateList$perc_water$wk49, "perc_water24" = covariateList$perc_water$wk50,
                             "perc_water25" = covariateList$perc_water$wk51, "perc_water26" = covariateList$perc_water$wk52, 
                             "perc_water27" = covariateList$perc_water$wk53, "perc_water28" = covariateList$perc_water$wk1,
                             "perc_water29" = covariateList$perc_water$wk2, "perc_water30" = covariateList$perc_water$wk3, 
                             "perc_water31" = covariateList$perc_water$wk4, "perc_water32" = covariateList$perc_water$wk5,
                             "perc_water33" = covariateList$perc_water$wk6, "perc_water34" = covariateList$perc_water$wk7, 
                             "perc_water35" = covariateList$perc_water$wk8, "perc_water36" = covariateList$perc_water$wk9,
                             "perc_water37" = covariateList$perc_water$wk10, "perc_water38" = covariateList$perc_water$wk11,
                             "perc_water39" = covariateList$perc_water$wk12, "perc_water40" = covariateList$perc_water$wk13,
                             
                             "DTN_road14" = covariateList$DTN_road$wk40,                                   # DTN_road
                             "DTN_road15" = covariateList$DTN_road$wk41, "DTN_road16" = covariateList$DTN_road$wk42, 
                             "DTN_road17" = covariateList$DTN_road$wk43, "DTN_road18" = covariateList$DTN_road$wk44, 
                             "DTN_road19" = covariateList$DTN_road$wk45, "DTN_road20" = covariateList$DTN_road$wk46,
                             "DTN_road21" = covariateList$DTN_road$wk47, "DTN_road22" = covariateList$DTN_road$wk48, 
                             "DTN_road23" = covariateList$DTN_road$wk49, "DTN_road24" = covariateList$DTN_road$wk50,
                             "DTN_road25" = covariateList$DTN_road$wk51, "DTN_road26" = covariateList$DTN_road$wk52, 
                             "DTN_road27" = covariateList$DTN_road$wk53, "DTN_road28" = covariateList$DTN_road$wk1,
                             "DTN_road29" = covariateList$DTN_road$wk2, "DTN_road30" = covariateList$DTN_road$wk3, 
                             "DTN_road31" = covariateList$DTN_road$wk4, "DTN_road32" = covariateList$DTN_road$wk5,
                             "DTN_road33" = covariateList$DTN_road$wk6, "DTN_road34" = covariateList$DTN_road$wk7, 
                             "DTN_road35" = covariateList$DTN_road$wk8, "DTN_road36" = covariateList$DTN_road$wk9,
                             "DTN_road37" = covariateList$DTN_road$wk10, "DTN_road38" = covariateList$DTN_road$wk11,
                             "DTN_road39" = covariateList$DTN_road$wk12, "DTN_road40" = covariateList$DTN_road$wk13,
                             
                             "DTN_mpine14" = covariateList$DTN_mpine$wk40,                                   # DTN_mpine
                             "DTN_mpine15" = covariateList$DTN_mpine$wk41, "DTN_mpine16" = covariateList$DTN_mpine$wk42, 
                             "DTN_mpine17" = covariateList$DTN_mpine$wk43, "DTN_mpine18" = covariateList$DTN_mpine$wk44, 
                             "DTN_mpine19" = covariateList$DTN_mpine$wk45, "DTN_mpine20" = covariateList$DTN_mpine$wk46,
                             "DTN_mpine21" = covariateList$DTN_mpine$wk47, "DTN_mpine22" = covariateList$DTN_mpine$wk48, 
                             "DTN_mpine23" = covariateList$DTN_mpine$wk49, "DTN_mpine24" = covariateList$DTN_mpine$wk50,
                             "DTN_mpine25" = covariateList$DTN_mpine$wk51, "DTN_mpine26" = covariateList$DTN_mpine$wk52, 
                             "DTN_mpine27" = covariateList$DTN_mpine$wk53, "DTN_mpine28" = covariateList$DTN_mpine$wk1,
                             "DTN_mpine29" = covariateList$DTN_mpine$wk2, "DTN_mpine30" = covariateList$DTN_mpine$wk3, 
                             "DTN_mpine31" = covariateList$DTN_mpine$wk4, "DTN_mpine32" = covariateList$DTN_mpine$wk5,
                             "DTN_mpine33" = covariateList$DTN_mpine$wk6, "DTN_mpine34" = covariateList$DTN_mpine$wk7, 
                             "DTN_mpine35" = covariateList$DTN_mpine$wk8, "DTN_mpine36" = covariateList$DTN_mpine$wk9,
                             "DTN_mpine37" = covariateList$DTN_mpine$wk10, "DTN_mpine38" = covariateList$DTN_mpine$wk11,
                             "DTN_mpine39" = covariateList$DTN_mpine$wk12, "DTN_mpine40" = covariateList$DTN_mpine$wk13,
                             
                             "DTN_grassy14" = covariateList$DTN_grassy$wk40,                                   # DTN_grassy
                             "DTN_grassy15" = covariateList$DTN_grassy$wk41, "DTN_grassy16" = covariateList$DTN_grassy$wk42, 
                             "DTN_grassy17" = covariateList$DTN_grassy$wk43, "DTN_grassy18" = covariateList$DTN_grassy$wk44, 
                             "DTN_grassy19" = covariateList$DTN_grassy$wk45, "DTN_grassy20" = covariateList$DTN_grassy$wk46,
                             "DTN_grassy21" = covariateList$DTN_grassy$wk47, "DTN_grassy22" = covariateList$DTN_grassy$wk48, 
                             "DTN_grassy23" = covariateList$DTN_grassy$wk49, "DTN_grassy24" = covariateList$DTN_grassy$wk50,
                             "DTN_grassy25" = covariateList$DTN_grassy$wk51, "DTN_grassy26" = covariateList$DTN_grassy$wk52, 
                             "DTN_grassy27" = covariateList$DTN_grassy$wk53, "DTN_grassy28" = covariateList$DTN_grassy$wk1,
                             "DTN_grassy29" = covariateList$DTN_grassy$wk2, "DTN_grassy30" = covariateList$DTN_grassy$wk3, 
                             "DTN_grassy31" = covariateList$DTN_grassy$wk4, "DTN_grassy32" = covariateList$DTN_grassy$wk5,
                             "DTN_grassy33" = covariateList$DTN_grassy$wk6, "DTN_grassy34" = covariateList$DTN_grassy$wk7, 
                             "DTN_grassy35" = covariateList$DTN_grassy$wk8, "DTN_grassy36" = covariateList$DTN_grassy$wk9,
                             "DTN_grassy37" = covariateList$DTN_grassy$wk10, "DTN_grassy38" = covariateList$DTN_grassy$wk11,
                             "DTN_grassy39" = covariateList$DTN_grassy$wk12, "DTN_grassy40" = covariateList$DTN_grassy$wk13,
                             
                             "DTN_decid14" = covariateList$DTN_decid$wk40,                                   # DTN_decid
                             "DTN_decid15" = covariateList$DTN_decid$wk41, "DTN_decid16" = covariateList$DTN_decid$wk42, 
                             "DTN_decid17" = covariateList$DTN_decid$wk43, "DTN_decid18" = covariateList$DTN_decid$wk44, 
                             "DTN_decid19" = covariateList$DTN_decid$wk45, "DTN_decid20" = covariateList$DTN_decid$wk46,
                             "DTN_decid21" = covariateList$DTN_decid$wk47, "DTN_decid22" = covariateList$DTN_decid$wk48, 
                             "DTN_decid23" = covariateList$DTN_decid$wk49, "DTN_decid24" = covariateList$DTN_decid$wk50,
                             "DTN_decid25" = covariateList$DTN_decid$wk51, "DTN_decid26" = covariateList$DTN_decid$wk52, 
                             "DTN_decid27" = covariateList$DTN_decid$wk53, "DTN_decid28" = covariateList$DTN_decid$wk1,
                             "DTN_decid29" = covariateList$DTN_decid$wk2, "DTN_decid30" = covariateList$DTN_decid$wk3, 
                             "DTN_decid31" = covariateList$DTN_decid$wk4, "DTN_decid32" = covariateList$DTN_decid$wk5,
                             "DTN_decid33" = covariateList$DTN_decid$wk6, "DTN_decid34" = covariateList$DTN_decid$wk7, 
                             "DTN_decid35" = covariateList$DTN_decid$wk8, "DTN_decid36" = covariateList$DTN_decid$wk9,
                             "DTN_decid37" = covariateList$DTN_decid$wk10, "DTN_decid38" = covariateList$DTN_decid$wk11,
                             "DTN_decid39" = covariateList$DTN_decid$wk12, "DTN_decid40" = covariateList$DTN_decid$wk13,
                             
                             "DTN_bf14" = covariateList$DTN_bf$wk40,                                   # DTN_bf
                             "DTN_bf15" = covariateList$DTN_bf$wk41, "DTN_bf16" = covariateList$DTN_bf$wk42, 
                             "DTN_bf17" = covariateList$DTN_bf$wk43, "DTN_bf18" = covariateList$DTN_bf$wk44, 
                             "DTN_bf19" = covariateList$DTN_bf$wk45, "DTN_bf20" = covariateList$DTN_bf$wk46,
                             "DTN_bf21" = covariateList$DTN_bf$wk47, "DTN_bf22" = covariateList$DTN_bf$wk48, 
                             "DTN_bf23" = covariateList$DTN_bf$wk49, "DTN_bf24" = covariateList$DTN_bf$wk50,
                             "DTN_bf25" = covariateList$DTN_bf$wk51, "DTN_bf26" = covariateList$DTN_bf$wk52, 
                             "DTN_bf27" = covariateList$DTN_bf$wk53, "DTN_bf28" = covariateList$DTN_bf$wk1,
                             "DTN_bf29" = covariateList$DTN_bf$wk2, "DTN_bf30" = covariateList$DTN_bf$wk3, 
                             "DTN_bf31" = covariateList$DTN_bf$wk4, "DTN_bf32" = covariateList$DTN_bf$wk5,
                             "DTN_bf33" = covariateList$DTN_bf$wk6, "DTN_bf34" = covariateList$DTN_bf$wk7, 
                             "DTN_bf35" = covariateList$DTN_bf$wk8, "DTN_bf36" = covariateList$DTN_bf$wk9,
                             "DTN_bf37" = covariateList$DTN_bf$wk10, "DTN_bf38" = covariateList$DTN_bf$wk11,
                             "DTN_bf39" = covariateList$DTN_bf$wk12, "DTN_bf40" = covariateList$DTN_bf$wk13,
                             
                             "DTN_water14" = covariateList$DTN_water$wk40,                                   # DTN_water
                             "DTN_water15" = covariateList$DTN_water$wk41, "DTN_water16" = covariateList$DTN_water$wk42, 
                             "DTN_water17" = covariateList$DTN_water$wk43, "DTN_water18" = covariateList$DTN_water$wk44, 
                             "DTN_water19" = covariateList$DTN_water$wk45, "DTN_water20" = covariateList$DTN_water$wk46,
                             "DTN_water21" = covariateList$DTN_water$wk47, "DTN_water22" = covariateList$DTN_water$wk48, 
                             "DTN_water23" = covariateList$DTN_water$wk49, "DTN_water24" = covariateList$DTN_water$wk50,
                             "DTN_water25" = covariateList$DTN_water$wk51, "DTN_water26" = covariateList$DTN_water$wk52, 
                             "DTN_water27" = covariateList$DTN_water$wk53, "DTN_water28" = covariateList$DTN_water$wk1,
                             "DTN_water29" = covariateList$DTN_water$wk2, "DTN_water30" = covariateList$DTN_water$wk3, 
                             "DTN_water31" = covariateList$DTN_water$wk4, "DTN_water32" = covariateList$DTN_water$wk5,
                             "DTN_water33" = covariateList$DTN_water$wk6, "DTN_water34" = covariateList$DTN_water$wk7, 
                             "DTN_water35" = covariateList$DTN_water$wk8, "DTN_water36" = covariateList$DTN_water$wk9,
                             "DTN_water37" = covariateList$DTN_water$wk10, "DTN_water38" = covariateList$DTN_water$wk11,
                             "DTN_water39" = covariateList$DTN_water$wk12, "DTN_water40" = covariateList$DTN_water$wk13,
                             
                             "perc_burn14" = covariateList$perc_burn$wk40,                                   # perc_burn
                             "perc_burn15" = covariateList$perc_burn$wk41, "perc_burn16" = covariateList$perc_burn$wk42, 
                             "perc_burn17" = covariateList$perc_burn$wk43, "perc_burn18" = covariateList$perc_burn$wk44, 
                             "perc_burn19" = covariateList$perc_burn$wk45, "perc_burn20" = covariateList$perc_burn$wk46,
                             "perc_burn21" = covariateList$perc_burn$wk47, "perc_burn22" = covariateList$perc_burn$wk48, 
                             "perc_burn23" = covariateList$perc_burn$wk49, "perc_burn24" = covariateList$perc_burn$wk50,
                             "perc_burn25" = covariateList$perc_burn$wk51, "perc_burn26" = covariateList$perc_burn$wk52, 
                             "perc_burn27" = covariateList$perc_burn$wk53, "perc_burn28" = covariateList$perc_burn$wk1,
                             "perc_burn29" = covariateList$perc_burn$wk2, "perc_burn30" = covariateList$perc_burn$wk3, 
                             "perc_burn31" = covariateList$perc_burn$wk4, "perc_burn32" = covariateList$perc_burn$wk5,
                             "perc_burn33" = covariateList$perc_burn$wk6, "perc_burn34" = covariateList$perc_burn$wk7, 
                             "perc_burn35" = covariateList$perc_burn$wk8, "perc_burn36" = covariateList$perc_burn$wk9,
                             "perc_burn37" = covariateList$perc_burn$wk10, "perc_burn38" = covariateList$perc_burn$wk11,
                             "perc_burn39" = covariateList$perc_burn$wk12, "perc_burn40" = covariateList$perc_burn$wk13,
                             
                             "bush414" = covariateList$bush4$wk40,                                   # bush4
                             "bush415" = covariateList$bush4$wk41, "bush416" = covariateList$bush4$wk42, 
                             "bush417" = covariateList$bush4$wk43, "bush418" = covariateList$bush4$wk44, 
                             "bush419" = covariateList$bush4$wk45, "bush420" = covariateList$bush4$wk46,
                             "bush421" = covariateList$bush4$wk47, "bush422" = covariateList$bush4$wk48, 
                             "bush423" = covariateList$bush4$wk49, "bush424" = covariateList$bush4$wk50,
                             "bush425" = covariateList$bush4$wk51, "bush426" = covariateList$bush4$wk52, 
                             "bush427" = covariateList$bush4$wk53, "bush428" = covariateList$bush4$wk1,
                             "bush429" = covariateList$bush4$wk2, "bush430" = covariateList$bush4$wk3, 
                             "bush431" = covariateList$bush4$wk4, "bush432" = covariateList$bush4$wk5,
                             "bush433" = covariateList$bush4$wk6, "bush434" = covariateList$bush4$wk7, 
                             "bush435" = covariateList$bush4$wk8, "bush436" = covariateList$bush4$wk9,
                             "bush437" = covariateList$bush4$wk10, "bush438" = covariateList$bush4$wk11,
                             "bush439" = covariateList$bush4$wk12, "bush440" = covariateList$bush4$wk13,
                             
                             "blower14" = covariateList$blower$wk40,                                   # blower
                             "blower15" = covariateList$blower$wk41, "blower16" = covariateList$blower$wk42, 
                             "blower17" = covariateList$blower$wk43, "blower18" = covariateList$blower$wk44, 
                             "blower19" = covariateList$blower$wk45, "blower20" = covariateList$blower$wk46,
                             "blower21" = covariateList$blower$wk47, "blower22" = covariateList$blower$wk48, 
                             "blower23" = covariateList$blower$wk49, "blower24" = covariateList$blower$wk50,
                             "blower25" = covariateList$blower$wk51, "blower26" = covariateList$blower$wk52, 
                             "blower27" = covariateList$blower$wk53, "blower28" = covariateList$blower$wk1,
                             "blower29" = covariateList$blower$wk2, "blower30" = covariateList$blower$wk3, 
                             "blower31" = covariateList$blower$wk4, "blower32" = covariateList$blower$wk5,
                             "blower33" = covariateList$blower$wk6, "blower34" = covariateList$blower$wk7, 
                             "blower35" = covariateList$blower$wk8, "blower36" = covariateList$blower$wk9,
                             "blower37" = covariateList$blower$wk10, "blower38" = covariateList$blower$wk11,
                             "blower39" = covariateList$blower$wk12, "blower40" = covariateList$blower$wk13,
                             
                             "burn_stat14" = covariateList$burn_stat$wk40,                                   # burn_stat
                             "burn_stat15" = covariateList$burn_stat$wk41, "burn_stat16" = covariateList$burn_stat$wk42, 
                             "burn_stat17" = covariateList$burn_stat$wk43, "burn_stat18" = covariateList$burn_stat$wk44, 
                             "burn_stat19" = covariateList$burn_stat$wk45, "burn_stat20" = covariateList$burn_stat$wk46,
                             "burn_stat21" = covariateList$burn_stat$wk47, "burn_stat22" = covariateList$burn_stat$wk48, 
                             "burn_stat23" = covariateList$burn_stat$wk49, "burn_stat24" = covariateList$burn_stat$wk50,
                             "burn_stat25" = covariateList$burn_stat$wk51, "burn_stat26" = covariateList$burn_stat$wk52, 
                             "burn_stat27" = covariateList$burn_stat$wk53, "burn_stat28" = covariateList$burn_stat$wk1,
                             "burn_stat29" = covariateList$burn_stat$wk2, "burn_stat30" = covariateList$burn_stat$wk3, 
                             "burn_stat31" = covariateList$burn_stat$wk4, "burn_stat32" = covariateList$burn_stat$wk5,
                             "burn_stat33" = covariateList$burn_stat$wk6, "burn_stat34" = covariateList$burn_stat$wk7, 
                             "burn_stat35" = covariateList$burn_stat$wk8, "burn_stat36" = covariateList$burn_stat$wk9,
                             "burn_stat37" = covariateList$burn_stat$wk10, "burn_stat38" = covariateList$burn_stat$wk11,
                             "burn_stat39" = covariateList$burn_stat$wk12, "burn_stat40" = covariateList$burn_stat$wk13)
                             
  
  BigDF <- rbind(BigDF, Bird_i_BigDF)
  print(paste0("bird number ", i, " of ", length(unique(inp_df2$birdID)), " is done"))
  }

#View(inp_df2)
sum(is.na(BigDF))

#########################################################################
######################################################################### 
######################################################################### End of bohemoth for() loops
######################################################################### 
#########################################################################

# add encounter history with covariates
nrow(inp_df2) # 355 --- why the fuck is this different than 343 
nrow(BigDF) # 343
pre_inp <- data.frame("birdid" = inp_df2$birdID, "eh" = eh$eh, inp_df2[,29:45], BigDF[,2:433]) # adds bird id, encounter history, average covariates per individual, time varying covariates
# View(pre_inp)
sum(is.na(pre_inp))


new_inp <- data.frame("forMARK" = paste0("/*", pre_inp$birdid, "*/", # bird ID
                                         pre_inp$eh, " ", # encounter history
                                         1, " ", # group
                                         round(pre_inp[,3],2), " ", # avg ndvi
                                         round(pre_inp[,4],2), " ", # avg perc mpine
                                         round(pre_inp[,5],2), " ", # avg perc grassyy 
                                         round(pre_inp[,6],2), " ", # avg perc decid
                                         round(pre_inp[,7],2), " ", # avg perc bf 
                                         round(pre_inp[,8],2), " ", # avg perc water
                                         round(pre_inp[,9],0), " ", # avg dtn road
                                         round(pre_inp[,10],0), " ", # avg dtn mpine
                                         round(pre_inp[,11],0), " ", # avg dtn grassy 
                                         round(pre_inp[,12],0), " ", # avg dtn decid 
                                         round(pre_inp[,13],0), " ", # avg dtn bf
                                         round(pre_inp[,14],0), " ", # avg dn water
                                         round(pre_inp[,15],2), " ", # avg perc burn
                                         pre_inp[,16], " ", # year (binary)
                                         pre_inp[,17], " ", # weight 
                                         pre_inp[,18], " ", # sex
                                         pre_inp[,19], " ", # age
                                         
                                         pre_inp[,20], " ", # Average % NDVI
                                         pre_inp[,21], " ", # 
                                         pre_inp[,22], " ", # 
                                         pre_inp[,23], " ", # col 20:46
                                         pre_inp[,24], " ",
                                         pre_inp[,25], " ",
                                         pre_inp[,26], " ",
                                         pre_inp[,27], " ",
                                         pre_inp[,28], " ",
                                         pre_inp[,29], " ",
                                         pre_inp[,30], " ",
                                         pre_inp[,31], " ",
                                         pre_inp[,32], " ",
                                         pre_inp[,33], " ",
                                         pre_inp[,34], " ",
                                         pre_inp[,35], " ",
                                         pre_inp[,36], " ",
                                         pre_inp[,37], " ",
                                         pre_inp[,38], " ",
                                         pre_inp[,39], " ",
                                         pre_inp[,40], " ",
                                         pre_inp[,41], " ",
                                         pre_inp[,42], " ",
                                         pre_inp[,43], " ",
                                         pre_inp[,44], " ",
                                         pre_inp[,45], " ",
                                         pre_inp[,46], " ",
                                         
                                         pre_inp[,47], " ",
                                         pre_inp[,48], " ", # percent mpine: TVC (time varying cov)
                                         pre_inp[,49], " ", # col 46:74
                                         pre_inp[,50], " ", # 
                                         pre_inp[,51], " ",
                                         pre_inp[,52], " ",
                                         pre_inp[,53], " ",
                                         pre_inp[,54], " ",
                                         pre_inp[,55], " ",
                                         pre_inp[,56], " ",
                                         pre_inp[,57], " ",
                                         pre_inp[,58], " ",
                                         pre_inp[,59], " ",
                                         pre_inp[,60], " ",
                                         pre_inp[,61], " ",
                                         pre_inp[,62], " ",
                                         pre_inp[,63], " ",
                                         pre_inp[,64], " ",
                                         pre_inp[,65], " ",
                                         pre_inp[,66], " ",
                                         pre_inp[,67], " ",
                                         pre_inp[,68], " ",
                                         pre_inp[,69], " ",
                                         pre_inp[,70], " ",
                                         pre_inp[,71], " ",
                                         pre_inp[,72], " ",
                                         pre_inp[,73], " ",
                                         
                                         pre_inp[,74], " ",
                                         pre_inp[,75], " ", # percent grassy: 
                                         pre_inp[,76], " ", # col 75-101
                                         pre_inp[,77], " ",  
                                         pre_inp[,78], " ",
                                         pre_inp[,79], " ",
                                         pre_inp[,80], " ",
                                         pre_inp[,81], " ",
                                         pre_inp[,82], " ",
                                         pre_inp[,83], " ",
                                         pre_inp[,84], " ",
                                         pre_inp[,85], " ",
                                         pre_inp[,86], " ",
                                         pre_inp[,87], " ",
                                         pre_inp[,88], " ",
                                         pre_inp[,89], " ",
                                         pre_inp[,90], " ",
                                         pre_inp[,91], " ",
                                         pre_inp[,92], " ",
                                         pre_inp[,93], " ",
                                         pre_inp[,94], " ",
                                         pre_inp[,95], " ",
                                         pre_inp[,96], " ",
                                         pre_inp[,97], " ",
                                         pre_inp[,98], " ",
                                         pre_inp[,99], " ",
                                         pre_inp[,100], " ",
                                         
                                         pre_inp[,101], " ",
                                         pre_inp[,102], " ", # percent decid
                                         pre_inp[,103], " ", # col 102-103
                                         pre_inp[,104], " ", # 
                                         pre_inp[,105], " ",
                                         pre_inp[,106], " ",
                                         pre_inp[,107], " ",
                                         pre_inp[,108], " ",
                                         pre_inp[,109], " ",
                                         pre_inp[,110], " ",
                                         pre_inp[,111], " ",
                                         pre_inp[,112], " ",
                                         pre_inp[,113], " ",
                                         pre_inp[,114], " ",
                                         pre_inp[,115], " ",
                                         pre_inp[,116], " ",
                                         pre_inp[,117], " ",
                                         pre_inp[,118], " ",
                                         pre_inp[,119], " ",
                                         pre_inp[,120], " ",
                                         pre_inp[,121], " ",
                                         pre_inp[,122], " ",
                                         pre_inp[,123], " ",
                                         pre_inp[,124], " ",
                                         pre_inp[,125], " ",
                                         pre_inp[,126], " ",
                                         pre_inp[,127], " ",
                                         
                                         pre_inp[,128], " ",
                                         pre_inp[,129], " ", # percent bf: 
                                         pre_inp[,130], " ", # col 128 - 154
                                         pre_inp[,131], " ", # 
                                         pre_inp[,132], " ",
                                         pre_inp[,133], " ",
                                         pre_inp[,134], " ",
                                         pre_inp[,135], " ",
                                         pre_inp[,136], " ",
                                         pre_inp[,137], " ",
                                         pre_inp[,138], " ",
                                         pre_inp[,139], " ",
                                         pre_inp[,140], " ",
                                         pre_inp[,141], " ",
                                         pre_inp[,142], " ",
                                         pre_inp[,143], " ",
                                         pre_inp[,144], " ",
                                         pre_inp[,145], " ",
                                         pre_inp[,146], " ",
                                         pre_inp[,147], " ",
                                         pre_inp[,148], " ",
                                         pre_inp[,149], " ",
                                         pre_inp[,150], " ",
                                         pre_inp[,151], " ",
                                         pre_inp[,152], " ",
                                         pre_inp[,153], " ",
                                         pre_inp[,154], " ",
                                         
                                         pre_inp[,155], " ",
                                         pre_inp[,156], " ", # percent water:
                                         pre_inp[,157], " ", # col 155-181
                                         pre_inp[,158], " ", # 
                                         pre_inp[,159], " ",
                                         pre_inp[,160], " ",
                                         pre_inp[,161], " ",
                                         pre_inp[,162], " ",
                                         pre_inp[,163], " ", #---------
                                         pre_inp[,164], " ",
                                         pre_inp[,165], " ",
                                         pre_inp[,166], " ",
                                         pre_inp[,167], " ",
                                         pre_inp[,168], " ",
                                         pre_inp[,169], " ",
                                         pre_inp[,170], " ",
                                         pre_inp[,171], " ",
                                         pre_inp[,172], " ",
                                         pre_inp[,173], " ",
                                         pre_inp[,174], " ",
                                         pre_inp[,175], " ",
                                         pre_inp[,176], " ",
                                         pre_inp[,177], " ",
                                         pre_inp[,178], " ",
                                         pre_inp[,179], " ",
                                         pre_inp[,180], " ",
                                         pre_inp[,181], " ",
                                         
                                         pre_inp[,182], " ",
                                         pre_inp[,183], " ", # DTN ROAD: TVC (time varying cov)
                                         pre_inp[,184], " ", # col 182-208
                                         pre_inp[,185], " ", # 
                                         pre_inp[,186], " ",
                                         pre_inp[,187], " ",
                                         pre_inp[,188], " ",
                                         pre_inp[,189], " ",
                                         pre_inp[,190], " ",
                                         pre_inp[,191], " ",
                                         pre_inp[,192], " ",
                                         pre_inp[,193], " ",
                                         pre_inp[,194], " ",
                                         pre_inp[,195], " ",
                                         pre_inp[,196], " ",
                                         pre_inp[,197], " ",
                                         pre_inp[,198], " ",
                                         pre_inp[,199], " ",
                                         pre_inp[,200], " ",
                                         pre_inp[,201], " ",
                                         pre_inp[,203], " ",
                                         pre_inp[,204], " ",
                                         pre_inp[,205], " ",
                                         pre_inp[,206], " ",
                                         pre_inp[,207], " ",
                                         pre_inp[,208], " ",
                                         
                                         pre_inp[,209], " ",
                                         pre_inp[,210], " ",
                                         pre_inp[,211], " ", # DTN MPINE: TVC (time varying cov)
                                         pre_inp[,212], " ", # col 209-235
                                         pre_inp[,213], " ", # 
                                         pre_inp[,214], " ",
                                         pre_inp[,215], " ",
                                         pre_inp[,216], " ",
                                         pre_inp[,217], " ",
                                         pre_inp[,218], " ",
                                         pre_inp[,219], " ",
                                         pre_inp[,220], " ",
                                         pre_inp[,221], " ",
                                         pre_inp[,222], " ",
                                         pre_inp[,223], " ",
                                         pre_inp[,224], " ",
                                         pre_inp[,225], " ",
                                         pre_inp[,226], " ",
                                         pre_inp[,227], " ",
                                         pre_inp[,228], " ",
                                         pre_inp[,229], " ",
                                         pre_inp[,230], " ",
                                         pre_inp[,231], " ",
                                         pre_inp[,232], " ",
                                         pre_inp[,233], " ",
                                         pre_inp[,234], " ",
                                         pre_inp[,235], " ",
                                         
                                         pre_inp[,236], " ",
                                         pre_inp[,237], " ",
                                         pre_inp[,238], " ", # DTN GRASSY: TVC (time varying cov)
                                         pre_inp[,239], " ", # col 236:262
                                         pre_inp[,240], " ", # 
                                         pre_inp[,241], " ",
                                         pre_inp[,242], " ",
                                         pre_inp[,243], " ",
                                         pre_inp[,244], " ",
                                         pre_inp[,245], " ",
                                         pre_inp[,246], " ",
                                         pre_inp[,247], " ",
                                         pre_inp[,248], " ",
                                         pre_inp[,249], " ",
                                         pre_inp[,250], " ",
                                         pre_inp[,251], " ",
                                         pre_inp[,252], " ",
                                         pre_inp[,253], " ",
                                         pre_inp[,254], " ",
                                         pre_inp[,255], " ",
                                         pre_inp[,256], " ",
                                         pre_inp[,257], " ",
                                         pre_inp[,258], " ",
                                         pre_inp[,259], " ",
                                         pre_inp[,260], " ",
                                         pre_inp[,261], " ",
                                         pre_inp[,262], " ",
                                         
                                         pre_inp[,263], " ",
                                         pre_inp[,264], " ",
                                         pre_inp[,265], " ", # DTN DECID: TVC (time varying cov)
                                         pre_inp[,266], " ", # col 254:289
                                         pre_inp[,267], " ", # 
                                         pre_inp[,268], " ",
                                         pre_inp[,269], " ",
                                         pre_inp[,270], " ",
                                         pre_inp[,271], " ",
                                         pre_inp[,272], " ",
                                         pre_inp[,273], " ",
                                         pre_inp[,274], " ",
                                         pre_inp[,275], " ",
                                         pre_inp[,276], " ",
                                         pre_inp[,277], " ",
                                         pre_inp[,278], " ",
                                         pre_inp[,279], " ",
                                         pre_inp[,280], " ",
                                         pre_inp[,281], " ",
                                         pre_inp[,282], " ",
                                         pre_inp[,283], " ",
                                         pre_inp[,284], " ",
                                         pre_inp[,285], " ",
                                         pre_inp[,286], " ",
                                         pre_inp[,287], " ",
                                         pre_inp[,288], " ",
                                         pre_inp[,289], " ",
                                         
                                         pre_inp[,290], " ",
                                         pre_inp[,291], " ",
                                         pre_inp[,292], " ", # DTN MPINE: TVC (time varying cov)
                                         pre_inp[,293], " ", # col 290:316
                                         pre_inp[,294], " ", # 
                                         pre_inp[,295], " ",
                                         pre_inp[,296], " ",
                                         pre_inp[,297], " ",
                                         pre_inp[,298], " ",
                                         pre_inp[,299], " ",
                                         pre_inp[,300], " ",
                                         pre_inp[,301], " ",
                                         pre_inp[,302], " ",
                                         pre_inp[,303], " ",
                                         pre_inp[,304], " ",
                                         pre_inp[,305], " ",
                                         pre_inp[,306], " ",
                                         pre_inp[,307], " ",
                                         pre_inp[,308], " ",
                                         pre_inp[,309], " ",
                                         pre_inp[,310], " ",
                                         pre_inp[,311], " ",
                                         pre_inp[,312], " ",
                                         pre_inp[,313], " ",
                                         pre_inp[,314], " ",
                                         pre_inp[,315], " ",
                                         pre_inp[,316], " ",
                                         
                                         pre_inp[,317], " ",
                                         pre_inp[,318], " ",
                                         pre_inp[,319], " ", # DTN WATER: TVC (time varying cov)
                                         pre_inp[,320], " ", # col 317:344
                                         pre_inp[,321], " ", # 
                                         pre_inp[,322], " ",
                                         pre_inp[,323], " ",
                                         pre_inp[,324], " ",
                                         pre_inp[,325], " ",
                                         pre_inp[,326], " ",
                                         pre_inp[,327], " ",
                                         pre_inp[,328], " ",
                                         pre_inp[,329], " ",
                                         pre_inp[,330], " ",
                                         pre_inp[,331], " ",
                                         pre_inp[,332], " ",
                                         pre_inp[,333], " ",
                                         pre_inp[,334], " ",
                                         pre_inp[,335], " ",
                                         pre_inp[,336], " ",
                                         pre_inp[,337], " ",
                                         pre_inp[,338], " ",
                                         pre_inp[,339], " ",
                                         pre_inp[,340], " ",
                                         pre_inp[,341], " ",
                                         pre_inp[,342], " ",
                                         pre_inp[,343], " ",
                                         
                                         pre_inp[,344], " ", #PERC BURN: TVC (time varying cov)
                                         pre_inp[,345], " ",
                                         pre_inp[,346], " ",
                                         pre_inp[,347], " ",
                                         pre_inp[,348], " ",
                                         pre_inp[,349], " ",
                                         pre_inp[,350], " ",
                                         pre_inp[,351], " ",
                                         pre_inp[,352], " ",
                                         pre_inp[,353], " ",
                                         pre_inp[,354], " ",
                                         pre_inp[,355], " ",
                                         pre_inp[,356], " ",
                                         pre_inp[,357], " ",
                                         pre_inp[,358], " ",
                                         pre_inp[,359], " ",
                                         pre_inp[,360], " ",
                                         pre_inp[,361], " ",
                                         pre_inp[,362], " ",
                                         pre_inp[,363], " ",
                                         pre_inp[,364], " ",
                                         pre_inp[,365], " ", 
                                         pre_inp[,366], " ", 
                                         pre_inp[,367], " ", 
                                         pre_inp[,368], " ",  
                                         pre_inp[,369], " ",
                                         pre_inp[,370], " ",
                                         
                                         pre_inp[,371], " ",# BUSH TREATMENT BINARY: TVC (time varying cov)
                                         pre_inp[,372], " ",
                                         pre_inp[,373], " ",
                                         pre_inp[,374], " ",
                                         pre_inp[,375], " ",
                                         pre_inp[,376], " ",
                                         pre_inp[,377], " ",
                                         pre_inp[,378], " ",
                                         pre_inp[,379], " ",
                                         pre_inp[,380], " ",
                                         pre_inp[,381], " ",
                                         pre_inp[,382], " ",
                                         pre_inp[,383], " ",
                                         pre_inp[,384], " ",
                                         pre_inp[,385], " ",
                                         pre_inp[,386], " ",
                                         pre_inp[,387], " ",
                                         pre_inp[,388], " ",
                                         pre_inp[,389], " ",
                                         pre_inp[,390], " ",
                                         pre_inp[,391], " ",
                                         pre_inp[,392], " ",
                                         pre_inp[,393], " ", 
                                         pre_inp[,393], " ", 
                                         pre_inp[,394], " ", # 
                                         pre_inp[,395], " ",
                                         pre_inp[,396], " ",
                                         pre_inp[,397], " ",
                                         
                                         pre_inp[,398], " ", # BLOWER: TVC (time varying cov)
                                         pre_inp[,399], " ",
                                         pre_inp[,400], " ",
                                         pre_inp[,401], " ",
                                         pre_inp[,402], " ",
                                         pre_inp[,403], " ",
                                         pre_inp[,404], " ",
                                         pre_inp[,405], " ",
                                         pre_inp[,406], " ",
                                         pre_inp[,407], " ",
                                         pre_inp[,408], " ",
                                         pre_inp[,409], " ",
                                         pre_inp[,410], " ",
                                         pre_inp[,411], " ",
                                         pre_inp[,412], " ",
                                         pre_inp[,413], " ",
                                         pre_inp[,414], " ",
                                         pre_inp[,415], " ",
                                         pre_inp[,416], " ",
                                         pre_inp[,417], " ",
                                         pre_inp[,418], " ",
                                         pre_inp[,419], " ", # 
                                         pre_inp[,420], " ", # BLAZE IT 
                                         pre_inp[,421], " ", # 
                                         pre_inp[,422], " ",
                                         pre_inp[,423], " ",
                                         pre_inp[,424], " ",
                                         
                                         pre_inp[,425], " ", #BURN STAT BINARY : TVC (time varying cov)
                                         pre_inp[,426], " ",
                                         pre_inp[,427], " ",
                                         pre_inp[,428], " ",
                                         pre_inp[,429], " ",  
                                         pre_inp[,430], " ",
                                         pre_inp[,431], " ",
                                         pre_inp[,432], " ",
                                         pre_inp[,433], " ",
                                         pre_inp[,434], " ",
                                         pre_inp[,435], " ",
                                         pre_inp[,436], " ",
                                         pre_inp[,437], " ",
                                         pre_inp[,438], " ",
                                         pre_inp[,439], " ",
                                         pre_inp[,440], " ",
                                         pre_inp[,441], " ",
                                         pre_inp[,442], " ",
                                         pre_inp[,443], " ",
                                         pre_inp[,444], " ",
                                         pre_inp[,445], " ",
                                         pre_inp[,446], " ",
                                         pre_inp[,447], " ",
                                         pre_inp[,448], " ",
                                         pre_inp[,449], " ",
                                         pre_inp[,450], " ",
                                         pre_inp[,451], ";"
                                         ))
View(new_inp)



# export
# write.csv(new_inp, "E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/Survival/knownfate_tVC_ Nonbreeding_29Oct23.inp", row.names = FALSE)








