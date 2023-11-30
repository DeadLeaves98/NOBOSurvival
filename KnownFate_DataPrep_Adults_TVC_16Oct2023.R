# Title: 
#              ---- Breeding Season Adult Survival ----
#              ---------- Encounter History -----------
# Date Created: Autumn Randall 
# Reference: Breeding season known fate data prep 
# Reference Author: Dr. DJ McNeil 
# Last Edited: 10/14/2023
#########################################################################

library(dplyr); library(raster); library(tidyr); library(stringr);library(rgdal); 
library(rgeos); library(sf); library(lubridate); library(stringr)

# DJ pathway
# nobo1 <- read.csv("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/NOBOtelemetry_all_26sept2023_asr.csv")
# Autumn pathway
nobo1 <- read.csv("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Telemetry Data/NOBOtelemetry_all_26sept2023_asr.csv")

nrow(nobo1) # 35990

# clean this file up by removing the columns we do not need
nobo1 <- dplyr::select(nobo1, -GlobalID, -Time, -Burn.Status, -Habitat.Type,
                       -Associated.Bird..1, -Associated.Bird..2, -Associated.Bird..3,
                       -Associated.Bird..4, -Associated.Bird..5, -Associated.Bird..6,
                       -Associated.Bird..7, -Associated.Bird..8, -Associated.Bird..9,
                       -Associated.Bird..10, -Parent.Present., -Enter.Adult.ID,
                       -Comments, -CreationDate, -Creator, -EditDate, -Editor)

nobo1 <- subset(nobo1, Location.Type != "Chick") # remove chicks
nobo1 <- arrange(nobo1, ObjectID) # re-order rows in order of objectID

# I need to subset by date to remove the chicks based off the year the were chicks 
# This is to ensure any chicks we tagged that carried over into the following year (chick transmitter followed by an adult collar) 
# are included within our analyses 

    # Autumn: check into this. There may be a bird or two that actually BELONG here.
    # split the data in half, in 2022, eliminate chicks that are from 2022... in 2023,
    # get rid of chicks that are "235...", etc., then re-merge the dataset

nobo1 <- nobo1 |> mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
# Some chicks were entered as broods and we need to remove from the data 
# View(nobosub2022)
nrow(nobo1) #29710
nobo1$chick = data.frame("band" = substr(nobo1$Bird.ID, 9, 11))


nobosub2022 = nobo1[nobo1$Date >= "2022-01-01" & nobo1$Date <= "2022-10-01", ]
length(unique(nobosub2022$Bird.ID))
nobosub2022 = subset(nobosub2022, chick!=225)
length(unique(nobosub2022$Bird.ID))

nobosub2023 = nobo1[nobo1$Date >= "2023-01-01" & nobo1$Date <= "2023-10-01", ]
length(unique(nobosub2023$Bird.ID))
nobosub2023 = subset(nobosub2023, chick!=235)
length(unique(nobosub2023$Bird.ID))

nobo2 = rbind(nobosub2022, nobosub2023)

nobo1 = nobo2

nrow(nobo1) # 28506

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
nrow(alives) + nrow(fatedead) + nrow(fatecensor) + nrow(censors) == nrow(nobo1) # 33814 rows
# add encounter
alives$encounter = 1
fatedead$encounter = 2
fatecensor$encounter = 0
censors$encounter = 0
nobo1 <- rbind(alives, fatedead, fatecensor, censors)
nobo1 <- nobo1 %>% arrange(ObjectID) # re-order columns

# modifying the dates to something easier to handle
nobodates <- data.frame("Date" = nobo1$Date)
nobodates$Date <- str_replace(nobodates$Date, " AM", "") #replace AM with nothing
nobodates$Date <- str_replace(nobodates$Date, " PM", "") #replace PM with nothing
nobodates <- separate(nobodates, Date, into = c("date", "time"), sep = " ") # split up day and time
# WARNING MESSAGE! "Expected 2 pieces. Missing pieces..." this is b/c some records have no times - just dates
nobodates <- separate(nobodates, date, into = c("year", "month", "day"), sep = "-") # split up month, day, year


# IF we need the code that fixes the issue that occurred within the year column that had some years 
  # as 202 it is what is below
      # error somewhere that turned some years (dates) into 202 rather than 2022 
      # this fixes it in a wild roundabout way 
      # nobodates$year <- str_replace(nobodates$year, "2023", "a") 
      # nobodates$year <- str_replace(nobodates$year, "2022", "b") 
      # nobodates$year <- str_replace(nobodates$year, "202", "2022") 
      # nobodates$year <- str_replace(nobodates$year, "b", "2022") 
      # nobodates$year <- str_replace(nobodates$year, "a", "2023") 
  
nobodates <- separate(nobodates, time, into = c("hour", "min"), sep = ":") # split up hour, minute, and second
nobo2 <- cbind(nobo1, nobodates)

# make month, day, and year numeric
nobo2$month <- as.numeric(nobo2$month); nobo2$day <- as.numeric(nobo2$day); nobo2$year <- as.numeric(nobo2$year)
nobo2$CombinedDate0 <- paste0(nobo2$day, "_", nobo2$month)

# add ordinal date using the lookup table
lookuptable1 <- read.csv("E:/NOBO Project Data/Analyses/NonBreeding Season/Nonbreeding_Survival/week_lookuptable.csv")
  # lookuptable1 <- read.csv("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/week_lookuptable_10oct2023.csv")
nobo_ord <- data.frame("ObjectID" = nobo2$ObjectID,"CombinedDate0" = nobo2$CombinedDate0)
merge1 <- merge(x = nobo_ord, by.x = "CombinedDate0", y = lookuptable1, by.y = "day_month", all.x = TRUE)
merge1 <- merge1 %>% arrange(ObjectID) # re-order columns...  NO FUCKING CLUE why merge() re-orders our columns...
nobo2 <-cbind(nobo2, merge1[,5:8]) # adding breeding season, ordinal, and week
nobo1 <- nobo2 # turn it back into nobo1

# clean nobo1 up by removing extra columns
nobo1 <- dplyr::select(nobo1, -month, -day, -hour, -min, -CombinedDate0, -Date)
nrow(nobo1) # 28506

# filter to be both breeding seasons 
breeding1 = subset(nobo1, breedingseasonCov == 1) # filters for BOTH summers 
nrow(breeding1) # 23744

# Now add a column that coincides with year that will be a; 0 = 2022, 1 = 2023
breeding1$year = ifelse(breeding1$year == 2023, 1, 0)

## Extract covariate values at each point
breeding_sp <- SpatialPoints(coords = data.frame("x" = breeding1$x, "y" = breeding1$y)) # convert DF to Spatial Points
crs(breeding_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)

courses <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/shapefiles/OrtonCourses.shp")
# courses <- readOGR("C:/Users/User/Desktop/Autumn_analyses/TimeVaryingCovPrep/shapefiles/OrtonCourses.shp")
plot(courses)
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
breeding_sp1 <- spTransform(breeding_sp, crs(NDVI)) # transform to NAD83
plot(NDVI)
plot(breeding_sp1, add = TRUE)
ndv1_ex <- raster::extract(x = NDVI, y = breeding_sp1)
perc_mpine_ex <- raster::extract(x = perc_mpine, y = breeding_sp1)
perc_grassy_ex <- raster::extract(x = perc_grassy, y = breeding_sp1)
perc_decid_ex <- raster::extract(x = perc_decid, y = breeding_sp1)
perc_bf_ex <- raster::extract(x = perc_bf, y = breeding_sp1)
perc_water_ex <- raster::extract(x = perc_water, y = breeding_sp1)

# reproject "breeding" to match DTN rasters and extract
breeding_sp2 <- spTransform(breeding_sp, crs(DTN_road)) # transform to pseudo-mercator
plot(DTN_road); plot(breeding_sp2, add = TRUE)
DTN_road_ex <- raster::extract(x = DTN_road, y = breeding_sp2)
DTN_mpine_ex <- raster::extract(x = DTN_mpine, y = breeding_sp2)
DTN_grassy_ex <- raster::extract(x = DTN_grassy, y = breeding_sp2)
DTN_decid_ex <- raster::extract(x = DTN_decid, y = breeding_sp2)
DTN_bf_ex <- raster::extract(x = DTN_bf, y = breeding_sp2)
DTN_water_ex <- raster::extract(x = DTN_water, y = breeding_sp2)

# reproject "breeding" to match "courses" shapefile and extract with over()
breeding_sp3 <- spTransform(breeding_sp, crs(courses)) # transform to pseudo-mercator
plot(courses); plot(breeding_sp3, add = TRUE)
course_ex <- over(x = breeding_sp3, y = courses)

# add columns to breeding and export
breeding1$ndvi <- ndv1_ex
breeding1$perc_mpine <- perc_mpine_ex
breeding1$perc_grassy <- perc_grassy_ex
breeding1$perc_decid <- perc_decid_ex
breeding1$perc_bf <- perc_bf_ex
breeding1$perc_water <- perc_water_ex
breeding1$DTN_road <- DTN_road_ex
breeding1$DTN_mpine <- DTN_mpine_ex
breeding1$DTN_grassy <- DTN_grassy_ex
breeding1$DTN_decid <- DTN_decid_ex
breeding1$DTN_bf <- DTN_bf_ex
breeding1$DTN_water <- DTN_water_ex
breeding1$course <- course_ex$course 

# fix NAs in course ID
breeding1$course[is.na(breeding1$course)] <- "unknown"

# Creating "bush4" from courseID (billjones, darkbranch, and fenceove are "1" for bush4)
breeding1$bush4 <- ifelse(breeding1$course == "billjones" |
                            breeding1$course == "darkbranch" | 
                            breeding1$course == "fencecove", 1, 0)
unique(breeding1$bush4)

# Creating "blower" from courseID (billjones, bigbay, and bluepond are "1" for blower)
breeding1$blower <- ifelse(breeding1$course == "billjones" |
                            breeding1$course == "bigbay" | 
                            breeding1$course == "bluepond", 1, 0)
unique(breeding1$blower)

#  replace NAs in NDVI and percent_...; DTN- should always have values
# IDK why NDVI is zero sometimes...
breeding1$ndvi[is.na(breeding1$ndvi)] <- 0
breeding1$perc_mpine[is.na(breeding1$perc_mpine)] <- 0
breeding1$perc_grassy[is.na(breeding1$perc_grassy)] <- 0
breeding1$perc_decid[is.na(breeding1$perc_decid)] <- 0
breeding1$perc_bf[is.na(breeding1$perc_bf)] <- 0
breeding1$perc_water[is.na(breeding1$perc_water)] <- 0
#View(breeding1)
nrow(breeding1) # 23942

unique(breeding1$course)

# assign "burn status" and "perc_burn" to each individual point
birds2022 <- subset(breeding1, year == 0) # # first subset by year; year 1
birds2023 <- subset(breeding1, year == 1) # year 2

birds22_sp <- SpatialPoints(coords = data.frame("x" = birds2022$x, "y" = birds2022$y)) # convert DF to Spatial Points
birds23_sp <- SpatialPoints(coords = data.frame("x" = birds2023$x, "y" = birds2023$y)) # convert DF to Spatial Points

crs(birds22_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc.)
crs(birds23_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc.)

birds22_sp = spTransform(birds22_sp, crs(burn2022)) # reproject birds 2022 and 2023 and extract
birds23_sp = spTransform(birds23_sp, crs(burn2023))

# extract "percent burn"
perc_burn22_ex <- raster::extract(x = burn2022, y = birds22_sp) 
perc_burn23_ex <- raster::extract(x = burn2023, y = birds23_sp)

# extract "burn_status"
plot(burnBinary2022); plot(birds22_sp, add = TRUE) # just making sure it looks OK
burn_stat22_ex <- raster::extract(x = burnBinary2022, y = birds22_sp) 
burn_stat23_ex <- raster::extract(x = burnBinary2023, y = birds23_sp)

# add columns 
birds2022$perc_burn <- perc_burn22_ex # percent burn, year 2022
birds2022$burn_stat <- burn_stat22_ex # burn status, year 2022

birds2023$perc_burn <- perc_burn23_ex # percent burn, year 2023
birds2023$burn_stat <- burn_stat23_ex # burn status, year 2023

# replace any NA
birds2022$perc_burn[is.na(birds2022$perc_burn)] <- 0
birds2023$perc_burn[is.na(birds2023$perc_burn)] <- 0
birds2022$burn_stat[is.na(birds2022$burn_stat)] <- 0
birds2023$burn_stat[is.na(birds2023$burn_stat)] <- 0
unique(birds2022$perc_burn); unique(birds2022$burn_stat)
unique(birds2023$perc_burn); unique(birds2023$burn_stat)
# now rbind them back together and call it breeding1 
dat00 = rbind(birds2022, birds2023)
nrow(dat00)
breeding1 = dat00 

#### For() loop that creates input file
birdlist <- unique(breeding1$Bird.ID) # the list of birds to be processed
length(unique(breeding1$Bird.ID)) # how many? n = 683
breeding1 <- breeding1[order(breeding1$year),] # reorder by year
breeding1 <- breeding1[order(breeding1$week),] # then reorder by week
length(unique(breeding1$week))
nrow(breeding1)

inp_df <- data.frame("birdID" = NA, 
                     "wk14" = NA, "wk15" = NA,
                     "wk16" = NA, "wk17" = NA, "wk18" = NA, "wk19" = NA,
                     "wk20" = NA, "wk21" = NA, "wk22" = NA, "wk23" = NA,
                     "wk24" = NA, "wk25" = NA, "wk26" = NA, "wk27" = NA,
                     "wk28" = NA, "wk29" = NA, "wk30" = NA, "wk31" = NA,
                     "wk32" = NA, "wk33" = NA, "wk34" = NA, "wk35" = NA,
                     "wk36" = NA, "wk37" = NA, "wk38" = NA, "wk39" = NA,
                     "ndvi" = NA, "perc_mpine" = NA, "perc_grassy" = NA, "perc_decid" = NA, 
                     "perc_bf" = NA, "perc_water" = NA, "DTN_road" = NA, "DTN_mpine" = NA, 
                     "DTN_grassy" = NA, "DTN_decid" = NA, "DTN_bf" = NA, "DTN_water" = NA,
                     "perc_burn" = NA,
                     "course" = NA, "year" = NA, "x" = NA, "y" = NA, 
                     "TrackedAlive" = NA)

for(i in 1:length(birdlist)){
  #i = 271
  bird_i_data <- subset(breeding1, Bird.ID == birdlist[i])
  vals <- c()
  
   for(j in 14:39){ # this generates a detection history of sorts; 0 = not tracked, 1 = alive, 2 = dead, NA = not tracked
    #j = 18
    week_j_data <- subset(bird_i_data, week == j)
    val_j <- ifelse(nrow(week_j_data) == 0, NA, max(week_j_data$encounter)) # if no data, give NA, if data, take max of encounter
    vals <- c(vals, val_j)
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
                       "wk14" = vals[1], "wk15" = vals[2], "wk16" = vals[3], "wk17" = vals[4],
                       "wk18" = vals[5], "wk19" = vals[6], "wk20" = vals[7], "wk21" = vals[8],
                       "wk22" = vals[9], "wk23" = vals[10], "wk24" = vals[11], "wk25" = vals[12],
                       "wk26" = vals[13], "wk27" = vals[14], "wk28" = vals[15], "wk29" = vals[16],
                       "wk30" = vals[17], "wk31" = vals[18], "wk32" = vals[19], "wk33" = vals[20],
                       "wk34" = vals[21], "wk35" = vals[22], "wk36" = vals[23], "wk37" = vals[24],
                       "wk38" = vals[25], "wk39" = vals[26],
                       "ndvi" = mean(bird_i_data$ndvi), "perc_mpine" = mean(bird_i_data$perc_mpine), 
                       "perc_grassy" = mean(bird_i_data$perc_grassy), "perc_decid" = mean(bird_i_data$perc_decid), 
                       "perc_bf" = mean(bird_i_data$perc_bf), "perc_water" = mean(bird_i_data$perc_water), 
                       "DTN_road" = mean(bird_i_data$DTN_road), "DTN_mpine" = mean(bird_i_data$DTN_mpine), 
                       "DTN_grassy" = mean(bird_i_data$DTN_grassy), "DTN_decid" = mean(bird_i_data$DTN_decid), 
                       "DTN_bf" = mean(bird_i_data$DTN_bf), "DTN_water" = mean(bird_i_data$DTN_water),
                       "perc_burn" = mean(bird_i_data$perc_burn),  
                        "course" = course_i, "year" = year_i, "x" = meanX, "y" = meanY, 
                        "TrackedAlive" = sum(lengths(regmatches(vals, gregexpr("1", vals)))))

  # add new row to "inp_df"
   inp_df <- rbind(inp_df, newrow)
  }

######################### Remove birds with "0" tracked alive occasions
inp_df <- inp_df[2:(nrow(inp_df)), ] # remove the blank row at the top 
inp_df <- subset(inp_df, TrackedAlive > 0)

# turn NA into "unknown"
inp_df$course[is.na(inp_df$course)]  <- "unknown"


######################### adding individual covariates
bird_data <- read.csv("E:/NOBO Project Data/Analyses/Trap Data/TrapData_08_29_2023 (1)/Trap_Data_0.csv")
bird_data$ID= paste(bird_data$Frequency_at_Release, bird_data$Band.ID, sep="_") # creates a bird id column (xxx.xxx_xxxxxxx)
bird_data <- bird_data[,c(6:9, 26)] # removes unnecessary columns 

inp_df2 <- merge(x = inp_df, by.x = "birdID", y = bird_data, by.y = "ID", all.x = TRUE)
#View(inp_df2)
length(unique(inp_df2$birdID)) #614

# check for duplicates 
sum(duplicated(inp_df2$birdID)) # 11 duplicates 
d = duplicated(inp_df2$birdID) 
d # this will show the locations

# remove the duplicates (11) that are caused by birds being recaptured and reentered w/o changing freq
test1 = inp_df2[!duplicated(inp_df2$birdID), ]
length(unique(test1$birdID)) #614

# check to see if it worked
nrow(inp_df2) #625
nrow(test1) #614 -- aka removed the 11 duplicates 

# convert back to the original df name now that i know I didnt fuck it up 
inp_df2 = test1

#View(test1)
# making sex and age into binary values
inp_df2$sex <- ifelse(inp_df2$Sex == "Male", 0, 1)
inp_df2$age <- ifelse(inp_df2$Age == "Juvenile", 0, 1)
#View(inp_df2)

# Removes more unneeded columns 
inp_df2 <- dplyr::select(inp_df2, -Sex, -Age, -TrackedAlive, -course, -Immature, -x, -y)

### Fixing NAs in birds' individual covariates (sex, age, etc.)
colSums(is.na(inp_df2)) # look at number of nas in each column 

inp_df2[,"Weight"][inp_df2[,"Weight"] == 0] <- round(mean(inp_df2[,"Weight"], na.rm = TRUE),0) # turn weights of 0 into mean weight
inp_df2$Weight[is.na(inp_df2$Weight)] <- round(mean(inp_df2[,"Weight"], na.rm = TRUE),0) # turn weights of NA into mean weight

# assume birds with age = NA are actually juveniles
inp_df2$age[is.na(inp_df2$age)] <- 0 # turn age of NA to 0

# assume birds with sex = NA are actually females
inp_df2$age[is.na(inp_df2$age)] <- 1 # turn sex of NA to 1

######################### creating encounter history
neweo <- inp_df2$birdID# new encounter occasions
for(i in 2:27){
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
# adjusted to 26 weeks (april 1 - oct 1)
eh <- paste0(neweo[,2],neweo[,3],neweo[,4],neweo[,5],neweo[,6],neweo[,7],neweo[,8],neweo[,9],neweo[,10],
             neweo[,11],neweo[,12],neweo[,13],neweo[,14],neweo[,15],neweo[,16],neweo[,17],neweo[,18],neweo[,19],
             neweo[,20],neweo[,21],neweo[,22],neweo[,23],neweo[,24],neweo[,25],neweo[,26],neweo[,27])
eh <- data.frame("eh" = eh)
#View(eh)

#########################################################################
######################################################################### Bohemoth for() loops
######################################################################### generates time-specific
######################################################################### covariates
#########################################################################

BigDF <- data.frame() # data.frame to hold the time-varying covariates
CovariateNames <- c("ndvi", "perc_mpine", "perc_grassy", "perc_decid", 
                    "perc_bf", "perc_water", "DTN_road", "DTN_mpine", 
                    "DTN_grassy","DTN_decid", "DTN_bf", "DTN_water",
                    "perc_burn", "bush4", "blower", "burn_stat")

for(i in 1:length(unique(inp_df2$birdID))){ # For each bird ....................
  # i = 133 # bird ID of 162.704_220686
  sub <- subset(breeding1, Bird.ID == unique(inp_df2$birdID)[i]) # subset bird i
  covariateList <- list() # create blank list to eventually store completed covariate data.frames
  
  for(j in 1:length(CovariateNames)){ # For each covariate .....................
    # j = 14
    covName <- CovariateNames[j] # isolate jth covariate name
    txt1 <- paste0("bird_i_cov_j <- data.frame('", covName, "' = sub$", covName,")")
    eval(parse(text = txt1)) # create data.frame with the jth covariate as the only column
    bird_i_cov_j$week <- sub$week # add week to "bird_i_cov_j"
    bird_i_cov_j$ordinal <- sub$ordinal # add ordinal to "bird_i_cov_j"
    covHistory <- c() # create blank vector to hold values of the jth covariate
    
    for(k in 14:39){  # For each week ..........................................
      # k = 15
      newval <- subset(bird_i_cov_j, week == k) # subset to week k
      newval <- ifelse(nrow(newval) == 0, # ifelse() that gives the mean value to any weeks with no cov value
                       mean(bird_i_cov_j[,1]), # assign mean if nrow = 0
                       subset(newval, ordinal == max(newval$ordinal))[1,1]) # otherwise take the max in week k
      # the prev ifelse() gives the mean for missing values; 
      # we don't want this for burn_stat, bush4, and blower
      newval <- ifelse(j >= 14 & newval<1 & newval>0, 0, newval) # for covs 14, 15, and 16, non-binary values become 0
      newval <- round(newval, digits = 2) # round to two digits
      covHistory <- c(covHistory, newval) # add newval to covHistory
    }
    
    covHistoryDF <- data.frame("wk14" = 1, "wk15" = 1, "wk16" = 1,             # create DF to hold covHistory
                               "wk17" = 1, "wk18" = 1, "wk19" = 1, "wk20" = 1, # values set to 1 as a placeholder
                               "wk21" = 1, "wk22" = 1, "wk23" = 1, "wk24" = 1,
                               "wk25" = 1, "wk26" = 1, "wk27" = 1, "wk28" = 1,
                               "wk29" = 1, "wk30" = 1, "wk31" = 1, "wk32" = 1,
                               "wk33" = 1, "wk34" = 1, "wk35" = 1, "wk36" = 1,
                               "wk37" = 1, "wk38" = 1, "wk39" = 1)
    covHistoryDF <- rbind(covHistoryDF, covHistory) # add covHistory with rbind()
    covHistoryDF <- covHistoryDF[2,] # remove row of 1's
    covariateList[[j]] <- covHistoryDF # add covHistory data.frame() to covariateList!
    # covariateList
  }
  
  names(covariateList) <- CovariateNames # name all elements in covariateList
  covariateList
  
  # creating a big data.frame to hold all of the columns for bird i's data
  Bird_i_BigDF <- data.frame("birdID" = unique(inp_df2$birdID)[i],
                             "ndvi14" = covariateList$ndvi$wk14,                                   # ndvi
                             "ndvi15" = covariateList$ndvi$wk15, "ndvi16" = covariateList$ndvi$wk16, 
                             "ndvi17" = covariateList$ndvi$wk17, "ndvi18" = covariateList$ndvi$wk18, 
                             "ndvi19" = covariateList$ndvi$wk19, "ndvi20" = covariateList$ndvi$wk20,
                             "ndvi21" = covariateList$ndvi$wk21, "ndvi22" = covariateList$ndvi$wk22, 
                             "ndvi23" = covariateList$ndvi$wk23, "ndvi24" = covariateList$ndvi$wk24,
                             "ndvi25" = covariateList$ndvi$wk25, "ndvi26" = covariateList$ndvi$wk26, 
                             "ndvi27" = covariateList$ndvi$wk27, "ndvi28" = covariateList$ndvi$wk28,
                             "ndvi29" = covariateList$ndvi$wk29, "ndvi30" = covariateList$ndvi$wk30, 
                             "ndvi31" = covariateList$ndvi$wk31, "ndvi32" = covariateList$ndvi$wk32,
                             "ndvi33" = covariateList$ndvi$wk33, "ndvi34" = covariateList$ndvi$wk34, 
                             "ndvi35" = covariateList$ndvi$wk35, "ndvi36" = covariateList$ndvi$wk36,
                             "ndvi37" = covariateList$ndvi$wk37, "ndvi38" = covariateList$ndvi$wk38,
                             "ndvi39" = covariateList$ndvi$wk39,
                             
                             "p_mpine14" = covariateList$perc_mpine$wk14,                                            # perc_mpine
                             "p_mpine15" = covariateList$perc_mpine$wk15, "p_mpine16" = covariateList$perc_mpine$wk16, 
                             "p_mpine17" = covariateList$perc_mpine$wk17, "p_mpine18" = covariateList$perc_mpine$wk18, 
                             "p_mpine19" = covariateList$perc_mpine$wk19, "p_mpine20" = covariateList$perc_mpine$wk20,
                             "p_mpine21" = covariateList$perc_mpine$wk21, "p_mpine22" = covariateList$perc_mpine$wk22, 
                             "p_mpine23" = covariateList$perc_mpine$wk23, "p_mpine24" = covariateList$perc_mpine$wk24,
                             "p_mpine25" = covariateList$perc_mpine$wk25, "p_mpine26" = covariateList$perc_mpine$wk26, 
                             "p_mpine27" = covariateList$perc_mpine$wk27, "p_mpine28" = covariateList$perc_mpine$wk28,
                             "p_mpine29" = covariateList$perc_mpine$wk29, "p_mpine30" = covariateList$perc_mpine$wk30, 
                             "p_mpine31" = covariateList$perc_mpine$wk31, "p_mpine32" = covariateList$perc_mpine$wk32,
                             "p_mpine33" = covariateList$perc_mpine$wk33, "p_mpine34" = covariateList$perc_mpine$wk34, 
                             "p_mpine35" = covariateList$perc_mpine$wk35, "p_mpine36" = covariateList$perc_mpine$wk36,
                             "p_mpine37" = covariateList$perc_mpine$wk37, "p_mpine38" = covariateList$perc_mpine$wk38,
                             "p_mpine39" = covariateList$perc_mpine$wk39,
                             
                             "p_grassy14" = covariateList$perc_grassy$wk14,                                               # perc_grassy
                             "p_grassy15" = covariateList$perc_grassy$wk15, "p_grassy16" = covariateList$perc_grassy$wk16, 
                             "p_grassy17" = covariateList$perc_grassy$wk17, "p_grassy18" = covariateList$perc_grassy$wk18, 
                             "p_grassy19" = covariateList$perc_grassy$wk19, "p_grassy20" = covariateList$perc_grassy$wk20,
                             "p_grassy21" = covariateList$perc_grassy$wk21, "p_grassy22" = covariateList$perc_grassy$wk22, 
                             "p_grassy23" = covariateList$perc_grassy$wk23, "p_grassy24" = covariateList$perc_grassy$wk24,
                             "p_grassy25" = covariateList$perc_grassy$wk25, "p_grassy26" = covariateList$perc_grassy$wk26, 
                             "p_grassy27" = covariateList$perc_grassy$wk27, "p_grassy28" = covariateList$perc_grassy$wk28,
                             "p_grassy29" = covariateList$perc_grassy$wk29, "p_grassy30" = covariateList$perc_grassy$wk30, 
                             "p_grassy31" = covariateList$perc_grassy$wk31, "p_grassy32" = covariateList$perc_grassy$wk32,
                             "p_grassy33" = covariateList$perc_grassy$wk33, "p_grassy34" = covariateList$perc_grassy$wk34, 
                             "p_grassy35" = covariateList$perc_grassy$wk35, "p_grassy36" = covariateList$perc_grassy$wk36,
                             "p_grassy37" = covariateList$perc_grassy$wk37, "p_grassy38" = covariateList$perc_grassy$wk38,
                             "p_grassy39" = covariateList$perc_grassy$wk39,
                             
                             "p_decid14" = covariateList$perc_decid$wk14,                                             # perc_decid
                             "p_decid15" = covariateList$perc_decid$wk15, "p_decid16" = covariateList$perc_decid$wk16, 
                             "p_decid17" = covariateList$perc_decid$wk17, "p_decid18" = covariateList$perc_decid$wk18, 
                             "p_decid19" = covariateList$perc_decid$wk19, "p_decid20" = covariateList$perc_decid$wk20,
                             "p_decid21" = covariateList$perc_decid$wk21, "p_decid22" = covariateList$perc_decid$wk22, 
                             "p_decid23" = covariateList$perc_decid$wk23, "p_decid24" = covariateList$perc_decid$wk24,
                             "p_decid25" = covariateList$perc_decid$wk25, "p_decid26" = covariateList$perc_decid$wk26, 
                             "p_decid27" = covariateList$perc_decid$wk27, "p_decid28" = covariateList$perc_decid$wk28,
                             "p_decid29" = covariateList$perc_decid$wk29, "p_decid30" = covariateList$perc_decid$wk30, 
                             "p_decid31" = covariateList$perc_decid$wk31, "p_decid32" = covariateList$perc_decid$wk32,
                             "p_decid33" = covariateList$perc_decid$wk33, "p_decid34" = covariateList$perc_decid$wk34, 
                             "p_decid35" = covariateList$perc_decid$wk35, "p_decid36" = covariateList$perc_decid$wk36,
                             "p_decid37" = covariateList$perc_decid$wk37, "p_decid38" = covariateList$perc_decid$wk38,
                             "p_decid39" = covariateList$perc_decid$wk39,
                             
                             "p_bf14" = covariateList$perc_bf$wk14,                                      # perc_bf
                             "p_bf15" = covariateList$perc_bf$wk15, "p_bf16" = covariateList$perc_bf$wk16, 
                             "p_bf17" = covariateList$perc_bf$wk17, "p_bf18" = covariateList$perc_bf$wk18, 
                             "p_bf19" = covariateList$perc_bf$wk19, "p_bf20" = covariateList$perc_bf$wk20,
                             "p_bf21" = covariateList$perc_bf$wk21, "p_bf22" = covariateList$perc_bf$wk22, 
                             "p_bf23" = covariateList$perc_bf$wk23, "p_bf24" = covariateList$perc_bf$wk24,
                             "p_bf25" = covariateList$perc_bf$wk25, "p_bf26" = covariateList$perc_bf$wk26, 
                             "p_bf27" = covariateList$perc_bf$wk27, "p_bf28" = covariateList$perc_bf$wk28,
                             "p_bf29" = covariateList$perc_bf$wk29, "p_bf30" = covariateList$perc_bf$wk30, 
                             "p_bf31" = covariateList$perc_bf$wk31, "p_bf32" = covariateList$perc_bf$wk32,
                             "p_bf33" = covariateList$perc_bf$wk33, "p_bf34" = covariateList$perc_bf$wk34, 
                             "p_bf35" = covariateList$perc_bf$wk35, "p_bf36" = covariateList$perc_bf$wk36,
                             "p_bf37" = covariateList$perc_bf$wk37, "p_bf38" = covariateList$perc_bf$wk38,
                             "p_bf39" = covariateList$perc_bf$wk39,
                             
                             "p_water14" = covariateList$perc_water$wk14,                                             # perc_water
                             "p_water15" = covariateList$perc_water$wk15, "p_water16" = covariateList$perc_water$wk16, 
                             "p_water17" = covariateList$perc_water$wk17, "p_water18" = covariateList$perc_water$wk18, 
                             "p_water19" = covariateList$perc_water$wk19, "p_water20" = covariateList$perc_water$wk20,
                             "p_water21" = covariateList$perc_water$wk21, "p_water22" = covariateList$perc_water$wk22, 
                             "p_water23" = covariateList$perc_water$wk23, "p_water24" = covariateList$perc_water$wk24,
                             "p_water25" = covariateList$perc_water$wk25, "p_water26" = covariateList$perc_water$wk26, 
                             "p_water27" = covariateList$perc_water$wk27, "p_water28" = covariateList$perc_water$wk28,
                             "p_water29" = covariateList$perc_water$wk29, "p_water30" = covariateList$perc_water$wk30, 
                             "p_water31" = covariateList$perc_water$wk31, "p_water32" = covariateList$perc_water$wk32,
                             "p_water33" = covariateList$perc_water$wk33, "p_water34" = covariateList$perc_water$wk34, 
                             "p_water35" = covariateList$perc_water$wk35, "p_water36" = covariateList$perc_water$wk36,
                             "p_water37" = covariateList$perc_water$wk37, "p_water38" = covariateList$perc_water$wk38,
                             "p_water38" = covariateList$perc_water$wk39,
                             
                             "dtn_road14" = covariateList$DTN_road$wk14,                                           # DTN_road
                             "dtn_road15" = covariateList$DTN_road$wk15, "dtn_road16" = covariateList$DTN_road$wk16, 
                             "dtn_road17" = covariateList$DTN_road$wk17, "dtn_road18" = covariateList$DTN_road$wk18, 
                             "dtn_road19" = covariateList$DTN_road$wk19, "dtn_road20" = covariateList$DTN_road$wk20,
                             "dtn_road21" = covariateList$DTN_road$wk21, "dtn_road22" = covariateList$DTN_road$wk22, 
                             "dtn_road23" = covariateList$DTN_road$wk23, "dtn_road24" = covariateList$DTN_road$wk24,
                             "dtn_road25" = covariateList$DTN_road$wk25, "dtn_road26" = covariateList$DTN_road$wk26, 
                             "dtn_road27" = covariateList$DTN_road$wk27, "dtn_road28" = covariateList$DTN_road$wk28,
                             "dtn_road29" = covariateList$DTN_road$wk29, "dtn_road30" = covariateList$DTN_road$wk30, 
                             "dtn_road31" = covariateList$DTN_road$wk31, "dtn_road32" = covariateList$DTN_road$wk32,
                             "dtn_road33" = covariateList$DTN_road$wk33, "dtn_road34" = covariateList$DTN_road$wk34, 
                             "dtn_road35" = covariateList$DTN_road$wk35, "dtn_road36" = covariateList$DTN_road$wk36,
                             "dtn_road37" = covariateList$DTN_road$wk37, "dtn_road38" = covariateList$DTN_road$wk38,
                             "dtn_road39" = covariateList$DTN_road$wk39,
                             
                             "dtn_mpine14" = covariateList$DTN_mpine$wk14,                                              # DTN_mpine
                             "dtn_mpine15" = covariateList$DTN_mpine$wk15, "dtn_mpine16" = covariateList$DTN_mpine$wk16, 
                             "dtn_mpine17" = covariateList$DTN_mpine$wk17, "dtn_mpine18" = covariateList$DTN_mpine$wk18, 
                             "dtn_mpine19" = covariateList$DTN_mpine$wk19, "dtn_mpine20" = covariateList$DTN_mpine$wk20,
                             "dtn_mpine21" = covariateList$DTN_mpine$wk21, "dtn_mpine22" = covariateList$DTN_mpine$wk22, 
                             "dtn_mpine23" = covariateList$DTN_mpine$wk23, "dtn_mpine24" = covariateList$DTN_mpine$wk24,
                             "dtn_mpine25" = covariateList$DTN_mpine$wk25, "dtn_mpine26" = covariateList$DTN_mpine$wk26, 
                             "dtn_mpine27" = covariateList$DTN_mpine$wk27, "dtn_mpine28" = covariateList$DTN_mpine$wk28,
                             "dtn_mpine29" = covariateList$DTN_mpine$wk29, "dtn_mpine30" = covariateList$DTN_mpine$wk30, 
                             "dtn_mpine31" = covariateList$DTN_mpine$wk31, "dtn_mpine32" = covariateList$DTN_mpine$wk32,
                             "dtn_mpine33" = covariateList$DTN_mpine$wk33, "dtn_mpine34" = covariateList$DTN_mpine$wk34, 
                             "dtn_mpine35" = covariateList$DTN_mpine$wk35, "dtn_mpine36" = covariateList$DTN_mpine$wk36,
                             "dtn_mpine37" = covariateList$DTN_mpine$wk37, "dtn_mpine38" = covariateList$DTN_mpine$wk38,
                             "dtn_mpine39" = covariateList$DTN_mpine$wk39,
                             
                             "dtn_grassy14" = covariateList$DTN_grassy$wk14,                                                # DTN_grassy
                             "dtn_grassy15" = covariateList$DTN_grassy$wk15, "dtn_grassy16" = covariateList$DTN_grassy$wk16, 
                             "dtn_grassy17" = covariateList$DTN_grassy$wk17, "dtn_grassy18" = covariateList$DTN_grassy$wk18, 
                             "dtn_grassy19" = covariateList$DTN_grassy$wk19, "dtn_grassy20" = covariateList$DTN_grassy$wk20,
                             "dtn_grassy21" = covariateList$DTN_grassy$wk21, "dtn_grassy22" = covariateList$DTN_grassy$wk22, 
                             "dtn_grassy23" = covariateList$DTN_grassy$wk23, "dtn_grassy24" = covariateList$DTN_grassy$wk24,
                             "dtn_grassy25" = covariateList$DTN_grassy$wk25, "dtn_grassy26" = covariateList$DTN_grassy$wk26, 
                             "dtn_grassy27" = covariateList$DTN_grassy$wk27, "dtn_grassy28" = covariateList$DTN_grassy$wk28,
                             "dtn_grassy29" = covariateList$DTN_grassy$wk29, "dtn_grassy30" = covariateList$DTN_grassy$wk30, 
                             "dtn_grassy31" = covariateList$DTN_grassy$wk31, "dtn_grassy32" = covariateList$DTN_grassy$wk32,
                             "dtn_grassy33" = covariateList$DTN_grassy$wk33, "dtn_grassy34" = covariateList$DTN_grassy$wk34, 
                             "dtn_grassy35" = covariateList$DTN_grassy$wk35, "dtn_grassy36" = covariateList$DTN_grassy$wk36,
                             "dtn_grassy37" = covariateList$DTN_grassy$wk37, "dtn_grassy38" = covariateList$DTN_grassy$wk38,
                             "dtn_grassy39" = covariateList$DTN_grassy$wk39,
                             
                             "dtn_decid14" = covariateList$DTN_decid$wk14,                                              # DTN_decid
                             "dtn_decid15" = covariateList$DTN_decid$wk15, "dtn_decid16" = covariateList$DTN_decid$wk16, 
                             "dtn_decid17" = covariateList$DTN_decid$wk17, "dtn_decid18" = covariateList$DTN_decid$wk18, 
                             "dtn_decid19" = covariateList$DTN_decid$wk19, "dtn_decid20" = covariateList$DTN_decid$wk20,
                             "dtn_decid21" = covariateList$DTN_decid$wk21, "dtn_decid22" = covariateList$DTN_decid$wk22, 
                             "dtn_decid23" = covariateList$DTN_decid$wk23, "dtn_decid24" = covariateList$DTN_decid$wk24,
                             "dtn_decid25" = covariateList$DTN_decid$wk25, "dtn_decid26" = covariateList$DTN_decid$wk26, 
                             "dtn_decid27" = covariateList$DTN_decid$wk27, "dtn_decid28" = covariateList$DTN_decid$wk28,
                             "dtn_decid29" = covariateList$DTN_decid$wk29, "dtn_decid30" = covariateList$DTN_decid$wk30, 
                             "dtn_decid31" = covariateList$DTN_decid$wk31, "dtn_decid32" = covariateList$DTN_decid$wk32,
                             "dtn_decid33" = covariateList$DTN_decid$wk33, "dtn_decid34" = covariateList$DTN_decid$wk34, 
                             "dtn_decid35" = covariateList$DTN_decid$wk35, "dtn_decid36" = covariateList$DTN_decid$wk36,
                             "dtn_decid37" = covariateList$DTN_decid$wk37, "dtn_decid38" = covariateList$DTN_decid$wk38,
                             "dtn_decid39" = covariateList$DTN_decid$wk39,
                             
                             "dtn_bf14" = covariateList$DTN_bf$wk14,                                        # DTN_bf
                             "dtn_bf15" = covariateList$DTN_bf$wk15, "dtn_bf16" = covariateList$DTN_bf$wk16, 
                             "dtn_bf17" = covariateList$DTN_bf$wk17, "dtn_bf18" = covariateList$DTN_bf$wk18, 
                             "dtn_bf19" = covariateList$DTN_bf$wk19, "dtn_bf20" = covariateList$DTN_bf$wk20,
                             "dtn_bf21" = covariateList$DTN_bf$wk21, "dtn_bf22" = covariateList$DTN_bf$wk22, 
                             "dtn_bf23" = covariateList$DTN_bf$wk23, "dtn_bf24" = covariateList$DTN_bf$wk24,
                             "dtn_bf25" = covariateList$DTN_bf$wk25, "dtn_bf26" = covariateList$DTN_bf$wk26, 
                             "dtn_bf27" = covariateList$DTN_bf$wk27, "dtn_bf28" = covariateList$DTN_bf$wk28,
                             "dtn_bf29" = covariateList$DTN_bf$wk29, "dtn_bf30" = covariateList$DTN_bf$wk30, 
                             "dtn_bf31" = covariateList$DTN_bf$wk31, "dtn_bf32" = covariateList$DTN_bf$wk32,
                             "dtn_bf33" = covariateList$DTN_bf$wk33, "dtn_bf34" = covariateList$DTN_bf$wk34, 
                             "dtn_bf35" = covariateList$DTN_bf$wk35, "dtn_bf36" = covariateList$DTN_bf$wk36,
                             "dtn_bf37" = covariateList$DTN_bf$wk37, "dtn_bf38" = covariateList$DTN_bf$wk38,
                             "dtn_bf39" = covariateList$DTN_bf$wk39,
                             
                             "dtn_water14" = covariateList$DTN_water$wk14,                                             # dtn_water
                             "dtn_water15" = covariateList$DTN_water$wk15, "dtn_water16" = covariateList$DTN_water$wk16, 
                             "dtn_water17" = covariateList$DTN_water$wk17, "dtn_water18" = covariateList$DTN_water$wk18, 
                             "dtn_water19" = covariateList$DTN_water$wk19, "dtn_water20" = covariateList$DTN_water$wk20,
                             "dtn_water21" = covariateList$DTN_water$wk21, "dtn_water22" = covariateList$DTN_water$wk22, 
                             "dtn_water23" = covariateList$DTN_water$wk23, "dtn_water24" = covariateList$DTN_water$wk24,
                             "dtn_water25" = covariateList$DTN_water$wk25, "dtn_water26" = covariateList$DTN_water$wk26, 
                             "dtn_water27" = covariateList$DTN_water$wk27, "dtn_water28" = covariateList$DTN_water$wk28,
                             "dtn_water29" = covariateList$DTN_water$wk29, "dtn_water30" = covariateList$DTN_water$wk30, 
                             "dtn_water31" = covariateList$DTN_water$wk31, "dtn_water32" = covariateList$DTN_water$wk32,
                             "dtn_water33" = covariateList$DTN_water$wk33, "dtn_water34" = covariateList$DTN_water$wk34, 
                             "dtn_water35" = covariateList$DTN_water$wk35, "dtn_water36" = covariateList$DTN_water$wk36,
                             "dtn_water37" = covariateList$DTN_water$wk37, "dtn_water38" = covariateList$DTN_water$wk38,
                             "dtn_water39" = covariateList$DTN_water$wk39,
                             
                             "perc_burn14" = covariateList$perc_burn$wk14,                                             # perc_burn
                             "perc_burn15" = covariateList$perc_burn$wk15, "perc_burn16" = covariateList$perc_burn$wk16, 
                             "perc_burn17" = covariateList$perc_burn$wk17, "perc_burn18" = covariateList$perc_burn$wk18, 
                             "perc_burn19" = covariateList$perc_burn$wk19, "perc_burn20" = covariateList$perc_burn$wk20,
                             "perc_burn21" = covariateList$perc_burn$wk21, "perc_burn22" = covariateList$perc_burn$wk22, 
                             "perc_burn23" = covariateList$perc_burn$wk23, "perc_burn24" = covariateList$perc_burn$wk24,
                             "perc_burn25" = covariateList$perc_burn$wk25, "perc_burn26" = covariateList$perc_burn$wk26, 
                             "perc_burn27" = covariateList$perc_burn$wk27, "perc_burn28" = covariateList$perc_burn$wk28,
                             "perc_burn29" = covariateList$perc_burn$wk29, "perc_burn30" = covariateList$perc_burn$wk30, 
                             "perc_burn31" = covariateList$perc_burn$wk31, "perc_burn32" = covariateList$perc_burn$wk32,
                             "perc_burn33" = covariateList$perc_burn$wk33, "perc_burn34" = covariateList$perc_burn$wk34, 
                             "perc_burn35" = covariateList$perc_burn$wk35, "perc_burn36" = covariateList$perc_burn$wk36,
                             "perc_burn37" = covariateList$perc_burn$wk37, "perc_burn38" = covariateList$perc_burn$wk38,
                             "perc_burn39" = covariateList$perc_burn$wk39,
                             
                             "bush4_14" = covariateList$bush4$wk14,                                       # bush4
                             "bush4_15" = covariateList$bush4$wk15, "bush4_16" = covariateList$bush4$wk16, 
                             "bush4_17" = covariateList$bush4$wk17, "bush4_18" = covariateList$bush4$wk18, 
                             "bush4_19" = covariateList$bush4$wk19, "bush4_20" = covariateList$bush4$wk20,
                             "bush4_21" = covariateList$bush4$wk21, "bush4_22" = covariateList$bush4$wk22, 
                             "bush4_23" = covariateList$bush4$wk23, "bush4_24" = covariateList$bush4$wk24,
                             "bush4_25" = covariateList$bush4$wk25, "bush4_26" = covariateList$bush4$wk26, 
                             "bush4_27" = covariateList$bush4$wk27, "bush4_28" = covariateList$bush4$wk28,
                             "bush4_29" = covariateList$bush4$wk29, "bush4_30" = covariateList$bush4$wk30, 
                             "bush4_31" = covariateList$bush4$wk31, "bush4_32" = covariateList$bush4$wk32,
                             "bush4_33" = covariateList$bush4$wk33, "bush4_34" = covariateList$bush4$wk34, 
                             "bush4_35" = covariateList$bush4$wk35, "bush4_36" = covariateList$bush4$wk36,
                             "bush4_37" = covariateList$bush4$wk37, "bush4_38" = covariateList$bush4$wk38,
                             "bush4_39" = covariateList$bush4$wk39,
                             
                             "blower14" = covariateList$blower$wk14,                                       # blower
                             "blower15" = covariateList$blower$wk15, "blower16" = covariateList$blower$wk16, 
                             "blower17" = covariateList$blower$wk17, "blower18" = covariateList$blower$wk18, 
                             "blower19" = covariateList$blower$wk19, "blower20" = covariateList$blower$wk20,
                             "blower21" = covariateList$blower$wk21, "blower22" = covariateList$blower$wk22, 
                             "blower23" = covariateList$blower$wk23, "blower24" = covariateList$blower$wk24,
                             "blower25" = covariateList$blower$wk25, "blower26" = covariateList$blower$wk26, 
                             "blower27" = covariateList$blower$wk27, "blower28" = covariateList$blower$wk28,
                             "blower29" = covariateList$blower$wk29, "blower30" = covariateList$blower$wk30, 
                             "blower31" = covariateList$blower$wk31, "blower32" = covariateList$blower$wk32,
                             "blower33" = covariateList$blower$wk33, "blower34" = covariateList$blower$wk34, 
                             "blower35" = covariateList$blower$wk35, "blower36" = covariateList$blower$wk36,
                             "blower37" = covariateList$blower$wk37, "blower38" = covariateList$blower$wk38,
                             "blower39" = covariateList$blower$wk39,
                             
                             "burn_stat14" = covariateList$burn_stat$wk14,                                              # burn_stat
                             "burn_stat15" = covariateList$burn_stat$wk15, "burn_stat16" = covariateList$burn_stat$wk16, 
                             "burn_stat17" = covariateList$burn_stat$wk17, "burn_stat18" = covariateList$burn_stat$wk18, 
                             "burn_stat19" = covariateList$burn_stat$wk19, "burn_stat20" = covariateList$burn_stat$wk20,
                             "burn_stat21" = covariateList$burn_stat$wk21, "burn_stat22" = covariateList$burn_stat$wk22, 
                             "burn_stat23" = covariateList$burn_stat$wk23, "burn_stat24" = covariateList$burn_stat$wk24,
                             "burn_stat25" = covariateList$burn_stat$wk25, "burn_stat26" = covariateList$burn_stat$wk26, 
                             "burn_stat27" = covariateList$burn_stat$wk27, "burn_stat28" = covariateList$burn_stat$wk28,
                             "burn_stat29" = covariateList$burn_stat$wk29, "burn_stat30" = covariateList$burn_stat$wk30, 
                             "burn_stat31" = covariateList$burn_stat$wk31, "burn_stat32" = covariateList$burn_stat$wk32,
                             "burn_stat33" = covariateList$burn_stat$wk33, "burn_stat34" = covariateList$burn_stat$wk34, 
                             "burn_stat35" = covariateList$burn_stat$wk35, "burn_stat36" = covariateList$burn_stat$wk36,
                             "burn_stat37" = covariateList$burn_stat$wk37, "burn_stat38" = covariateList$burn_stat$wk38,
                             "burn_stat39" = covariateList$burn_stat$wk39)
  
  BigDF <- rbind(BigDF, Bird_i_BigDF)
  print(paste0("bird number ", i, " of ", length(unique(inp_df2$birdID)), " is done"))
  }


#########################################################################
######################################################################### 
######################################################################### End of bohemoth for() loops
######################################################################### 
#########################################################################

# add encounter history with covariates
pre_inp <- data.frame("birdid" = inp_df2$birdID, "eh" = eh$eh, inp_df2[,28:44], BigDF[,2:417]) # adds bird id, encounter history, average covariates per individual, time varying covariates
# View(pre_inp)

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
                                         
                                         pre_inp[,20], " ", # NDVI time varying 1-26
                                         pre_inp[,21], " ", # NDVI time varying 1-26
                                         pre_inp[,22], " ", # NDVI time varying 1-26
                                         pre_inp[,23], " ", # col 20:45
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
                                         
                                         pre_inp[,46], " ", # percent mpine: TVC (time varying cov)
                                         pre_inp[,47], " ", # col 46:71
                                         pre_inp[,48], " ", # 
                                         pre_inp[,49], " ",
                                         pre_inp[,50], " ",
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
                                         
                                         pre_inp[,72], " ", # percent grassy: TVC (time varying cov)
                                         pre_inp[,73], " ", # col 72:97
                                         pre_inp[,74], " ",  
                                         pre_inp[,75], " ",
                                         pre_inp[,76], " ",
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
                                      
                                         pre_inp[,98], " ", # percent decid: TVC (time varying cov)
                                         pre_inp[,99], " ", # col 98:123
                                         pre_inp[,100], " ", # 
                                         pre_inp[,101], " ",
                                         pre_inp[,102], " ",
                                         pre_inp[,103], " ",
                                         pre_inp[,104], " ",
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
                                         
                                         pre_inp[,124], " ", # percent bf: TVC (time varying cov)
                                         pre_inp[,125], " ", # col 124:149
                                         pre_inp[,126], " ", # 
                                         pre_inp[,127], " ",
                                         pre_inp[,128], " ",
                                         pre_inp[,129], " ",
                                         pre_inp[,130], " ",
                                         pre_inp[,131], " ",
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
                                         
                                         pre_inp[,150], " ", # percent water: TVC (time varying cov)
                                         pre_inp[,151], " ", # col 150:175
                                         pre_inp[,152], " ", # 
                                         pre_inp[,153], " ",
                                         pre_inp[,154], " ",
                                         pre_inp[,155], " ",
                                         pre_inp[,156], " ",
                                         pre_inp[,157], " ",
                                         pre_inp[,158], " ",
                                         pre_inp[,159], " ",
                                         pre_inp[,160], " ",
                                         pre_inp[,161], " ",
                                         pre_inp[,162], " ",
                                         pre_inp[,163], " ",
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
                                         
                                         pre_inp[,176], " ", # DTN ROAD: TVC (time varying cov)
                                         pre_inp[,177], " ", # col 176:201
                                         pre_inp[,178], " ", # 
                                         pre_inp[,179], " ",
                                         pre_inp[,180], " ",
                                         pre_inp[,181], " ",
                                         pre_inp[,182], " ",
                                         pre_inp[,183], " ",
                                         pre_inp[,184], " ",
                                         pre_inp[,185], " ",
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
                                         
                                         pre_inp[,202], " ", # DTN MPINE: TVC (time varying cov)
                                         pre_inp[,203], " ", # col 202:227
                                         pre_inp[,204], " ", # 
                                         pre_inp[,205], " ",
                                         pre_inp[,206], " ",
                                         pre_inp[,207], " ",
                                         pre_inp[,208], " ",
                                         pre_inp[,209], " ",
                                         pre_inp[,210], " ",
                                         pre_inp[,211], " ",
                                         pre_inp[,212], " ",
                                         pre_inp[,213], " ",
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
                                         
                                         pre_inp[,228], " ", # DTN GRASSY: TVC (time varying cov)
                                         pre_inp[,229], " ", # col 228:253
                                         pre_inp[,230], " ", # 
                                         pre_inp[,231], " ",
                                         pre_inp[,232], " ",
                                         pre_inp[,233], " ",
                                         pre_inp[,234], " ",
                                         pre_inp[,235], " ",
                                         pre_inp[,236], " ",
                                         pre_inp[,237], " ",
                                         pre_inp[,238], " ",
                                         pre_inp[,239], " ",
                                         pre_inp[,240], " ",
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
                                         
                                         pre_inp[,254], " ", # DTN DECID: TVC (time varying cov)
                                         pre_inp[,255], " ", # col 254:279
                                         pre_inp[,256], " ", # 
                                         pre_inp[,257], " ",
                                         pre_inp[,258], " ",
                                         pre_inp[,259], " ",
                                         pre_inp[,260], " ",
                                         pre_inp[,261], " ",
                                         pre_inp[,262], " ",
                                         pre_inp[,263], " ",
                                         pre_inp[,264], " ",
                                         pre_inp[,265], " ",
                                         pre_inp[,266], " ",
                                         pre_inp[,267], " ",
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
                                         
                                         pre_inp[,280], " ", # DTN MPINE: TVC (time varying cov)
                                         pre_inp[,281], " ", # col 280:305
                                         pre_inp[,282], " ", # 
                                         pre_inp[,283], " ",
                                         pre_inp[,284], " ",
                                         pre_inp[,285], " ",
                                         pre_inp[,286], " ",
                                         pre_inp[,287], " ",
                                         pre_inp[,288], " ",
                                         pre_inp[,289], " ",
                                         pre_inp[,290], " ",
                                         pre_inp[,291], " ",
                                         pre_inp[,292], " ",
                                         pre_inp[,293], " ",
                                         pre_inp[,294], " ",
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
                                         
                                         pre_inp[,306], " ", # DTN WATER: TVC (time varying cov)
                                         pre_inp[,307], " ", # col 306:331
                                         pre_inp[,308], " ", # 
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
                                         pre_inp[,319], " ",
                                         pre_inp[,320], " ",
                                         pre_inp[,321], " ",
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
                                         
                                         
                                         pre_inp[,332], " ", # PERC BURN: TVC (time varying cov)
                                         pre_inp[,333], " ", # col 332:357
                                         pre_inp[,334], " ", # 
                                         pre_inp[,335], " ",
                                         pre_inp[,336], " ",
                                         pre_inp[,337], " ",
                                         pre_inp[,338], " ",
                                         pre_inp[,339], " ",
                                         pre_inp[,340], " ",
                                         pre_inp[,341], " ",
                                         pre_inp[,342], " ",
                                         pre_inp[,343], " ",
                                         pre_inp[,344], " ",
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
                                         
                                         pre_inp[,358], " ", # BUSH TREATMENT BINARY: TVC (time varying cov)
                                         pre_inp[,359], " ", # col 358:383
                                         pre_inp[,360], " ", # 
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
                                         pre_inp[,371], " ",
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
                                      
                                         pre_inp[,384], " ", # BLOWER: TVC (time varying cov)
                                         pre_inp[,385], " ", # col 384:409
                                         pre_inp[,386], " ", # 
                                         pre_inp[,387], " ",
                                         pre_inp[,388], " ",
                                         pre_inp[,389], " ",
                                         pre_inp[,390], " ",
                                         pre_inp[,391], " ",
                                         pre_inp[,392], " ",
                                         pre_inp[,393], " ",
                                         pre_inp[,394], " ",
                                         pre_inp[,395], " ",
                                         pre_inp[,396], " ",
                                         pre_inp[,397], " ",
                                         pre_inp[,398], " ",
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
                                         
                                         pre_inp[,410], " ", # BURN STAT BINARY : TVC (time varying cov)
                                         pre_inp[,411], " ", # column 410:435
                                         pre_inp[,412], " ", # 
                                         pre_inp[,413], " ",
                                         pre_inp[,414], " ",
                                         pre_inp[,415], " ",
                                         pre_inp[,416], " ",
                                         pre_inp[,417], " ",
                                         pre_inp[,418], " ",
                                         pre_inp[,419], " ",
                                         pre_inp[,420], " ", #BLAZE IT 
                                         pre_inp[,421], " ",
                                         pre_inp[,422], " ",
                                         pre_inp[,423], " ",
                                         pre_inp[,424], " ",
                                         pre_inp[,425], " ",
                                         pre_inp[,426], " ",
                                         pre_inp[,427], " ",
                                         pre_inp[,428], " ",
                                         pre_inp[,429], " ",
                                         pre_inp[,430], " ",
                                         pre_inp[,431], " ",
                                         pre_inp[,432], " ",
                                         pre_inp[,433], " ",
                                         pre_inp[,434], " ",
                                         pre_inp[,435], ";"
                                         ))
View(new_inp)



# export
write.csv(new_inp, "E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/Survival/knownfate 3.0_timevarying_16Oct23", row.names = FALSE)








