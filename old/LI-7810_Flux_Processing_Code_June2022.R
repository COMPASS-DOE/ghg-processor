##### Methane Flux Processing from Licor 7810 #### 
## Originally written by Genevieve Noyce
## Received on 12 July 2021
## Modified by HSK with assistance from MLV & Michael Lonneman
## Last modified on 30 May 2022 (HSK)

### Steph June 2022 COMPASS chamber fluxes


#########################
#### 1. Daily set-up ####
#########################

rm(list = ls())

# Load necessary packages 
library(data.table); library(tidyverse)
library(readxl); library(lubridate)
library(ggpubr); library(cowplot)
library(plotrix)


# Set year,month and date of files being processed
year='2021'
month='July'
Date='2021-07-21'
julian.date='2021-01-01'



######################################
###### 2. Format Licor raw data ######
######################################

# Create a list of all the Licor data files for the month 
dat_input <- list.files(paste("Data/Methane/", month, "2021/Raw", sep = ""))

# Assign a name to each file name so data can be read in more quickly
#file1 <- dat_input[1] # 7/19, 
#file2 <- dat_input[2] # 7/20
file3 <- dat_input[3] # 7/21
#file4 <- dat_input[4] # 7/22
#file5 <- dat_input[5] # 7/23

# Read in raw data file from Licor, make sure to change file number based on day of week
dat <- read.table(file = paste("Data/Methane/", month, "2021/Raw/", 
                               file3, # change this name! 
                               sep = ""),
  sep = '\t', header = T, skip = 5)

# Remove line with only units
dat <- dat[-1,]

# Keep columns with GHG data and remove columns with auxiliary data
dat <- dat %>% 
  select('DATE','TIME','CO2','CH4','H2O','DIAG')

# Calculate fractional day-of-year
# Combine Date and Time columns
dat$date_time <- paste(dat$DATE,dat$TIME)
# Set as date and time format & convert from EDT to EST by subtracting 1 hour 
dat$date_time <- ymd_hms(dat$date_time, tz = "EST") - 3600
# Add fractional DOY column
dat$fDOY <- as.numeric(julian(dat$date_time, julian.date))

# Remove columns that are no longer necessary 
dat <- dat %>% 
  select(-c(DATE, TIME))

# Save Licor data as a data frame
dat_all <- data.frame(dat)



########################################
#### 3. Format met tower data table ####
########################################

# Read in Met Tower file from SERC
met <- read.csv("Data/MetTower/compiled_met_tower_2021.csv")

# Make sure timestamp is recognized as date + time measured in EST
met$timestamp <- ymd_hms(met$timestamp, tz = "EST")

met <- met %>% 
  # Remove unnecessary columns
  select(-c(rain_accumulation_mm, wind_speed_avg_m_per_s, date, time)) %>% 
  # Add fractional day of year (DOY) column
  mutate(fDOY_met = as.numeric(julian(met$timestamp, julian.date)))

# Save met tower data as a data frame
met_all <- data.frame(met)


################################
#### 4. Format pot metadata ####
################################

# Read in file with plot data
pots_july <- read.csv(paste("Data/Methane/", month, 
                       "2021/MethaneFluxes_July2021_potlevel.csv", sep = ""))

# Modify pot-level metadata 
pots <- pots_july %>% 
  # Create united unique identifier by pot
  unite("pot_id", c(site_frame, row, pos), sep = "_", remove = T) %>% 
  # Remove unsampled pots (July)
  filter(pot_id != "fresh_6_3_4") %>% 
  filter(pot_id != "gcrew_3_4_3") %>% 
  filter(pot_id != "gcrew_7_5_4") %>% 
  # Format start time data 
  mutate(start_time = as.character(sub(".*\\s+", "", start_time)),
         # Format the date column
         date = mdy(date),
         # Turn date and time into a timestamp, converted from EDT to EST to match other datasets 
         timestamp_start = ymd_hm(paste(date, start_time), tz = "EST") - 3600) %>%
  # Format end time data
  mutate(end_time = as.character(sub(".*\\s+", "", end_time)),
         # Turn date and time into a timestamp, converted from EDT to EST 
         timestamp_end = ymd_hm(paste(date, end_time), tz = "EST") - 3600) %>% 
  # Filter to only select the metadata from the sampling Date assigned at the beginning of code 
  filter(date == Date) 

# Pull out date and time data
pot_date_time_start = pots$timestamp_start
# Add 59 seconds to this final time to capture the entire final minute of the flux so there is no missing data 
pot_date_time_end = pots$timestamp_end + 59

# Add fDOY columns for start and endtime to dataframe
pots$fDOY_start = as.numeric(julian(pot_date_time_start, julian.date))  
pots$fDOY_end = as.numeric(julian(pot_date_time_end, julian.date))  

# Remove unnecessary columns
pots <- pots %>% 
  select(-c(date, start_ppb, end_ppb))


###############################################################
#### 5. Merge Licor data, met tower file, and pot metadata ####
###############################################################

# Calculate fractional DOY for met tower data
#met_all$fDOY_met <- as.numeric(julian(met_all$timestamp, julian.date))

# Create an empty list for the loop to store stuff in
list_dat <- vector(mode = "list", length = nrow(pots))
list_met <- vector(mode = "list", length = nrow(pots))

# Loop through the pots and pull out only the data from dat_all that is within the flux times
for (i in 1:nrow(pots)){
  temp <- dat_all %>% filter(fDOY >= pots$fDOY_start[i] & fDOY <= pots$fDOY_end[i])
  list_dat[[i]] <- temp
  temp2 <- met_all %>% filter(fDOY_met >= pots$fDOY_start[i] & fDOY_met <= pots$fDOY_end[i])
  list_met[[i]] <- temp2
  list_met[[i]]$pot_no <- rep(i, nrow(list_met[[i]]))
} 

# Bind all data.frames in the list into a big data.frame
dat_all_flux <- do.call(rbind, list_dat) #bind_rows(list_dat) would also work instead of do.call(rbind, list_dat)

# Bind all data.frames in the list into a big data.frame
met_all_flux <- do.call(rbind, list_met)

# Sort pots by time
pots %>% 
  arrange(timestamp_start) %>% 
  mutate(pot_no = 1:nrow(pots)) -> pots

# Merge met tower data and pot metadata together by pot number 
merge(met_all_flux, pots, by = "pot_no", all.x = TRUE) -> met_pots

# Modify the timestamps so that the time is rounded to the floor time at the minute level
  # for example: 13:14:09 is changed to 13:14:00, same with 13:14:59 is changed to 13:14:00
dat_all_flux <- dat_all_flux %>% 
  mutate(timestamp = floor_date(date_time, unit = "minutes"))

# Merge the 3 datasets together
merge(dat_all_flux, met_pots, by = "timestamp") -> all

# Arrange data frame by pot_no and then time 
all %>% 
  arrange(pot_no, date_time) -> all



#########################################
#### 6. Visually check data clipping ####
#########################################

par(mfrow=c(2,1),mar=c(4,4,1,1))
# Look at what the unclipped data looked like - CH4
with(dat_all, plot(fDOY, CH4, ylim=c(1900,3000)))
# Plot the clipped data - CH4
with(all, plot(fDOY_met, CH4, ylim=c(2000,3200)))

# Look at what the unclipped data looked like - CO2
with(dat_all, plot(fDOY, CO2, ylim=c(300,600)))
# Plot the clipped data - CO2
with(all, plot(fDOY_met, CO2, ylim=c(300,550)))



######################################################
#### 7. Add 'flag' columns and export merged data #### 
######################################################

# Make flag columns
all$flag_CH4 <- "y"
all$flag_CO2 <- "y"

# export merged data as a .csv file 
write.csv(all, file = paste("Data/Methane/", month, 
                            "2021/Working/merged_data_", Date, ".csv", sep = ""), 
          row.names=F)



##########################################
#### 8. Plot individual fluxes for QC ####
##########################################

# Make greenhouse gas data a numeric 
all$CH4 <- as.numeric(all$CH4)
all$CO2 <- as.numeric(all$CO2)

# Extract and make vector of each pot id to use for labeling plots 
pot_labels <- unique(all$pot_id)

for (i in 1:length(pot_labels)) {
  ### CH4 ###
  # Create a linear regression by pot of CH4 over time 
  mod1 <- all %>% filter(pot_no == i) %>%
    lm(CH4 ~ fDOY, data = .)
  # Create label for plot
  label <- paste(pot_labels[i], " (", i, "), CH4", sep = "")
  # Plot data 
  p_CH4 <- all %>% filter(pot_no == i) %>%
    ggplot(aes(x = fDOY, y = CH4)) +
    # Plot raw data 
    geom_point() + 
    # Plot linear regression
    geom_abline(intercept = mod1$coefficients[1], slope = mod1$coefficients[2],
                color = "blue", aes(x = fDOY, y = CH4)) + 
    # Calculate R2 
    stat_regline_equation(label.y = Inf, aes(label = ..rr.label..), color = "red", vjust = "inward") + 
    ggtitle(label = label) + 
    theme_linedraw(base_size = 15)
  ### CO2 ###
  # Create a linear regression by pot of CH4 over time
  mod2 <- all %>% filter(pot_no == i) %>%
    lm(CO2 ~ fDOY, data = .)
  # Create label for plot 
  label <- paste(pot_labels[i], " (", i, "), CO2", sep = "") 
  # Plot data 
  p_CO2 <- all %>% filter(pot_no == i) %>%
    ggplot(aes(x = fDOY, y = CO2)) +
    # Plot raw data 
    geom_point() + 
    # Plot linear regression 
    geom_abline(intercept = mod2$coefficients[1], slope = mod2$coefficients[2],
                color = "blue", aes(x = fDOY, y = CO2)) + 
    # Calculate R2 
    stat_regline_equation(label.y = Inf, aes(label = ..rr.label..), color = "red", vjust = "inward") + 
    ggtitle(label = label) + 
    theme_linedraw(base_size = 15)
  # Print the paired plots for each measurement 
  print(plot_grid(p_CH4, p_CO2))
}

# When opening .csv file in excel, columns I want to keep visible are: 
  # timestamp, CO2, CH4, fDOY, pot_no, flag_CH4, flag_CO2

# Confirm there are no diagnosis codes present in dataset 
unique(all$DIAG) 


# Clean up global environment before moving on to working with cleaned data 
rm(all, dat_all_flux, dat, dat_all, list_dat, list_met, met, met_all, met_pots,
   met_all, met_all_flux, mod1, mod2, p_CH4, p_CO2, pots_july, temp, temp2, 
   pot_date_time_start, pot_date_time_end, i, file1, dat_input)



#######################################################
#### 9. Remove flagged data + separate by gas type ####
#######################################################

## Goal: Read in .csv, remove flagged points, and save CH4 and CO2 data separately 

# Read in flagged datafile
dat_flag <- read.csv(paste("Data/Methane/", month, "2021/Working/merged_data_",
                        Date, "_flagged.csv", sep = ""))
                        

# For CH4:
# Remove rows of data that don't have flag 'y'
dat_CH4 <- dat_flag %>% 
  filter(flag_CH4 == "y") %>% 
  # Select relevant columns of data and reorder remaining columns
  select(timestamp, fDOY, timestamp_start, pot_id, soil_to_pot, air_temp_C,
         CH4, pot_no) 

## For CO2:
# Remove rows of data that don't have flag 'y'
dat_CO2 <- dat_flag %>% 
  filter(flag_CO2 == "y") %>% 
  # Select relevant columns of data and reorder remaining columns
  select(timestamp, fDOY, timestamp_start, pot_id, soil_to_pot, air_temp_C,
         CO2, pot_no)


#################################################
#### 10. Plot individual fluxes for final QC ####
#################################################

# Focus on CH4 data for final QC 

# Make greenhouse gas data a numeric 
dat_CH4$CH4 <- as.numeric(dat_CH4$CH4)

# Extract and make vector of each pot id to use for labeling plots 
pot_labels <- unique(dat_CH4$pot_id)

for (i in 1:length(pot_labels)) {
  ### CH4 ###
  # Create a linear regression by pot of CH4 over time 
  mod1 <- dat_CH4 %>% filter(pot_no == i) %>%
    lm(CH4 ~ fDOY, data = .)
  # Create label for plot
  label <- paste(pot_labels[i], " (", i, "), CH4", sep = "")
  # Plot data 
  p_CH4 <- dat_CH4 %>% filter(pot_no == i) %>%
    ggplot(aes(x = fDOY, y = CH4)) +
    # Plot raw data 
    geom_point() + 
    # Plot linear regression
    geom_abline(intercept = mod1$coefficients[1], slope = mod1$coefficients[2],
                color = "blue", aes(x = fDOY, y = CH4)) + 
    # Calculate R2 
    stat_regline_equation(label.y = Inf, aes(label = ..rr.label..), color = "red", vjust = "inward") + 
    ggtitle(label = label) + 
    theme_linedraw(base_size = 15)
  # Print the plot that was created for each measurement  
  print(p_CH4)
}



######################################################
#### 11. Convert ppb/ppm to moles for CH4 and CO2 ####
######################################################

# CH4 conversions 
dat_CH4 <- dat_CH4 %>% 
  # Use known volume of chamber (in m3) to convert ppb of gas to liters of gas, through this conversion:
  # (parts CH4 per billion parts air * 
  # volume of chamber (pi*r^2*h) [height is chamber height + soil to pot height] in m3 * 
  # 1000 L per m3)
  mutate(CH4_L = (CH4/1000000000) * (pi * 0.05^2 * ((99 + soil_to_pot)/100)) * 1000) %>% 
  # Use ideal gas law to calculate umol of CH4, through this conversion:
  # (atm pressure * L CH4) / (R [in L*atm / degrees K * mol] * degrees K) * 10^6 umol/mol
  mutate(CH4_umol = (1 * CH4_L) / (0.08206 * (air_temp_C + 273)) * 10^6)

# CO2 conversions
dat_CO2 <- dat_CO2 %>% 
  # Use known volume of chamber (in m3) to convert ppb of gas to liters of gas, through this conversion:
  # (parts CO2 per million parts air * 
  # volume of chamber (pi*r^2*h) [height is chamber height + soil to pot height] in m3 * 
  # 1000 L per m3)
  mutate(CO2_L = (CO2/1000000) * (pi * 0.05^2 * ((99 + soil_to_pot)/100)) * 1000) %>% 
  # Use ideal gas law to calculate mmol of CO2, through this conversion:
  # (atm pressure * L CO2) / (R [in L*atm / degrees K * mol] * degrees K) * 10^3 mmol/mol
  mutate(CO2_mmol = (1 * CO2_L) / (0.08206 * (air_temp_C + 273)) * 10^3)


###################################################################
#### 12. Calculate mean and SE for log variables for each flux ####
###################################################################

# Calculate average and SE air + soil temp for CH4 for each measurement 
air_sum_CH4 <- dat_CH4 %>% 
  group_by(pot_id) %>% 
  summarize(air_temp_average = mean(air_temp_C), 
            air_temp_sd = sd(air_temp_C),
            air_temp_n = length(air_temp_C),
            air_temp_se = air_temp_sd / sqrt(air_temp_n)) %>% 
  select(pot_id, air_temp_average, air_temp_se)

# Calculate average and SE air temp for CO2 for each measurement 
air_sum_CO2 <- dat_CO2 %>% 
  group_by(pot_id) %>% 
  summarize(air_temp_average = mean(air_temp_C), 
            air_temp_sd = sd(air_temp_C),
            air_temp_n = length(air_temp_C),
            air_temp_se = air_temp_sd / sqrt(air_temp_n)) %>% 
  select(pot_id, air_temp_average, air_temp_se)



######################################
#### 13. Calculate chamber fluxes ####
######################################

# Create new dataframes to hold final fluxes 
fluxes_CH4 <- data.frame(matrix(NA, nrow = length(unique(dat_CH4$pot_id))))
fluxes_CO2 <- data.frame(matrix(NA, nrow = length(unique(dat_CO2$pot_id))))

# Add named columns
# CH4
fluxes_CH4$pot_id <- pot_labels
fluxes_CH4$flux_CH4 <- 0
fluxes_CH4$R2_CH4 <- 0
fluxes_CH4$p_CH4 <- 0

# CO2
fluxes_CO2$pot_id <- pot_labels
fluxes_CO2$flux_CO2 <- 0
fluxes_CO2$R2_CO2 <- 0
fluxes_CO2$p_CO2 <- 0

# Remove initial empty column
fluxes_CH4 <- fluxes_CH4[,-1]
fluxes_CO2 <- fluxes_CO2[,-1]

## For each start time
for (i in pot_labels) {
  ## CH4 ##
  # Subset data for one chamber measurement
  temp1 = subset(dat_CH4, pot_id == i)
  # Set corresponding row of output table
  j = which(pot_labels == i)
  # Determine if start time has a CH4 flux
  if (nrow(temp1) > 0) {
    # If so:  
    # Calulate flux in umol/day using linear regression
    mod = with(temp1, lm(CH4_umol ~ fDOY))
    # Save flux rate and R2 and p-value of slope in corresponding row of dataframe
    # flux rate, converted from umol/day to umol/m2/day (last unit is the surface area of pot pi*r^2)
    fluxes_CH4$flux_CH4[j]=coef(mod)[2]/(0.05^2*pi)
    # R2 of slope
    fluxes_CH4$R2_CH4[j]=summary(mod)$r.squared
    # p-value of slope
    fluxes_CH4$p_CH4[j]=summary(mod)$coefficients[2,4]
    # If not:
    # Fill rows of table with NA    
  } else {
    fluxes_CH4$flux_CH4[j]=NA
    fluxes_CH4$R2_CH4[j]=NA
    fluxes_CH4$p_CH4[j]=NA
  }
  ## CO2 ##
  # Subset data for one chamber measurement
  temp2 = subset(dat_CO2, pot_id == i)
  # Calulate flux in mol/day using linear regression
  mod2 = with(temp2, lm(CO2_mmol ~ fDOY))
  # Save flux rate and R2 and p-value of slope in corresponding row of dataframe
  # flux rate, converted from mol/day to mol/m2/day
  fluxes_CO2$flux_CO2[j]=coef(mod2)[2]/(0.05^2*pi) #area of chamber base
  # R2 of slope
  fluxes_CO2$R2_CO2[j]=summary(mod2)$r.squared
  # p-value of slope
  fluxes_CO2$p_CO2[j]=summary(mod2)$coefficients[2,4]
}

# Subset dat_CH4 for desired metadata 
dat_CH4_sub <- dat_CH4 %>% 
  group_by(pot_id) %>% 
  summarize(#date = unique(date),
            timestamp_start = unique(timestamp_start),
            fDOY = head(fDOY, 1)) 

# Subset dat_CO2 for desired metadata 
dat_CO2_sub <- dat_CO2 %>% 
  group_by(pot_id) %>% 
  summarize(#date = unique(date),
            timestamp_start = unique(timestamp_start),
            fDOY = head(fDOY, 1)) 

# subset pots for desired metadata 
pots <- pots %>% 
  select(pot_id, gt, spcs, comp, chamber, water_to_pot, soil_temp, salinity)
  

# Merge flux data with plot metadata, keeping desired columns
# CH4
fluxes_CH4 <- full_join(fluxes_CH4, dat_CH4_sub, by = "pot_id")
fluxes_CH4 <- full_join(fluxes_CH4, pots, by = "pot_id")
fluxes_CH4 <- full_join(fluxes_CH4, air_sum_CH4, by = "pot_id")

# CO2
fluxes_CO2 <- full_join(fluxes_CO2, dat_CO2_sub, by = "pot_id")
fluxes_CO2 <- full_join(fluxes_CO2, pots, by = "pot_id")
fluxes_CO2 <- full_join(fluxes_CO2, air_sum_CO2, by = "pot_id")

# Rearrange order of columns
# CH4
fluxes_CH4 <- fluxes_CH4 %>% 
  select(pot_id, gt, spcs, comp, timestamp_start, fDOY, flux_CH4, 
         R2_CH4, p_CH4, chamber, air_temp_average, air_temp_se, 
         salinity, soil_temp, water_to_pot)

# CO2
fluxes_CO2 <- fluxes_CO2 %>% 
  select(pot_id, gt, spcs, comp, timestamp_start, fDOY, flux_CO2, 
         R2_CO2, p_CO2, chamber, air_temp_average, air_temp_se, 
         salinity, soil_temp, water_to_pot)


## Set units for each column
# CH4
units_CH4 <- c("", "", "", "", "", "", "umol/m2/day", "", "", "",
               "°C", "", "ppt", "°C", "cm")
# CO2
units_CO2 <- c("", "", "", "", "", "", "mmol/m2/day", "", "", "",
               "°C", "", "ppt", "°C", "cm")

# Add row with units
options(warn=-1) # suppress warnings about NA not being a factor level
# CH4
fluxes_CH4 <- rbind(units_CH4, fluxes_CH4)
# CO2
fluxes_CO2 <- rbind(units_CO2, fluxes_CO2)



#####################################
#### 14. Export final data files ####
#####################################

# Before writing file, convert timestamp to a character, because write.csv() can
# sometimes cause problems re-interpreting DateTime data when it's reimported 
fluxes_CH4$timestamp_start <- as.character(fluxes_CH4$timestamp_start)
fluxes_CO2$timestamp_start <- as.character(fluxes_CO2$timestamp_start)

# Export fluxes as .csv file
# CH4
write.csv(fluxes_CH4, file = paste("Data/Methane/", month, 
                                   "2021/Clean/final_CH4_fluxes_", Date, ".csv", sep=""), 
          row.names = F)
# CO2
write.csv(fluxes_CO2, file = paste("Data/Methane/", month, 
                                   "2021/Clean/final_CO2_fluxes_", Date, ".csv", sep=""), 
          row.names = F)

