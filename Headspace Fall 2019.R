# Brynn's notes:
#To run- 
#  install.packages("tidyverse")
#  install.packages("lubridate")
#  install.packages("readxl")

# Set working directory
# Only have XL file (NOT CSV) in the directory to run

## headspace_R_code.R 
# Created by Ian J. Kahrilas (7.25.2018)
# Last edited 10.4.2019
# 
# Takes headspace data pulls (.csv format) and outputs into user stats format including total sessions, 
# last 7 days, total minutes, run streak, and totals by headspace pack
# 
# Files required to run this script are the latest headspace data pull (in this case, Completed_Sessions_-_SCILUC3M_2018-07-20T1500.csv)
# and participant tracking log, or any .csv file, that contains user voucher codes and participant names
# -------------------------------------------------------------------------------------------------------

##First, set your working directory to where the .csv files are located. Go to Session -> Set Working Directory -> Choose Directory
##to run entire script
##if this is your first time using R, you must run the following lines in the console without the number signs (bottom window):
#install.packages("tidyverse")
#install.packages("lubridate")
#load in tidyverse, as this is used throughout for reading in files and data manipulation
library(tidyverse)
#load in readxl package to import tracking spreadsheet
library(readxl)

#read in files
files <- list.files()
sessions <- read_csv(str_subset(files, "Completed_Sessions"))
tracking <- read_excel(str_subset(files, "Tracking"))

#eliminate missing rows in tracking log
tracking <- tracking %>% filter(!is.na(...1))

#change name of access code column in tracking log so that datasets can be merged
names(tracking) <- str_replace(names(tracking), "HS Free Access Code", str_subset(names(sessions), "Voucher"))

#merge dataframes so that names correctly line up with voucher codes
hs <- left_join(sessions, tracking, by = str_subset(names(sessions), "Voucher"))

#rename variables so that they are easier to work with and create smaller
#dataframe with only variables of interest
hs_int <- hs %>%
  rename(user_id = "Subscriptions Hs User ID",
         time = "Sessions Session Time",
         pack_name = "Sessions Pack Name",
         session_name = "Sessions Session Name",
         duration = "Sessions Duration",
         platform = "Sessions Platform",
         voucher = "Subscriptions Voucher Code",
         name = "...1",
         hs_id = "HS ID#",
         rand_group = "Randomization Group",
         cohort = "Cohort #") %>%
  select(user_id:rand_group, cohort) %>% 
  filter(rand_group == "PSA", 
         !is.na(name), 
         !is.na(time),
         duration < 30)

#convert time variable from GMT to CST using lubridate package, which contains
#helpful functions for working with dates and times
library(lubridate)
hs_int$time <- mdy_hm(hs_int$time, tz = "GMT") #label current date/times as GMT
hs_int$time <- with_tz(hs_int$time, tzone = "America/Chicago")

##calculate user stats:
#total sessions
total_sessions <- hs_int %>% 
  group_by(name, hs_id) %>%
  summarize(`Total_sessions` = n())

#extract date of latest pull from the title of the .csv file sent from headspace
date_to_convert <- str_extract(str_subset(files, "Completed_Sessions"), "[0-9]{4}[-][0-9]{2}[-][0-9]{2}") ##this code extracts the date from the name of the .csv data pull
ref_date <- ymd(date_to_convert) #this converts the extracted string to a date type so that it can be worked with later

#last 7 days from date of .csv data pull 
last_7 <- hs_int %>%
  filter(time >= ref_date - days(7)) %>%
  group_by(name, hs_id) %>%
  summarize(`Last 7 days` = n())

#total minutes
total_min <- hs_int %>%
  group_by(name, hs_id) %>%
  summarize(`Total minutes` = sum(duration, na.rm = TRUE))

#total by pack
basics <- c("Basics", "Basics 2")
stress <- c("Stress", "Letting Go of Stress")
anxiety <- "Managing Anxiety" 
depression <- c("Depression", "Handling Sadness")
self_esteem <- "Self-esteem"
acceptance <- c("Acceptance")
core_sessions <- c(basics, stress, anxiety, depression, self_esteem, acceptance)
minis <- hs_int %>%
  filter(!(pack_name %in% core_sessions)) %>% 
  filter(duration <= 3) %>%
  select(pack_name) %>%
  distinct() %>%
  pull()
singles <- hs_int %>% 
  filter(!(pack_name %in% minis)) %>% 
  filter(pack_name == session_name) %>% 
  select(pack_name) %>%
  distinct() %>%
  pull()
# compile all pack names into single character vector so that an "other" category can be created
all_sessions <- c(basics, stress, anxiety, depression, self_esteem, acceptance, singles, minis)
# if a session does not belong to any of the the other packs, assign it to other_packs
other_packs <- hs_int %>% 
  filter(!(pack_name %in% all_sessions)) %>%
  select(pack_name) %>% 
  distinct() %>% 
  pull()

pack_total <- hs_int %>%
  group_by(name, hs_id) %>%
  summarize(Basics = sum(pack_name %in% basics),
            Stress = sum(pack_name %in% stress),
            Anxiety = sum(pack_name %in% anxiety),
            Depression = sum(pack_name %in% depression),
            `Self-Esteem` = sum(pack_name %in% self_esteem),
            Acceptance = sum(pack_name %in% acceptance),
            `Other Packs` = sum(pack_name %in% other_packs),
            Singles = sum(pack_name %in% singles),
            Minis = sum(pack_name %in% minis)
  )

##Calculating run streak
part_names <- unique(hs_int$name) #character vector containing all participant names in hs_int

#make list with each element corresponding to a participant
#preallocate space and name each element the participant that specific element corresponds to.
#also create a tibble that will act as a container for run streaks
participants <- vector("list", length(part_names))
names(participants) <- part_names
streak_number <- tibble(name = part_names, `Run streak` = NA)

#This for loop calculates the run streaks
for (i in seq_along(part_names)) {
  participants[[i]] <- hs_int %>% 
    filter(name == part_names[i]) %>% 
    arrange(time) %>%
    mutate(hour_diff = difftime(time, lag(time), units = "hours")) %>% #finds time difference in hours between session completion times that are one apart
    select(time, hour_diff) 
  streak <- 1
  for (j in 1:nrow(participants[[i]])) {
    if(participants[[i]]$hour_diff[j] <= 8 | is.na(participants[[i]]$hour_diff[j])) { #if there is less than or equal to 8 hours between sessions, or if is NA,
      streak <- streak                                                                #streak remains the same
    } else {
      if(participants[[i]]$hour_diff[j] > 8 & participants[[i]]$hour_diff[j] <= 24) {  #if there is more than 8 and less or equal to
        streak <- streak + 1                                                          #24 hours between sessions, add 1 to run streak
      } else {
        if(participants[[i]]$hour_diff[j] > 24) { #if there is more than 24 hours in between sessions, reset streak to 0
          streak <- 0
        }
      }
    }
  }
  if (difftime(ref_date, last(participants[[i]]$time), units = "hours") > 24) { #if the time difference between data pull date (obtained from the title of the .csv file) 
    #and participant's latest session is more than 24 hours, reset to 0. Otherwise, participants
    streak <- 0                                                                 #who haven't had a session in more than 24 hours may still have a run streak.
  }                                                                            
  streak_number$`Run streak`[i] <- streak #assign calculated streak number to participants streak tibble
}


#merge all the tibbles togeter
user_stats <- full_join(total_sessions, last_7, by = c("name", "hs_id")) %>%
  full_join(., total_min, by = c("name", "hs_id")) %>% 
  full_join(., streak_number, by = "name") %>%
  full_join(., pack_total, by = c("name", "hs_id"))

#replace missing values in last 7 days column with zeroes
user_stats$`Last 7 days`[is.na(user_stats$`Last 7 days`)] <- 0

#remove hs_id variable for purpose of output
user_stats_output <- select(user_stats, "name", "Total_sessions":"Minis")

#get rid of column name for participants' names - it must be removed for output
names(user_stats_output)[1] <- ""

#add in "min" to total/average minutes variable
user_stats_output$`Total minutes` <- paste(user_stats_output$`Total minutes`, "min", sep = " ")

#transpose data and remove hs_id variable for purpose of output
user_stats_transposed <- t(user_stats_output)

#create name for output that contains today's date
name_for_output <- paste("individual_user_stats_", today(), ".csv", sep = "")

#output csv file
write.csv(user_stats_transposed, file = name_for_output)

#####################################################
###############GROUP STATS##########################
###################################################

day_group <- hs_int %>%
  mutate(time_diff = difftime(ref_date, time, units = "hours")) %>%
  filter(time_diff <= 24)

week_group <- hs_int %>%
  mutate(time_diff = difftime(ref_date, time, units = "hours")) %>%
  filter(time_diff <= (24* 7))

##group stat calculations
#24 hours
day_stats <- day_group %>%
  summarize(`Group Progress:` = "24 hours",
            `Total minutes` = sum(duration, na.rm = TRUE),
            `Sessions (any)` = n(),
            `Avg duration` = mean(duration, na.rm = TRUE),
            `Sessions by Area:` = " ",
            Basics = sum(pack_name %in% basics),
            Stress = sum(pack_name %in% stress),
            Anxiety = sum(pack_name %in% anxiety),
            Depression = sum(pack_name %in% depression),
            `Self-Esteem` = sum(pack_name %in% self_esteem),
            Acceptance = sum(pack_name %in% acceptance),
            `Other Packs` = sum(pack_name %in% other_packs),
            Singles = sum(pack_name %in% singles),
            Minis = sum(pack_name %in% minis))

week_stats <- week_group %>%
  summarize(`Group Progress:` = "7 days",
            `Total minutes` = sum(duration, na.rm = TRUE),
            `Sessions (any)` = n(),
            `Avg duration` = mean(duration, na.rm = TRUE),
            `Sessions by Area:` = " ",
            Basics = sum(pack_name %in% basics),
            Stress = sum(pack_name %in% stress),
            Anxiety = sum(pack_name %in% anxiety),
            Depression = sum(pack_name %in% depression),
            `Self-Esteem` = sum(pack_name %in% self_esteem),
            Acceptance = sum(pack_name %in% acceptance),
            `Other Packs` = sum(pack_name %in% other_packs),
            Singles = sum(pack_name %in% singles),
            Minis = sum(pack_name %in% minis))

total_stats <- hs_int %>%
  summarize(`Group Progress:` = "Total",
            `Total minutes` = sum(duration, na.rm = TRUE),
            `Sessions (any)` = n(),
            `Avg duration` = mean(duration, na.rm = TRUE),
            `Sessions by Area:` = " ",
            Basics = sum(pack_name %in% basics),
            Stress = sum(pack_name %in% stress),
            Anxiety = sum(pack_name %in% anxiety),
            Depression = sum(pack_name %in% depression),
            `Self-Esteem` = sum(pack_name %in% self_esteem),
            Acceptance = sum(pack_name %in% acceptance),
            `Other Packs` = sum(pack_name %in% other_packs),
            Singles = sum(pack_name %in% singles),
            Minis = sum(pack_name %in% minis))

group_stats_output <- bind_rows(day_stats, week_stats) %>%
  bind_rows(., total_stats)

#round avg duration variable to two decimal points
group_stats_output$`Avg duration` <- round(group_stats_output$`Avg duration`, digits = 2)

#code displays "min" next to avg duration and total min
group_stats_output$`Total minutes` <- paste(group_stats_output$`Total minutes`, "min", sep = " ")
group_stats_output$`Avg duration` <- paste(group_stats_output$`Avg duration`, "min", sep = " ")

#transpose the dataset for output
group_stats_output_transposed <- t(group_stats_output)

#create name for output that contains today's date
name_for_output <- paste("group_user_stats_", today(), ".csv", sep = "")

#output csv file
write.csv(group_stats_output_transposed, file = name_for_output)
