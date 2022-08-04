#-----------------------------------------------------------------------------------------------------------------------

# Title: Camera Model Comparisons
# Authors: Dave J Huggard, Marcus Becker
# Date: March 2022

#-----------------------------------------------------------------------------------------------------------------------

# Google Drive
root <- "G:/Shared drives/ABMI Camera Mammals/"

# Attach packages
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(lubridate)

# Native species tags in WildTrax
load(paste0(root, "data/lookup/wt_native_sp.RData"))

# Gap groups
df_gap_groups <- read_csv(paste0(root, "data/lookup/species-gap-groups.csv"))
# 1. Probabilistic gaps
df_leave_prob_pred <- read_csv(paste0(root, "data/processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv"))
# 2. Time between photos
df_tbp <- read_csv(paste0(root, "data/processed/time-btwn-images/abmi-cmu_-all-years_tbp_2021-06-25.csv"))

# Authenticate into WildTrax to retrieve data
library(keyring)
library(wildRtrax)

# Set environment variables
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

# Authenticate
wt_auth()

# Issue - species tags. 'Deer', 'Wolves, Coyotes, and Allies'. These might be resolved. Let's wait on coding it in.

#-----------------------------------------------------------------------------------------------------------------------

# List model comparison projects
projects <- wt_get_download_summary(sensor_id = "CAM") %>%
  filter(str_detect(project, "Comparison"),
         # Don't have 2021 yet.
         year < 2021) %>%
  select(project_id) %>%
  pull() %>%
  unlist()

# Obtain HP2X data
df_hp2x <- map_df(.x = projects,
             .f = ~ wt_download_report(
               project_id = .x,
               sensor_id = "CAM",
               cols_def = FALSE,
               weather_cols = FALSE
             )) %>%
  filter(camera_make == "RECONYX")

# Now we need data from ABMI EH 2019

# Locations with paired cameras
eh2019_loc <- df_hp2x %>%
  filter(project == "ABMI Camera Model Comparison 2019") %>%
  select(location) %>%
  distinct() %>%
  pull()

# EH 2019 project ID
project_eh2019 <- wt_get_download_summary(sensor_id = "CAM") %>%
  filter(str_detect(project, "Ecosystem Health"),
         year == "2019") %>%
  select(project_id) %>%
  pull() %>%
  unlist()

# EH 2019 PC900 data
eh2019_pc900 <- wt_download_report(project_id = project_eh2019,
                               sensor_id = "CAM",
                               cols_def = FALSE,
                               weather_cols = FALSE) %>%
  filter(location %in% eh2019_loc)

#-----------------------------------------------------------------------------------------------------------------------

# Clean and combine.

df_all <- df_hp2x %>%
  mutate(location = str_remove(location, "71-")) %>%
  bind_rows(eh2019_pc900) %>%
  select(project, location, date_detected, field_of_view, common_name, number_individuals, camera_model)

# 2020
df_range_20 <- df_all %>%
  filter(project == "ABMI Camera Model Comparison 2020") %>%
  group_by(location, camera_model) %>%
  summarise(start_date_time = ymd_hms(min(date_detected)),
            end_date_time = ymd_hms(max(date_detected))) %>%
  ungroup() # These seem good.

#-----------------------------------------------------------------------------------------------------------------------

# Let's start with 2020. Seems cleaner. No issues with time range, out of field of view photos, etc.

df_series_20 <- df_all %>%
  filter(project == "ABMI Camera Model Comparison 2020") %>%
  # Let's append camera model to location
  mutate(location_cam = paste0(location, "_", camera_model),
         date_detected = ymd_hms(date_detected)) %>%
  # Amalgamate tags of same species in same image
  mutate(number_individuals = as.numeric(ifelse(number_individuals == "VNA", 1, number_individuals))) %>%
  group_by(location_cam, date_detected, common_name) %>%
  mutate(number_individuals = sum(number_individuals)) %>%
  distinct(location_cam, date_detected, common_name, number_individuals, .keep_all = TRUE) %>%
  ungroup() %>%
  # Just native species.
  filter(common_name %in% native_sp) %>%
  # Order observations
  arrange(location_cam, date_detected, common_name) %>%
  # Assess series
  mutate(series_num = 0,
         date_detected_lag = lag(date_detected),
         diff_time = as.numeric(date_detected - date_detected_lag),
         common_name_lag = lag(common_name),
         diff_sp = ifelse(common_name != common_name_lag, TRUE, FALSE),
         location_cam_lag = lag(location_cam),
         diff_loc = ifelse(location_cam != location_cam_lag, TRUE, FALSE),
         gap_check = ifelse(diff_loc == FALSE & diff_sp == FALSE & (diff_time <= 120 & diff_time >= 20), 1, 0),
         diff_series = ifelse(diff_loc == TRUE | diff_sp == TRUE | diff_time > 120, 1, 0),
         series_num = c(0, cumsum(diff_series[-1]))) %>%
  left_join(df_gap_groups, by = "common_name") %>%
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time")) %>%
  mutate(pred = replace_na(pred, 1),
         diff_time_adj = round(ifelse(gap_check == 1, diff_time * (1 - pred), diff_time), digits = 2))

# Calculate total time in front of the camera, by series

# Start with series that have >1 image
df_tts_multiple <- df_series_20 %>%
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) %>%
  filter(series_num_previous == series_num) %>%
  group_by(series_num) %>%
  summarise(n_images = n() + 1,
            total_time = sum(diff_time_adj))

# Next, single-image series'
df_tts_single <- df_series_20 %>%
  group_by(series_num) %>%
  summarise(n_images = n()) %>%
  # Keep only series with 1 image
  filter(n_images == 1) %>%
  mutate(total_time = 0)

# Bind together
df_tts_all <- df_tts_multiple %>%
  bind_rows(df_tts_single) %>%
  arrange(series_num)

# Add time between photos, accounting for the average number of animals in each photo of a series
df_tts_final <- df_series_20 %>%
  mutate(number_individuals = as.numeric(ifelse(number_individuals == "VNA", 1, number_individuals))) %>%
  # Join in average tbp, by species; note that this is slightly different from results calculated above (incl. non-ABMI)
  left_join(df_tbp, by = "common_name") %>%
  select(series_num, common_name, number_individuals, tbp) %>%
  group_by(series_num) %>%
  # Number of individuals in a series
  mutate(avg_individuals = mean(number_individuals)) %>%
  ungroup() %>%
  select(-number_individuals) %>%
  distinct() %>%
  left_join(df_tts_all, by = "series_num") %>%
  mutate(series_total_time = (total_time + tbp) * avg_individuals) %>%
  select(series_num, common_name, n_images, series_total_time)

# Calculate total time in front of camera, by deployment, year, and species (tt = total time)

# Seasonal start/end dates (julian day):
summer.start.j <- 106 # April 16
summer.end.j <- 288 # October 15

df_tt <- df_series_20 %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final, by = c("series_num", "common_name")) %>%
  select(series_num, n_images, location_cam, date_detected, common_name, series_total_time) %>%
  ungroup() %>%
  mutate(julian = as.numeric(format(date_detected, "%j")),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "summer", "winter")) %>%
  mutate_at(c("location_cam", "common_name", "season"), factor) %>%
  group_by(location_cam, common_name, season, .drop = FALSE) %>%
  summarise(total_duration = sum(series_total_time),
            total_images = sum(n_images),
            total_series = n()) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character)

write_csv(df_tt, "data/processed/summary_of_series_2020.csv")

#---------------------------------------------------------------------------------------------------

# Now, let's try to figure out the 2019 stuff. A little messier, a little less pleasant. Sigh.
# Note: I like what Dave did - have the series summaries already done, that he could read in.

# Bring 2019 data together into single dataframe.

df_2019 <- df_hp2x %>%
  filter(str_detect(project, "2019")) %>%
  bind_rows(eh2019_pc900) %>%
  mutate(location_cam = paste0(location, "_", camera_model)) %>%
  select(location_cam, date_detected, field_of_view, common_name, age_class, sex, number_individuals) %>%
  # Amalgamate tags of same species in same image
  mutate(number_individuals = as.numeric(ifelse(number_individuals == "VNA", 1, number_individuals))) %>%
  group_by(location_cam, date_detected, common_name) %>%
  mutate(number_individuals = sum(number_individuals),
         age_class = paste0(age_class, collapse = ", "),
         sex = paste0(sex, collapse = ", ")) %>%
  distinct(location_cam, date_detected, common_name, number_individuals, .keep_all = TRUE) %>%
  ungroup()

# Let's investigate the operating time ranges.
df_2019_ranges <- df_2019 %>%
  group_by(location_cam) %>%
  summarise(start_date_time = min(date_detected),
            end_date_time = max(date_detected))

# Notes:
# 739 should be removed from the comparison altogether - HF2 basically never worked.
# 896 PC ended early. Adjust according. Truncate both cameras to July 15, 2019.

# All of the out-of-range images are in 788-NW PC900. Starts on January 27, 2019.
out_of_range <- df_2019 %>%
  filter(!field_of_view == "WITHIN") %>%
  select(location_cam) %>%
  distinct()

# Remove non-overlap periods. Kind of a hacky way to do it, but oh well.
df_2019_adj <- df_2019 %>%
  # Remove 739.
  filter(!str_detect(location_cam, "^739")) %>%
  # End 896 early.
  filter(!(str_detect(location_cam, "^896") & date_detected > as.Date("2019-07-14 23:59:59"))) %>%
  # End 788 early as well.
  filter(!(str_detect(location_cam, "^788") & date_detected > as.Date("2019-01-26 23:59:59")))

# Now let's calculate series.

df_series_19 <- df_2019_adj %>%
  # Just native species.
  filter(common_name %in% native_sp) %>%
  # Let's append camera model to location
  mutate(date_detected = ymd_hms(date_detected)) %>%
  # Order observations
  arrange(location_cam, date_detected, common_name) %>%
  # Assess series
  mutate(series_num = 0,
         date_detected_lag = lag(date_detected),
         diff_time = as.numeric(date_detected - date_detected_lag),
         common_name_lag = lag(common_name),
         diff_sp = ifelse(common_name != common_name_lag, TRUE, FALSE),
         location_cam_lag = lag(location_cam),
         diff_loc = ifelse(location_cam != location_cam_lag, TRUE, FALSE),
         gap_check = ifelse(diff_loc == FALSE & diff_sp == FALSE & (diff_time <= 120 & diff_time >= 20), 1, 0),
         diff_series = ifelse(diff_loc == TRUE | diff_sp == TRUE | diff_time > 120, 1, 0),
         series_num = c(0, cumsum(diff_series[-1]))) %>%
  left_join(df_gap_groups, by = "common_name") %>%
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time")) %>%
  mutate(pred = replace_na(pred, 1),
         diff_time_adj = round(ifelse(gap_check == 1, diff_time * (1 - pred), diff_time), digits = 2))

# Calculate total time in front of the camera, by series

# Start with series that have >1 image
df_tts_multiple <- df_series_19 %>%
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) %>%
  filter(series_num_previous == series_num) %>%
  group_by(series_num) %>%
  summarise(n_images = n() + 1,
            total_time = sum(diff_time_adj))

# Next, single-image series'
df_tts_single <- df_series_19 %>%
  group_by(series_num) %>%
  summarise(n_images = n()) %>%
  # Keep only series with 1 image
  filter(n_images == 1) %>%
  mutate(total_time = 0)

# Bind together
df_tts_all <- df_tts_multiple %>%
  bind_rows(df_tts_single) %>%
  arrange(series_num)

# Add time between photos, accounting for the average number of animals in each photo of a series
df_tts_final <- df_series_19 %>%
  mutate(number_individuals = as.numeric(ifelse(number_individuals == "VNA", 1, number_individuals))) %>%
  # Join in average tbp, by species; note that this is slightly different from results calculated above (incl. non-ABMI)
  left_join(df_tbp, by = "common_name") %>%
  select(series_num, common_name, number_individuals, tbp) %>%
  group_by(series_num) %>%
  # Number of individuals in a series
  mutate(avg_individuals = mean(number_individuals)) %>%
  ungroup() %>%
  select(-number_individuals) %>%
  distinct() %>%
  left_join(df_tts_all, by = "series_num") %>%
  mutate(series_total_time = (total_time + tbp) * avg_individuals) %>%
  select(series_num, common_name, n_images, series_total_time)

# Calculate total time in front of camera, by deployment, year, and species (tt = total time)

# Seasonal start/end dates (julian day):
summer.start.j <- 106 # April 16
summer.end.j <- 288 # October 15

df_tt <- df_series_19 %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final, by = c("series_num", "common_name")) %>%
  select(series_num, n_images, location_cam, date_detected, common_name, series_total_time) %>%
  ungroup() %>%
  mutate(julian = as.numeric(format(date_detected, "%j")),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "summer", "winter")) %>%
  mutate_at(c("location_cam", "common_name", "season"), factor) %>%
  group_by(location_cam, common_name, season, .drop = FALSE) %>%
  summarise(total_duration = sum(series_total_time),
            total_images = sum(n_images),
            total_series = n()) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character)

write_csv(df_tt, "data/processed/summary_of_series_2019.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Let's grab what I can for 2021 too :)

# List model comparison projects
cmc_21 <- wt_get_download_summary(sensor_id = "CAM") %>%
  filter(str_detect(project, "Comparison"),
         # want 2021
         year > 2020) %>%
  select(project_id) %>%
  pull() %>%
  unlist()

# Obtain HP2X data
hp2 <- wt_download_report(project_id = cmc_21, sensor_id = "CAM", cols_def = FALSE, weather_cols = FALSE)

hp2_dep <- hp2 %>% select(location) %>% distinct() %>% pull()

# Obtain data from OSM 2021 for the PC900

# OSM 2021 project ID
osm_21 <- wt_get_download_summary(sensor_id = "CAM") %>%
  filter(str_detect(project, "OSM"),
         year == "2021") %>%
  select(project_id) %>%
  pull() %>%
  unlist()

# Obtain PC900 data
pc900 <- wt_download_report(project_id = osm_21,
                                   sensor_id = "CAM",
                                   cols_def = FALSE,
                                   weather_cols = FALSE) %>%
  filter(location %in% hp2_dep)

# Alright. Seems a little funky. Lots more PC900.

# Let's look at time ranges.
ranges_21 <- hp2 %>% bind_rows(pc900) %>%
  mutate(location_cam = paste0(location, "_", camera_model)) %>%
  group_by(location_cam) %>%
  summarise(start_date_time = min(date_detected),
            end_date_time = max(date_detected))
# Seems good except for 3-1F1-CA2. PC900 ends early. 2021-06-09.

out_of_range <- hp2 %>% bind_rows(pc900) %>%
  filter(!field_of_view == "WITHIN") %>%
  select(location, camera_model, field_of_view, date_detected) %>%
  distinct() %>%
  filter(!field_of_view == "Out of Range")
# Funky ones:
# 2-2A1-CA1 - both models end early. 2021-05-25.
# 3-2A1-CA1 HP2 - ends 2021-11-15 and starts again 2021-11-22
# 3-2B1-CA3 HP2 - ends 2021-11-16 and starts again 2021-11-19.

# Remove non-overlap periods. Kind of a hacky way to do it, but oh well.
df_2021_adj <- hp2 %>% bind_rows(pc900) %>%
  mutate(location_cam = paste0(location, "_", camera_model)) %>%
  # End 3-1F1-CA2 early.
  filter(!(str_detect(location_cam, "^3-1F1-CA2") & date_detected > as.Date("2021-06-08 23:59:59"))) %>%
  # End 2-2A1-CA1 early.
  filter(!(str_detect(location_cam, "^2-2A1-CA1") & date_detected > as.Date("2021-05-24 23:59:59"))) %>%
  # End 3-2A1-CA1 early, and restart it a week later.
  filter(!(str_detect(location_cam, "^3-2A1-CA1") & (date_detected > as.Date("2021-11-14 23:59:59") & date_detected < as.Date("2021-11-23 00:00:01")))) %>%
  # End 3-2B1-CA3 early, and restart it a few days later.
  filter(!(str_detect(location_cam, "^3-2B1-CA3") & (date_detected > as.Date("2021-11-15 23:59:59") & date_detected < as.Date("2021-11-20 00:00:01"))))

# Great! Nice job.

# Now let's calculate series.

df_series_21 <- df_2021_adj %>%
  # Just native species.
  filter(common_name %in% native_sp) %>%
  # Let's append camera model to location
  mutate(date_detected = ymd_hms(date_detected)) %>%
  # Order observations
  arrange(location_cam, date_detected, common_name) %>%
  # Assess series
  mutate(series_num = 0,
         date_detected_lag = lag(date_detected),
         diff_time = as.numeric(date_detected - date_detected_lag),
         common_name_lag = lag(common_name),
         diff_sp = ifelse(common_name != common_name_lag, TRUE, FALSE),
         location_cam_lag = lag(location_cam),
         diff_loc = ifelse(location_cam != location_cam_lag, TRUE, FALSE),
         gap_check = ifelse(diff_loc == FALSE & diff_sp == FALSE & (diff_time <= 120 & diff_time >= 20), 1, 0),
         diff_series = ifelse(diff_loc == TRUE | diff_sp == TRUE | diff_time > 120, 1, 0),
         series_num = c(0, cumsum(diff_series[-1]))) %>%
  left_join(df_gap_groups, by = "common_name") %>%
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time")) %>%
  mutate(pred = replace_na(pred, 1),
         diff_time_adj = round(ifelse(gap_check == 1, diff_time * (1 - pred), diff_time), digits = 2))

# Calculate total time in front of the camera, by series

# Start with series that have >1 image
df_tts_multiple <- df_series_21 %>%
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) %>%
  filter(series_num_previous == series_num) %>%
  group_by(series_num) %>%
  summarise(n_images = n() + 1,
            total_time = sum(diff_time_adj))

# Next, single-image series'
df_tts_single <- df_series_21 %>%
  group_by(series_num) %>%
  summarise(n_images = n()) %>%
  # Keep only series with 1 image
  filter(n_images == 1) %>%
  mutate(total_time = 0)

# Bind together
df_tts_all <- df_tts_multiple %>%
  bind_rows(df_tts_single) %>%
  arrange(series_num)

# Add time between photos, accounting for the average number of animals in each photo of a series
df_tts_final <- df_series_21 %>%
  mutate(number_individuals = as.numeric(ifelse(number_individuals == "VNA", 1, number_individuals))) %>%
  # Join in average tbp, by species; note that this is slightly different from results calculated above (incl. non-ABMI)
  left_join(df_tbp, by = "common_name") %>%
  select(series_num, common_name, number_individuals, tbp) %>%
  group_by(series_num) %>%
  # Number of individuals in a series
  mutate(avg_individuals = mean(number_individuals)) %>%
  ungroup() %>%
  select(-number_individuals) %>%
  distinct() %>%
  left_join(df_tts_all, by = "series_num") %>%
  mutate(series_total_time = (total_time + tbp) * avg_individuals) %>%
  select(series_num, common_name, n_images, series_total_time)

# Calculate total time in front of camera, by deployment, year, and species (tt = total time)

# Seasonal start/end dates (julian day):
summer.start.j <- 106 # April 16
summer.end.j <- 288 # October 15

df_tt <- df_series_21 %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final, by = c("series_num", "common_name")) %>%
  select(series_num, n_images, location_cam, date_detected, common_name, series_total_time) %>%
  ungroup() %>%
  mutate(julian = as.numeric(format(date_detected, "%j")),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "summer", "winter")) %>%
  mutate_at(c("location_cam", "common_name", "season"), factor) %>%
  group_by(location_cam, common_name, season, .drop = FALSE) %>%
  summarise(total_duration = sum(series_total_time),
            total_images = sum(n_images),
            total_series = n()) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character)

write_csv(df_tt, "data/processed/summary_of_series_2021.csv")








