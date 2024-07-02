################################ 

# GOAL: to clean the midday water potential and analyze the data -------------------------------
# authors: sharathsp93@gmail.com

#. tasks: 
#.      load the file and clean
#.      plot
#.      test for differences between time
#.      export file to merge it with Gs data
###############################


# 1. preparation ---------------------------------------------------------

pacman::p_load(tidyverse, janitor)


# 2. load and clean the data ----------------------------------------------



# list files
psi_files_path <- here::here("data/water-potential/")

# list all the pv curve measured files
psi_files <- list.files(psi_files_path, recursive = T, full.names = T, pattern = "xlsx")

psi_midday <- tibble(files = psi_files) %>% 
  mutate(data = map(files, ~ readxl::read_excel(.x, skip = 3))) %>%
  unnest(data) %>% 
  clean_names()

# clean and recalculate
psi_midday_clean <- psi_midday %>% 
  # extract time components
  mutate(time = lubridate::ymd_hms(time),
         hour = hour(time),
         minute = minute(time),
         time =  hm(paste0(hour, ":", minute)),
         minutes = round(period_to_seconds(hms(time))/60)) %>%
  rowwise() %>% 
  # calculate in MPa and extract plant_id's
  mutate(psi = (mean(c(psi_1, psi_2), na.rm = T)/10),
         plant_num = str_sub(sample_id, 6, 7),
         meas_points = case_when(c(time < '10H 30M 0S') ~ "1st",
                                 c(time > '10H 30M 0S' & time < '11H 20M 0S') ~ "2nd",
                                 T ~ "3rd")) %>% 
  # remove unnecessary variables
  select(!c(psi_1, psi_2, comments, hour, minute))


# 3. Plots ----------------------------------------------------------------

# visit https://cran.r-project.org/web/packages/esquisse/readme/README.html 
# download and play around with esquisse!

psi_midday_clean %>% 
  ggplot(aes(minutes/60, psi, group = sample_id, col = sample_id)) +
  geom_point() +
  geom_line() + 
  theme_bw()


# load Gs and clean -------------------------------------------------------


# list files
gs_files_path <- here::here("data/Gs/")

# list all the pv curve measured files
gs_files <- list.files(gs_files_path, recursive = T, full.names = T, pattern = "xlsx")

# list all the sheets with the pv data
# read the data here 
gs_cww <-  tibble(files = gs_files) %>% 
  mutate(data = map(files, ~ readxl::read_excel(.x, skip = 1))) %>%
  unnest(data) %>% 
  clean_names() %>% 
  select(obs_number, time, date, species, tree_num, gsw,
         vpd_leaf = vp_dleaf, tleaf) %>% 
  filter(!is.na(obs_number))

gs_cww %>% 
  names()

gs_cww_clean <- gs_cww %>% 
  mutate(across(c(gsw, vpd_leaf, tleaf), ~as.numeric(.))) %>% 
  mutate(time_meas = lubridate::hms(time),
         minutes = round(period_to_seconds(hms(time_meas))/60),
         meas_points = case_when(c(time_meas < '10H 30M 0S') ~ "1st",
                                 c(time_meas > '10H 30M 0S' & time_meas < '11H 20M 0S') ~ "2nd",
                                 T ~ "3rd")) %>% 
  group_by(tree_num, species, meas_points) %>% 
  mutate(gsw = mean(gsw))

gs_cww_clean  %>% 
  ggplot(aes(minutes/60, gsw, group = interaction(species, tree_num), col = species)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~species)

# HOW do we improve this graph further?
# labeling axis? 
# better comparability? 


  


