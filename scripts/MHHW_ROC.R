## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks


## Where does the "shore" line cross the MHHW plane? 

# -------------------------------------------------------------------------

# profile.pattern <- "prof_16"
# year.pattern <- c("00")
# 
# source("scripts/src/load_packages.R")
# source("scripts/src/import_profiles.R")

## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), 
                            skip = 3, show_col_types = FALSE) %>%
  filter(profile %in% str_extract_all(profile.pattern,"\\(?[0-9,.]+\\)?")[[1]])

## Combine
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, MHHW, BasePoint_X, BasePoint_Y, season:z) 

# How far are those points from the Base Point? -------------------------------------------------
MHHW.dist <- complete.profile %>%
  filter(z == MHHW) %>%
  group_by(year) %>%
  mutate(x = mean(x),
         y = mean(y)) %>%
  select(profile, year, BasePoint_X, BasePoint_Y, x, y, z) %>%
  unique() %>%
  group_by(profile, year) %>%
  mutate(euc_dist_to_BP = sqrt(((BasePoint_X - x)^2) + ((BasePoint_Y -  y)^2)))

MHHW.dist.plot <- ggplot(MHHW.dist, aes(x = year, y = euc_dist_to_BP)) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle(paste("Profile", profile.pattern, "MHHW Euclidean Distance from BP")) 

MHHW.dist.plot

MHHW.ROC <- MHHW.dist %>%
  group_by(profile) %>% 
  mutate(pct_change = (euc_dist_to_BP/lead(euc_dist_to_BP) - 1) * 100)

MHHW.ROC.plot <- ggplot(MHHW.ROC, aes(x = year, y = pct_change)) +
  geom_bar(position = "dodge", stat = "identity") + 
  ggtitle(paste("Profile", profile.pattern, "MHHW Euclidean Rate of Change")) 

MHHW.ROC.plot