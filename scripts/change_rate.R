## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

source("scripts/load_packages.R")

#profile.pattern <- "prof_6|prof_7|prof_8|prof_9|prof_17|prof_41"
profile.pattern <- "prof"
source("scripts/import_profiles.R")

## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), 
                            skip = 3, show_col_types = FALSE)

## Combine and add euclidean distance
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile") %>%
  select(profile, Park, BasePoint_X, BasePoint_Y, season:z) %>%
  rowwise() %>%
  mutate(euc_dist = sqrt(((BasePoint_X - x)^2) + ((BasePoint_Y - y)^2))) %>%
  group_by(profile, year, season) %>% # can probably drop season
  mutate(X_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(Y_midpoint = ((min(y) + max(y))/2)) %>%
  mutate(euc_avg = mean(euc_dist)) %>%
  group_by(profile) %>%
  mutate(slope = ((max(y) - min(y)) / (max(x) - min(x)))) %>%
  mutate(slope = ifelse(slope > 1, "positive", "negative")) %>%
  ##
  drop_na()

## Prelim visual of data vs BasePoint
g <- ggplot(data = complete.profile) +
  geom_point(aes(x = x, y = y, color = profile)) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "black", size = 3) +
  ggtitle(paste("Profile:", profile.pattern))
g

# With color by year and best fit
partial.visual <- complete.profile #%>%
  #filter(profile %in% c(6, 7, 8, 9, 17, 41))

ggplot(data = partial.visual) +
  geom_point(aes(x = x, y = y), alpha = 0.2) +
  geom_point(aes(x = BasePoint_X, y = BasePoint_Y), color = "black", size = 3) +
  geom_point(aes(x = X_midpoint, y = Y_midpoint), color = "blue", size = 3) +
  ggtitle(paste("Profile:", profile.pattern))

## Euclidean distances
euclidean <- partial.visual %>%
  group_by(profile, year) %>%
  select(profile, year, slope, BasePoint_X, BasePoint_Y, X_midpoint, Y_midpoint) %>%
  mutate(X_midpoint = mean(X_midpoint),
         Y_midpoint = mean(Y_midpoint)) %>%
  unique() %>%
  mutate(euc_dist = sqrt(((BasePoint_X - X_midpoint)^2) + ((BasePoint_Y - Y_midpoint)^2)))
euclidean$year <- factor(euclidean$year, levels =  c("97", "98", "99","00", "01", "02", "03",
                                                     "04", "05", "06", "07", "08", "09", "10",
                                                     "11", "12", "13", "14", "15", "16", "17",
                                                     "18", "19", "20", "21", "22"))

## Visualize euclidean distance from average Euclidean distance of each year
ggplot(euclidean, aes(year, euc_dist, group = profile, color=slope)) +
  facet_wrap(~profile, scales = "free") +
  geom_col(position = position_dodge(width = 0.5)) +
  geom_line(aes(group = slope), position = position_dodge(width = 1),
            size = 2) +
  geom_smooth(method = "lm", se = TRUE, color="black") +
  theme(legend.position = "none") +
  ggtitle(paste("Profile", profile.pattern, ": Midpoint Euclidean Distance from Base Point"))

## Line plot overlay
ggplot(euclidean, aes(year, euc_dist, 
                      group = profile, color = factor(profile))) +
  geom_point() +
  geom_line()



ggplot(dat %>% group_by(pairs) %>%
         mutate(slope = (value[t==2] - value[t==1])/(2-1)),
       aes(t, value, group=pairs, linetype=group, colour=slope > 0)) +
  geom_point() +
  geom_line()




#### CHECK OUT LATER
library(healthyR.ts)
data_tbl <- ts_to_tbl(AirPassengers) |>
  select(-index)
output <- ts_ma_plot(
  .data = data_tbl,
  .date_col = date_col,
  .value_col = value
)
output$pgrid
output$xts_plt

                            