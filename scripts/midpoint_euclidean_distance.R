## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

profile.pattern <- "prof"
source("scripts/src/import_profiles.R")

## Import erosion file for Base Point data
profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("profile", "Park", "MHHW",
                                          "X_BasePoint", "Y_BasePoint", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "Hannah", "2050", "Comments"), 
                            skip = 3, show_col_types = FALSE)

## Combine and add euclidean distance
complete.profile <- profile.erosion %>%
  full_join(profiles.df, by = "profile", multiple = "all") %>%
  select(profile, Park, X_BasePoint, Y_BasePoint, season:z) %>%
  group_by(profile, year)


## Euclidean distances
euclidean <- complete.profile %>%
  mutate(x_midpoint = ((min(x) + max(x))/2)) %>%
  mutate(y_midpoint = ((min(y) + max(y))/2)) %>%
  select(profile, Park, year, X_BasePoint, Y_BasePoint, x_midpoint,  y_midpoint) %>%
  unique() %>%
  arrange(profile, year) %>%
  group_by(profile) %>%
  mutate(euc_dist_to_BP = sqrt(((X_BasePoint - x_midpoint)^2) + ((Y_BasePoint -  y_midpoint)^2))) 

## Estimate net slope as a proxy for erosion
total.slope <- euclidean %>%
  drop_na() %>%
  group_by(profile) %>%
  mutate(profile_slope = ifelse(euc_dist_to_BP[which.min(year)] < euc_dist_to_BP[which.max(year)],
                                "Accretion", "Erosion")) %>%
  select(profile, profile_slope)

euclidean.with.slope <- euclidean %>%
  left_join(total.slope, by = "profile", multiple = "all")

## Download for cluster
cluster <- euclidean.with.slope %>%
  drop_na() %>%
  select(profile:year, euc_dist_to_BP) %>%
  unique() 

## Visualize euclidean distance from average Euclidean distance of each year
midpoint.euc.dist.plot <- ggplot(euclidean.with.slope %>% drop_na(), 
       aes(year, euc_dist_to_BP, fill=profile_slope, group = profile_slope)) +
  scale_fill_manual(values=c("#04A1FF", "tomato2")) +
  facet_wrap(~profile) +
  geom_col(position = position_dodge(width = 1)) +
  geom_line(aes(group = profile_slope), position = position_dodge(width = 1),
            linewidth = 1, color = "black") +
  geom_smooth(method = "lm", se = TRUE, color="black") +
  xlab("Year") +
  ylab("Distance in m from BasePoint") + 
  theme(axis.text.x = element_blank()) +
  guides(fill=guide_legend(title="")) +
  ggtitle("Net Accretion or Erosion per Profile")
midpoint.euc.dist.plot


### Extract equation parameters

equation.details <- euclidean.with.slope %>%
  select(profile, Park, year, euc_dist_to_BP) %>%
  unique() %>%
  drop_na() %>%
  group_by(profile) %>%
  mutate(dummy_year = row_number()) %>%
  do(model = lm(euc_dist_to_BP ~ dummy_year, data = .)) %>%
  mutate(intercept = coef(model)[1],
         slope = coef(model)[2],
         rsq = summary(model)$r.squared,
         se = summary(model)$sigma) %>%
  mutate(profile_direction = ifelse(slope > 0, "Accretion", "Erosion")) %>%
  select(profile, slope, rsq, se, profile_direction)

