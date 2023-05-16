## Graphs for GIS Inundation/Attribute Table

library(tidyverse)

## Step 1 import data and select important columns

relevant_column <- c("OID_", "ParkName", "Asset_Broad", "Asset_Detail", "Z_relToMHHW_FT", 
                     "SensitivityScore", "CoastEros_Score", "CoastInund_Score", "ExposureScore", "VulnerabilityScore",
                     "Hazard_Erosion",  "Hazard_Inundation", "Hazard_FEMA", "Hazard_Any")

polygons <- read.csv("summary_files/data_raw/Scored_WA_Parks_Facilities_20230511_polygons.csv") %>%
  select(any_of(relevant_column)) %>%
  mutate(feature_class = "polygon")
lines <- read.csv("summary_files/data_raw/Scored_WA_Parks_Facilities_20230511_lines.csv") %>%
  select(any_of(relevant_column)) %>%
  mutate(feature_class = "line")
points <- read.csv("summary_files/data_raw/Scored_WA_Parks_Facilities_20230511_points.csv") %>%
  select(2, any_of(relevant_column)) %>%
  rename(ParkLocation = 1) %>%
  mutate(feature_class = "point")

locations <- points %>%
  select(ParkLocation, ParkName) %>%
  unique()

parks.data <- polygons %>%
  rbind(lines) %>%
  rbind(points %>% select(-ParkLocation)) %>%
  left_join(locations, by = "ParkName") %>%
  select(OID_, ParkName, ParkLocation, everything()) %>%
  arrange(SensitivityScore) %>%
  mutate(ParkLocation2 = case_when(
    (ParkName == "Clallam Bay") ~ "Pacific Coast",
    (ParkName == "Lilliwaup Tidelands") ~ "Puget Sound",
    (ParkName == "North Jetty") ~ "Pacific Coast",
    (ParkName == "Shine Tidelands") ~ "Puget Sound")) %>%
  mutate(ParkLocation = ifelse(is.na(ParkLocation), ParkLocation2, ParkLocation))
    


## Start graphin'
## add weight?
parks.data2 <- parks.data %>% 
  select(ParkLocation, ParkName, VulnerabilityScore) %>%
  group_by(ParkName) %>%
  mutate(VulnerabilityScore = mean(VulnerabilityScore, na.rm = TRUE)) %>%
  unique() %>%
  arrange(VulnerabilityScore)

ggplot(parks.data, aes(x=reorder(ParkName, -VulnerabilityScore),
                       y = VulnerabilityScore,
                       fill = ParkLocation)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Exposure Score by Park Location")

ggplot(parks.data, aes(x=reorder(ParkName,-Z_relToMHHW_FT),
                   y=Z_relToMHHW_FT,fill=ParkLocation))+
  geom_col(position = "dodge", width=0.9)+
  xlab('Park')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Z relative to MHHW")

## good for key findings, good one
ggplot(parks.data, aes(x = Asset_Broad, fill = ParkLocation)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle("Broad Asset by Park Location")

ggplot(parks.data, aes(x=Z_relToMHHW_FT, fill=ParkLocation)) +
  geom_histogram(position="dodge") +
  ggtitle("Histogram of z relative to MHHW")


t <- parks.data %>%
  select(ParkName, contains("score"), -contains("Coast")) %>%
  pivot_longer(cols = contains("score"), 
               names_to = "park") %>%
  left_join(locations, by = "ParkName")
t

## Vulnerability is exposure + sensitivty, rmove vulnerablity
## apply same weight 
ggplot(t, aes(fill=park, y=value, x=stats::reorder(ParkName, - value))) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

dash <- parks.data %>%
  filter(ParkName == "Dash Point") %>%
  mutate(ExposureScore = as.factor(ExposureScore))

ggplot(dash, aes(x=Asset_Detail, fill = ExposureScore)) +
  geom_histogram(position="dodge", stat = "count") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle("Dash Point Detailed Assets")

ggplot(t %>% filter(ParkName == "Dash Point"), aes(fill=park, y=value, x=stats::reorder(ParkName, - value))) + 
  geom_bar(position="dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

