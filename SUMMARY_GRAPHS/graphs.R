## Graphs for GIS Inundation/Attribute Table

library(tidyverse)

## Step 1 import data and select important columns

relevant_column <- c("OID_", "ParkName", "Asset_Broad", "Asset_Detail", "Z_relToMHHW_FT", 
                     "SensitivityScore", "CoastEros_Score", "ExposureScore", "VulnerabilityScore",
                     "Hazard_Inundation", "Hazard_FEMA", "Hazard_Any")

polygons <- read.csv("SUMMARY_GRAPHS/AJM_Coast&PugetSound_Copy of Facilities_Review_20230510_polygon.csv") %>%
  select(any_of(relevant_column)) %>%
  mutate(feature_class = "polygon")
lines <- read.csv("SUMMARY_GRAPHS/AJM_Coast&PugetSound_Copy of Facilities_Review_20230510_line.csv") %>%
  select(any_of(relevant_column)) %>%
  mutate(feature_class = "line")
points <- read.csv("SUMMARY_GRAPHS/AJM_Coast&PugetSound_Copy of Facilities_Review_20230510_points.csv") %>%
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

ggplot(parks.data2, aes(x=reorder(ParkName, -VulnerabilityScore),
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

# --------------------------------------------------------------------
## Most urgent: distribution of vulnerability scores symbolized by count,
## number of items w/ that score. What is the overall distribution?
## is it clumpy, clustered around one score?

vulnerability <- parks.data %>%
  select(VulnerabilityScore)

vulnerability.mean <- round(mean(vulnerability$VulnerabilityScore, na.rm = TRUE), digits = 2)
vulnerability.std <- round(sd(vulnerability$VulnerabilityScore, na.rm = TRUE), digits = 2)
vulnerability.quant <- quantile(vulnerability$VulnerabilityScore, na.rm = TRUE)  

# Histogram overlaid with kernel density curve
ggplot(vulnerability, aes(x=VulnerabilityScore)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=.5,
                 colour="black", fill="#04A1FF") +
  geom_density(alpha=.2, fill='#1D455C') +
  xlab("Vulnerability Score") +
  ylab("Count") +
  ggtitle("Overall Vulnerability Score Distribution Across Parks")


### EXPOSURE
exposure <- parks.data %>%
  select(ParkName, ExposureScore)

no.exposure <- exposure %>%
  filter(ExposureScore == 0)

exposure.mean <- round(mean(exposure$ExposureScore, na.rm = TRUE), digits = 2)
exposure.sd <- round(sd(exposure$ExposureScore, na.rm = TRUE), digits = 2)
exposure.quant <- quantile(exposure$ExposureScore, na.rm = TRUE)  

# Histogram overlaid with kernel density curve
ggplot(exposure, aes(x=ExposureScore)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=.5,
                 colour="black", fill='#395C51') +
  geom_density(alpha=.2, fill='#1D455C') +
  xlab("Exposure Score") +
  ylab("Count") +
  ggtitle("Overall Exposure Score Distribution Across Parks")

### Sensitivity
sensitivity <- parks.data %>%
  select(ParkName, SensitivityScore) %>%
  filter(!ParkName %in% no.exposure$ParkName)

sensitivity.mean <- round(mean(sensitivity$SensitivityScore, na.rm = TRUE), digits = 2)
sensitivity.sd <- round(sd(sensitivity$SensitivityScore, na.rm = TRUE), digits = 2)
sensitivity.quant <- quantile(sensitivity$SensitivityScore, na.rm = TRUE)  

# Histogram overlaid with kernel density curve
ggplot(sensitivity, aes(x=SensitivityScore)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=.5,
                 colour="black", fill='#FFC42E') +
  geom_density(alpha=.2, fill='#1D455C') +
  xlab("Exposure Score") +
  ylab("Count") +
  ggtitle("Overall Sensitivity Score Distribution Across Parks")
  

# --------------------------------------------------------------------
## exposure by asset across whole park system? 
## asset on the x axis, count, type of vulnerability and count (inundation, erosion, both)
## overall across whole park system
palette =c("#04A1FF",'#048CBD','#3ECDA3','#DBA827','#395C51')

asset <- parks.data %>%
  select(-OID_, -feature_class, -ParkLocation2, -Hazard_Inundation, -Hazard_FEMA,
         -Asset_Detail, -ParkLocation, -Z_relToMHHW_FT) %>%
  rename(Hazard_Score = Hazard_Any) %>%
  unique() %>%
  pivot_longer(cols = contains("score"), 
               names_to = "ErosionScore")
  
ggplot(asset, aes(fill=ErosionScore, y=value, x=Asset_Broad)) + 
  geom_histogram(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, hjust=1)) +
  scale_fill_manual(values = palette)


asset <- parks.data %>%
  select(-OID_, -feature_class, -ParkLocation2, -Hazard_Inundation, -Hazard_FEMA,
         -Asset_Detail,-ParkName, -ParkLocation, -Z_relToMHHW_FT) %>%
  rename(Hazard_Score = Hazard_Any) %>%
  unique() %>%
  pivot_longer(cols = contains("score"), 
               names_to = "ErosionScore") %>%
  group_by(Asset_Broad, ErosionScore) %>%
  mutate(meanscore = mean(value, na.rm = TRUE))


ggplot(asset, aes(fill=ErosionScore, y=value, x=Asset_Broad)) + 
  geom_histogram(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, hjust=1)) +
  scale_fill_manual(values = palette)

# --------------------------------------------------------------------
## count of type of facility across whole park system in those 8 bins
## inundation, erosion, hazards
## 8 asset broad on bottom, y axis is count, stacked bar according to
## hazard erosion, hazard inundation, count of facilities with both
## inundation is either hazard inundation or hazard fema, that's both considered inundation
facilities <- parks.data %>%
  select(-OID_, -feature_class, -ParkLocation2, -Hazard_Inundation, -Hazard_FEMA,
         -Asset_Detail, -ParkLocation, -Z_relToMHHW_FT)
  
ggplot(facilities, aes(x=Asset_Broad)) +
  geom_histogram(position="dodge", stat = "count") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  ggtitle("Dash Point Detailed Assets")


# --------------------------------------------------------------------

## bar chart with just count of facilities
## x is broad asset, select only facilities that are vulnerable or exposed
## just a count
## This is per park filtered for field visit parks only
## stacked bar chart vulnerable to erosion, vulnerable to inundation score, both?
## "Hazard_Erosion"        


## Pixel count required