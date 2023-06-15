library(tidyverse)

group.colors <- c(Erosion = "#DBA827", Inundation = "#04A1FF", Both ='#3ECDA3')

relevant_column <- c("ParkName", "Asset_Broad", "Asset_Detail",
                     "Hazard_Erosion",  "Hazard_Inundation", "Hazard_FEMA")

polygons <- read.csv("visuals_analysis/data_raw/Scored_WA_Parks_Facilities_20230614_polygons.csv") %>%
  select(any_of(relevant_column)) %>%
  mutate(feature_class = "polygon")
lines <- read.csv("visuals_analysis/data_raw/Scored_WA_Parks_Facilities_20230614_lines.csv") %>%
  select(any_of(relevant_column)) %>%
  mutate(feature_class = "line")
points <- read.csv("visuals_analysis/data_raw/Scored_WA_Parks_Facilities_20230614_points.csv") %>%
  select(any_of(relevant_column)) %>%
  mutate(feature_class = "point")


parks.data <- polygons %>%
  rbind(lines) %>%
  rbind(points)


# Graph -------------------------------------------------------------------

class.one <- c("Admin_Buildings", "Admin_Storage_NonHaz", "Admin_Storage_Haz",
               "DayUse_Utilities", "Accom_Roofed", "Sanitary_Utilities", "Sanitary_NoUtil")
class.two <- c("Circ_Bridges", "Circ_Emergency", "Circ_Parking", "Circ_Roads", "Circ_Trails")
class.three <- c("DayUse_NoUtil", "Accom_Camp_NoUtil", "Accom_Camp_Utilities", "Shore_Hard")
class.four <- c("Marine_LandBased", "Marine_Overwater")
class.five <- c("Util_Communication", "Util_Electric", "Util_Emergency", "Util_gas", "Util_WW",
                "Util_Stormwater", "Util_Water")

parks.new.classes <- parks.data %>%
  mutate(Asset_Broad = ifelse(Asset_Detail %in% class.one, "Building",
                              ifelse(Asset_Detail %in% class.two, "Circulation",
                                     ifelse(Asset_Detail %in% class.three, "Infrastructure",
                                            ifelse(Asset_Detail %in% class.four, "Marine",
                                                   ifelse(Asset_Detail %in% class.five, "Utility", NA)))))) %>%
  drop_na(Asset_Broad)

# o	Erosion 
# Select all features with Hazard_Erosion, 
# but exclude any erosion ones that have Hazard_Inundation OR Hazard_FEMA)
erosion <- parks.new.classes %>%
  select(-ParkName) %>%
  filter(Hazard_Erosion == 1) %>%
  filter(Hazard_FEMA != 1) %>%
  filter(Hazard_Inundation != 1) %>%
  group_by(Asset_Broad) %>%
  mutate(facility_count = n()) %>%
  mutate(hazard_type = "Erosion") %>%
  unique() %>%
  select(Asset_Broad, facility_count, hazard_type)

# o	Inundation 
# Select all with Hazard_Inundation OR Hazard_FEMA, 
# but exclude any inundation ones that have Hazard_Erosion)
inundation <- parks.new.classes %>%
  select(-ParkName) %>%
  filter(Hazard_Inundation == 1 | Hazard_FEMA == 1) %>%
  filter(Hazard_Erosion != 1) %>%
  group_by(Asset_Broad) %>%
  mutate(facility_count = n()) %>%
  mutate(hazard_type = "Inundation") %>%
  select(Asset_Broad, facility_count, hazard_type) %>%
  unique()

# o	Both (Select those with Hazard_Erosion AND Hazard_Inundation OR Hazard_FEMA)
both <- parks.new.classes %>%
  select(-ParkName) %>%
  filter(Hazard_Inundation == 1 & Hazard_Erosion == 1) %>%
  group_by(Asset_Broad) %>%
  mutate(facility_count = n()) %>%
  mutate(hazard_type = "Both") %>%
  select(Asset_Broad, facility_count, hazard_type) %>%
  unique()

toplot <- both %>%
  rbind(erosion) %>%
  rbind(inundation) %>%
  group_by(Asset_Broad) %>%
  mutate(complete_count = sum(facility_count))
toplot[toplot == "Shoreline"] <- "Shoreline Armor"


near.term <- ggplot(toplot, aes(fill=factor(hazard_type, levels = c("Erosion", "Inundation", "Both")),
                                y=facility_count, 
                                x=reorder(Asset_Broad, - complete_count)),
                    labels = labels) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_fill_manual(values=group.colors) +
  xlab("Coastal Facility Type") +
  ylab("Number of Coastal Facilities") +
  labs(fill = "Hazard Type") +
  ggtitle("Coastal Facilities Impacted in the Near-Term")
near.term

ggsave("visuals_analysis/figures/20230614_CoastalFacilitiesImpactNearTerm.png", near.term, width = 130,
       height = 130, units = "mm")

write.csv(toplot, "visuals_analysis/data_secondary/20230614_CoastalFacilitesNearTerm_counts.csv", row.names = FALSE)
