library(tidyverse)

group.colors <- c(Erosion = "#DBA827", Inundation = "#04A1FF", Both ='#3ECDA3')

# Imports + tidying -------------------------------------------------------------------

## Name relevant columns to choose
relevant_column <- c("Asset_Broad", "Asset_Detail",
                     "Hazard_Erosion",  "Hazard_Inundation", "Hazard_FEMA")

## Import csvs
polygons <- read.csv("visuals_analysis/data_raw/Scored_WA_Parks_Facilities_20230614_polygons.csv") %>%
  select(any_of(relevant_column)) %>%
  mutate(feature_class = "polygon")
lines <- read.csv("visuals_analysis/data_raw/Scored_WA_Parks_Facilities_20230614_lines.csv") %>%
  select(any_of(relevant_column)) %>%
  mutate(feature_class = "line")
points <- read.csv("visuals_analysis/data_raw/Scored_WA_Parks_Facilities_20230614_points.csv") %>%
  select(any_of(relevant_column)) %>%
  mutate(feature_class = "point")

## Create complete dataframe
parks.data <- polygons %>%
  rbind(lines) %>%
  rbind(points)


# Assign new classes -------------------------------------------------------------------
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
  drop_na(Asset_Broad) %>%
  mutate(ID_col = row_number())

## Identify which facilities are vulnerable to erosion, inundation, xor both
at.risk <- parks.new.classes %>%
  select(ID_col, Asset_Broad, Hazard_Erosion, Hazard_FEMA, Hazard_Inundation) %>%
  mutate(Erosion = ifelse(Hazard_Erosion == 1 & Hazard_FEMA != 1 & Hazard_Inundation != 1,
                          "Erosion", NA)) %>%
  mutate(Inundation = ifelse(Hazard_Erosion != 1 & (Hazard_Inundation == 1 | Hazard_FEMA == 1), 
         "Inundation", NA)) %>%
  mutate(Both = ifelse(Hazard_Inundation == 1 & Hazard_Erosion == 1, "Both", NA)) %>%
  filter_at(vars(Erosion, Inundation, Both), any_vars(!is.na(.)))

## Drop any facilities that have no risk, pivot longer, and count total facilities
to.plot <- at.risk %>%
  pivot_longer(cols = c(6:8), names_to = "names", values_to = "hazard_type") %>%
  select(-c(Hazard_Erosion:names), -ID_col) %>%
  drop_na() %>%
  group_by(Asset_Broad, hazard_type) %>%
  mutate(facility_count = n()) %>%
  unique() %>%
  group_by(Asset_Broad) %>%
  mutate(complete_count = sum(facility_count))

## Graph
near.term <- ggplot(to.plot, aes(fill=factor(hazard_type, levels = c("Erosion", "Inundation", "Both")),
                                y=facility_count, 
                                x=reorder(Asset_Broad, - complete_count)),
                    labels = labels) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_fill_manual(values=group.colors) +
  xlab("Coastal Facility Type") +
  ylab("Number of Coastal Facilities") +
  labs(fill = "Hazard Type") +
  ggtitle("Coastal Facilities Exposed in the Near-Term")
near.term

ggsave("visuals_analysis/figures/20230614_CoastalFacilitiesImpactNearTerm.png", near.term, width = 130,
       height = 130, units = "mm")

write.csv(to.plot, "visuals_analysis/data_secondary/20230614_CoastalFacilitesNearTerm_counts.csv", row.names = FALSE)
