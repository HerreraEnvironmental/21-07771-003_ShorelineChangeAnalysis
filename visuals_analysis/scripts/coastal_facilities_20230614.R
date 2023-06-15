# --------------------------------------------------------------------
# •	Stacked bar chart, if possible add the total number of facilities at the top of each of the 8 columns
# •	X-axis: Facility Type (use Asset_Broad)
# •	Y-axis: Number of Facilities
# •	Symbolize by three colors: Erosion, Inundation, and Both
# o	First, filter data for only features where Hazard_Erosion = 20 OR Hazard_Inundation = 18 OR 20
# o	Erosion (Select all features with Hazard_Erosion, but exclude any Erosion ones that have Hazard_Inundation)
# o	Inundation (Select all with Hazard_Inundation, but exclude any Inundation ones that have Hazard_Erosion)
# o	Both (Select those with Hazard_Erosion AND Hazard_Inundation)

## Graphs for GIS Inundation/Attribute Table

library(tidyverse)
source("visuals_analysis/scripts/src/functions.R")
group.colors <- c(Erosion = "#DBA827", Inundation = "#04A1FF", Both ='#3ECDA3')


## Set file pattern
file.pattern <- "20230614"

relevant.columns <- c("ParkName", "Asset_Broad", "Asset_Detail",
                     "SensitivityScore", "CoastEros_Score", "CoastInund_Score", "ExposureScore", "VulnerabilityScore",
                     "Hazard_Erosion",  "Hazard_Inundation", "Hazard_FEMA", "Hazard_Any")

# Import all files --------------------------------------------------
filenames <- RemoveCsv(list.files(path = "visuals_analysis/data_raw", pattern = file.pattern))

for (i in filenames) {
  filepath <- file.path("visuals_analysis/data_raw", paste(i, ".csv", sep = ""))
  assign(make.names(i), read.csv(filepath, stringsAsFactors = FALSE, check.names = TRUE))
}

## Select columns and create feature class
polygons <- Scored_WA_Parks_Facilities_20230614_polygons %>%
  select(any_of(relevant.columns)) %>%
  mutate(feature_class = "polygon")
lines <- Scored_WA_Parks_Facilities_20230614_lines %>%
  select(any_of(relevant.columns)) %>%
  mutate(feature_class = "line")
points <- Scored_WA_Parks_Facilities_20230614_points %>%
  select(any_of(relevant.columns)) %>%
  mutate(feature_class = "point")

## Combine to one df
parks.data <- polygons %>%
  rbind(lines) %>%
  rbind(points) 
parks.data <- parks.data[!(parks.data$ParkName=="" | parks.data$Asset_Broad=="" | parks.data$Asset_Detail==""), ]

## Assign custom classes
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

## Identify which facilities are vulnerable to erosion, inundation, or both
at.risk <- parks.new.classes %>%
  select(ID_col, Asset_Broad, CoastEros_Score, CoastInund_Score) %>%
  mutate(Erosion = ifelse(CoastEros_Score == 20, "Erosion", NA)) %>%
  mutate(Inundation = ifelse(CoastInund_Score %in% c(18, 20), "Inundation", NA)) %>%
  mutate(Both = ifelse(!is.na(Erosion) & !is.na(Inundation), "Both", NA)) %>%
  mutate(Erosion = ifelse(!is.na(Both), NA, Erosion)) %>%
  mutate(Inundation = ifelse(!is.na(Both), NA, Inundation)) %>%
  filter_at(vars(Erosion, Inundation, Both), any_vars(!is.na(.)))

to.plot <- at.risk %>%
  pivot_longer(cols = c(5:7), names_to = "names", values_to = "hazard_type") %>%
  select(-names, -CoastEros_Score, -CoastInund_Score, -ID_col) %>%
  drop_na() %>%
  group_by(Asset_Broad, hazard_type) %>%
  mutate(facility_count = n()) %>%
  unique() %>%
  group_by(Asset_Broad) %>%
  mutate(complete_count = sum(facility_count))


## Plot complete figure
currently.impacted <- ggplot(to.plot, aes(fill=factor(hazard_type, levels = c("Erosion", "Inundation", "Both")),
                                         y=facility_count, 
                                         x=reorder(Asset_Broad, - complete_count)),
                             labels = labels) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  scale_fill_manual(values=group.colors) +
  xlab("Coastal Facility Type") +
  ylab("Number of Coastal Facilities") +
  labs(fill = "Hazard Type") +
  ggtitle("Coastal Facilities Currently Exposed")
currently.impacted

## Save output
ggsave("visuals_analysis/figures/20230614_CoastalFacilitiesCurrentlyImpacted.png", 
       plot = currently.impacted, width = 130,
       height = 130, units = "mm")
write.csv(to.plot, "visuals_analysis/data_secondary/20230614_CoastalFacilitesCurrent_counts.csv", row.names = FALSE)
