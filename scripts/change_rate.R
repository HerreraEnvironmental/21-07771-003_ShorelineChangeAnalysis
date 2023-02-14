source("scripts/load_packages.R")


profile.erosion <- read_csv("data_raw/ProfilesForErosion.csv", 
                            col_names = c("Profile", "Park", "MHHW",
                                          "BasePoint_X", "BasePoint_Y", 
                                          "Start_Year", "Start_X", "Start_Y", "Start_Dist",
                                          "End_Year", "End_X", "End_Y", "End_Dist",
                                          "Total_Change", "Years", "Change_per_Year",
                                          "2050", "Comments"), skip = 3, show_col_types = FALSE)

# Visual layout of BP, Start, End
ggplot(profile.erosion) +
  geom_line(aes(x = BasePoint_X, y = BasePoint_Y), color = "darkred") +
  geom_line(aes(x = Start_X, y = Start_Y), color = "steelblue") +
  geom_line(aes(x = End_X, y = End_Y), color = "darkgreen")
                            