## RLionheart
## 21-0771-001
## February 2023
## Shoreline Conservation Areas, Washington State Parks


## Where does the "shore" line cross the MHHW plane? 

# -------------------------------------------------------------------------

# year.pattern <- c("00")
# profile.pattern <- "prof_6"
# source("scripts/load_packages.R")
# source("scripts/import_profiles.R")

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

# xyz linear model + plot -------------------------------------------------

N <- nrow(complete.profile) 

mean_profile <- apply(complete.profile[, 8:10], 2, mean)
pca_profile  <- princomp(complete.profile[, 8:10]) 
vector_profile <- pca_profile$loadings[, 1]

profile_fit <- matrix(rep(mean_profile, each = N), ncol=3) + 
  pca_profile$score[, 1] %*% t(vector_profile) 
fitted.values <- as_tibble(profile_fit) %>%
  rename(x_fit = x, y_fit = y, z_fit = z)

plot_ly(x=fitted.values$x_fit, y=fitted.values$y_fit, z=fitted.values$z_fit,
        type="scatter3d", mode="markers") %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = list(text = paste("Profile:", profile.pattern), y = 0.9))
  

# Complete profile and MHHW plot -------------------------------------------------
marker <- list(color = ~year, showscale = TRUE,
               size = 2, shape = 1)

profileplot <- plot_ly(complete.profile %>% drop_na(), x = ~x, y = ~y, z = ~z,
      marker = marker, hoverinfo = "text", 
      text = ~paste('</br> Year: ', year)) %>%
  add_markers() %>%
  add_mesh(complete.profile, x = ~x, y = ~y, z = ~MHHW, opacity = 0.5) %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = list(text = paste("Profile:", profile.pattern, "Years:"), y = 0.9),
    legend = levels(year))

profileplot

# Where does each profile cross the MHHW? -------------------------------------------------
MHHW <- complete.profile %>%
  group_by(year) %>%
  filter(z == MHHW)

marker <- list(showscale = TRUE,
               size = 5, shape = 1)

MHHWplot <- plot_ly(MHHW %>% drop_na(), x = ~x, y = ~y, z = ~z,
                       marker = marker, hoverinfo = "text", 
                       text = ~paste('</br> Year: ', year)) %>%
  add_markers() %>%
  add_markers(x = ~BasePoint_X, y = ~BasePoint_Y, z = ~MHHW) %>%
  add_mesh(complete.profile, x = ~x, y = ~y, z = ~MHHW, opacity = 0.5) %>%
  layout(
    scene = list(xaxis = list(title = "x"),
                 yaxis = list(title = "y"),
                 zaxis = list(title = "z")),
    title = list(text = paste("Profile:", profile.pattern, "Years:"), y = 0.9),
    legend = levels(year))

MHHWplot


# How far are those points from the Base Point? -------------------------------------------------
MHHW_dist <- complete.profile %>%
  filter(z == MHHW) %>%
  group_by(year) %>%
  mutate(x = mean(x),
         y = mean(y)) %>%
  select(profile, year, BasePoint_X, BasePoint_Y, x, y, z) %>%
  unique() %>%
  group_by(profile, year) %>%
  mutate(euc_dist_to_BP = sqrt(((BasePoint_X - x)^2) + ((BasePoint_Y -  y)^2)))

ggplot(MHHW_dist, aes(x = year, y = euc_dist_to_BP)) +
  geom_bar(position = "dodge", stat = "identity")
