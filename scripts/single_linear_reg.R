## RLionheart
## P21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks


#source("scripts/src/load_packages.R")
#source("scripts/src/import_profiles.R")

## Take a look only at a single profile
single.profile <- profiles.df %>%
  filter(year %in% year.pattern)

## Linear Regression
single.linear <- ggplot(data = single.profile, aes(x = x, y = y, group = year)) +
  geom_point() +
  geom_smooth(method = "lm", linewidth = 1) +
  theme(axis.text = element_blank()) +
  ggtitle(paste("Profile:", profile.pattern, "Year:", year.pattern))
single.linear



