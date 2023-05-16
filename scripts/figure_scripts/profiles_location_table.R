library(knitr)
library(kableExtra)
library(magrittr)


profiles.w.location <- read.csv("data_secondary/profiles_with_location.csv")


kable(profiles.w.location, 
    caption = "Profiles with Corresponding Geographic Location",
    #format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    linesep = "",
    align = "l"
  ) %>%
  kableExtra::kable_styling(
    position = "left",
    #latex_options = c("striped", "repeat_header"),
    stripe_color = "gray!15",
    fixed_thead = TRUE
  ) %>%
  save_kable(file = "figures/test.png")


kable(profiles.w.location, "html") %>%
  kable_styling("striped", font_size = 16) %>%
  as_image(file = "~/HerreraWork/SCA_P21-07771-002/figures/test.png", width = 5)
