## RLionheart
## 21-0771-003
## Shoreline Conservation Areas, Washington State Parks

required_Packages_Install <- c("broom",
                               "data.table",
                               "factoextra",
                               "kableExtra",
                               "plotly",
                               "tidyverse")

for(Package in required_Packages_Install) {
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  suppressWarnings(library(Package, character.only = TRUE))
}

