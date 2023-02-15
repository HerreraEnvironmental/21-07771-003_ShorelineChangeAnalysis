## RLionheart
## 21-0771-001
## January 2023
## Shoreline Conservation Areas, Washington State Parks

required_Packages_Install <- c("data.table",
                               "ggmap",
                               "rgl",
                               "scatterplot3d",
                               "plotly",
                               "tidyverse")

for(Package in required_Packages_Install) {
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  library(Package, character.only = TRUE)
}

