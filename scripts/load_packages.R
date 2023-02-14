required_Packages_Install <- c("tidyverse")

for(Package in required_Packages_Install) {
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  library(Package,character.only = TRUE)
}