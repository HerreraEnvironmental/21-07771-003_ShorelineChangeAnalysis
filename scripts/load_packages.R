required_Packages_Install <- c("ggplot2", "caret", "leaflet", "plotly", "magick")
for(Package in required_Packages_Install){
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  library(Package,character.only = TRUE)
}