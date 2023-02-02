library(ggplot2)
library(flextable) ## check this out!!
iris <- iris
ggplot(data = iris, aes(Sepal.Length)) + geom_bar(color="grey", fill="red") +
  labs(x = "Length of Sepal", 
       y = "Count of flowers", 
       title = "Number of flowers \nby sepal length",
       caption = "Source: IRIS Dataset \nBase R Package")
theme_organisation <- function(){
  font <- "Times New Roman"
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),    
    panel.border = element_rect(colour = "black", fill = NA, linetype = 3, size = 0.5),
    panel.background = element_rect(fill = "#05a6f0"),
    legend.position = "bottom",
    plot.title = element_text( family = font, size = 20, face = 'bold', hjust = 0,vjust = 2),               
    axis.text = element_text(family = font,size = 9),                
    axis.text.x = element_text(margin=margin(5, b = 10))
  )
}
ggplot(data = iris, aes(Sepal.Length)) + geom_bar(color="grey", fill="red") +
  labs(x = "Length of Sepal", 
       y = "Count of flowers", 
       title = "Number of flowers \nby sepal length",
       caption = "Source: IRIS Dataset \nBase R Package") +
  theme_organisation()
library(magick)
logo <- image_read("../Useless_R_functions/image/myiriscompany.png")
#adding the logo
grid::grid.raster(logo, x = 0.1, y = 0.02, just = c('left', 'bottom'), width = unit(1.9, 'inches'))



library(flextable)
myiris <- flextable(head(iris), 
                    col_keys = c("Species", "Petal.Width", "Sepal.Length", "Sepal.Width" ))
myiris <- color(myiris, ~ Sepal.Length > 4.5, ~ Sepal.Length, color = "red")
myiris <- add_header_row(
  x = myiris, values = c("Name and Petals", "Measures on Sepal"),colwidths = c(2, 2))
myiris