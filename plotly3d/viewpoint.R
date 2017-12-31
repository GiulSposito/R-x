library(plotly)
set.seed(123)

# setwd("./plotly3d")

# Create Random Data
ds <- diamonds[sample(1:nrow(diamonds), size = 100),]

scene = list(camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25)))

plot_ly(ds, x = ~carat, y = ~cut, z = ~price, group = ~color, type = "scatter3d", mode = "markers",
        marker = list(opacity = 0.6, size = 4)) %>% 
  layout(title = "3D Scatter plot", scene = scene) -> p


export(p, file="./plots/test.png")
