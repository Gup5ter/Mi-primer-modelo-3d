library(ggplot2)
library(plotly)
library(dplyr)

data("diamonds")
head(diamonds)
str(diamonds)
summary(diamonds)

#Usamos las variables continuas de la data diamons
#Primeras 1000 variables categricas para el análisis

diamonds_small <- diamonds %>% sample_n(1000)
#crear grafo interactivo
#variables
#Peso del diamanta - profundidad total (%) - precio - corte
grafico_3d <- plot_ly(diamonds_small, 
                      x = ~carat,
                      y = ~depth,
                      z = ~price,
                      color = ~cut,
                      colors = c("blue", "green", "orange", "red", "purple"),
                      type = "scatter3d",
                      mode = "markers",
                      marker = list(size = 4, opacity = 0.8),
                      text = ~paste("Carat:", carat, "<br>",
                                    "Depth:", depth, "<br>",
                                    "Price:", price, "<br>",
                                    "Cut:", cut))
grafico_3d <- grafico_3d %>%
  layout(title = "Gráfico 3D interactivo",
         scene = list(xasis = list(title = "Carat (peso)",
                      yasis = list(title = "Depth (%)",
                      zasis = list(title = "Price (USD)")))))
grafico_3d

# Gráfico de burbujas
diamonds <- diamonds[sample(1:nrow(diamonds), 500),]
diamonds_small$cut_num <- as.numeric(diamonds_small$cut)
head(diamonds_small["cut", "cut_num"])

bubbles3d <- plot_ly(diamonds_small, 
                      x = ~carat,
                      y = ~depth,
                      z = ~price,
                      size = ~cut_num,
                      color = ~cut,
                      colors = c("blue", "green", "red", "purple"),
                      type = "scatter3d",
                      mode = "markers",
                      marker = list(opacity = 0.8),
                      text = ~paste("Carat:", carat, "<br>",
                                    "Depth:", depth, "<br>",
                                    "Price:", price, "<br>",
                                    "Cut:", cut))
bubbles3d <- bubbles3d %>%
  layout(title = "Gráfico 3D interactivo",
         scene = list(xasis = list(title = "Carat (peso)",
                      yasis = list(title = "Depth (%)",
                      zasis = list(title = "Price (USD)")))))
bubbles3d

#grafico surface
diamonds_small <- diamonds[sample(1:nrow(diamonds),500),]
x_vals <- seq(min(diamonds_small$carat), max(diamonds_small$carat), length.out = 30)
y_vals <- seq(min(diamonds_small$depth), max(diamonds_small$depth), length.out = 30)

z_matrix <- outer(x_vals, y_vals, function(x,y) x*5000 + y*100 + sin(x*y))

surface_3d <- plot_ly(
  x = x_vals,
  y = y_vals,
  z = z_matrix
) %>%
  add_surface(colorscale = list(c(0,1), c("blue", "red"))) %>%
  layout(
    title = "Surface 3D",
    scene = list(xaxis = list(title = "Carat (peso)"),
                 yaxis = list(title = "Depth (%)"),
                 zaxis = list(title = "Price (USD)")
                 
    )
  )

surface_3d
