# System Pre-req
# You need install ImageMagick cmd tool: https://www.imagemagick.org/

# Atention
# If you are in windows the new imagemagick executable calls "imagick.exe"
# but gganimate are expecting "convert.exe".
# the easyest way that I found to correct this is to do
# a copy of "imagick.exe" and rename it to "convert.exe" :) Lazy, I know!

# some preliminar checking of installed packages

# devtools to install gganimate
if (!require(devtools)) {
  install.packages("devtools")
}

# gganimate
if (!require(gganimate)){
  library(devtools)
  devtools::install_github("dgrtwo/gganimate")
}

# dataset 
if (!require(gapminder)) {
  install.packages("gapminder")
}

# libraries
library(gapminder)
library(ggplot2)
library(gganimate)

# hello gganimate

# ggplot: x=gdp, y=life, size=population, color=continent
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size=pop, color=continent,
                           frame=year)) + # <- look here!! the gganimate parameter
  # points (scatter)
  geom_point() +
  # adjusting x axis to log 10 (gdp)
  scale_x_log10()

# animate it
gganimate(p)


# animating a diferente parameter
# ggplot: x=gdp, y=life, size=population, color=continent
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size=pop, color=continent)) +
  # points (scatter)
  geom_point(alpha=.4) +
  # look here!! We overplot the points of the frame/year (smart!!)
  geom_point( aes(frame=year), color="black") + 
  # adjusting x axis to log 10 (gdp)
  scale_x_log10()

gganimate(p)


## cumulative parameter
# ggplot: x=gdp, y=life
# frame = year
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, frame = year)) +
  # we will plot a line for each controy, look the cumulative parameter
  geom_path(aes(cumulative = TRUE, group = country, color=continent)) + 
  scale_x_log10() +
  # split by continent
  facet_wrap(~continent) +
  theme(legend.position="none")

gganimate(p)
