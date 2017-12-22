# install packages
if (!require(devtools)) {
  install.packages("devtools")
}

if (!require(tweenr) ){
  devtools::install_github("thomasp85/tweenr")
}

# load library
library(tweenr)
library(ggplot2)
library(gganimate)

# tweenr expect a list of data frames as "key frames"
# the keys can be a dataframe with only the column that chanbe
# not the case here :)
key_frames <- list(
  data.frame(x=0, y=0, size=1, color="red", stringsAsFactors = F),
  data.frame(x=1, y=2, size=5, color="blue", stringsAsFactors = F),
  data.frame(x=2, y=1, size=10, color="green", stringsAsFactors = F),
  data.frame(x=3, y=2, size=8, color="blue", stringsAsFactors = F),
  data.frame(x=1, y=3, size=5, color="red", stringsAsFactors = F),
  data.frame(x=0, y=0, size=1, color="red", stringsAsFactors = F)
  )

# to interpoling the colour, the column can't be a "factor"

# interpoling keys frame values
tf <- tween_states(key_frames, tweenlength = 2, statelength = 1,
                   ease = c('cubic-in-out', 'cubic-in-out'),
                   nframes=100)

# look the ".frame" column
str(tf)

# plotting and using gganimate in the ".frame" dimension
p <- ggplot(tf, aes(x,y, color=color, frame=.frame)) +
  theme(legend.position = "none") +
  geom_point(aes(size=size))

# lets see
gganimate(p,interval=.2)


