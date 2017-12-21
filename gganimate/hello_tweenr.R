# install packages
if (!require(devtools)) {
  install.packages("devtools")
}

if (!require(tweenr) ){
  devtools::install_github("thomasp85/tweenr")
}


library(tweenr)

key_frames <- list(
  data.frame(x=0, y=0, color="red"),
  data.frame(x=1, y=2, color="blue"),
  data.frame(x=2, y=1, color="green"),
  data.frame(x=3, y=2, color="blue"),
  data.frame(x=1, y=3, color="red"),
  data.frame(x=0, y=0, color="red")
  )

tf <- tween_states(key_frames, tweenlength = 2, statelength = 1,
                   ease = c('cubic-in-out', 'cubic-in-out'),
                   nframes=100)
str(tf)

p <- ggplot(tf, aes(x,y, color=color, frame=.frame)) +
  geom_point(size=3)

gganimate(p,interval=.2)
