library(xlsx)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gganimate)

# read dataset
data <- read.xlsx("./Dots/RedGreenGreyDots.xlsx", sheetIndex = 1)

# look the structure
head(data)

# generate data to plot
data %>%
  # round the time in 15 minutes slots
  mutate(Movement15 = lubridate::floor_date(MovementDateTime,"15 minutes")) %>% 
  # group by moviment, stage post e then by time slot
  group_by(IN_OUT, Movement_Type,Staging_Post,Movement15) %>% 
  mutate(
    # create a "transaction" field to count moviments
    counter = case_when (
      IN_OUT == 'IN' ~ 1,
      IN_OUT == 'OUT' ~ -1)
    ) %>%
  # sum the transactions to generate a balance
  mutate( Movement_15_SEQNO = cumsum(counter) ) %>% 
  # remove 'grouping' info
  ungroup() %>%
  mutate (Movement_Type = gsub("Transfer.*","Transfer",Movement_Type )) -> plot.data

# look the balance
head(plot.data[,c(6:10)])


# Set limits for plotting
lims <- as.POSIXct(strptime(c("2014-09-03 00:00","2014-09-03 24:00")
                            , format = "%Y-%m-%d %H:%M")) 

# lets plot
  # x = timeslot, y=balance, color=mov_type
p <-ggplot(plot.data,aes(Movement15,Movement_15_SEQNO, colour=Movement_Type,
                     frame=Movement15))+
  # plot as points with some "jitter"
  geom_jitter(width=0.10, aes(cumulative = TRUE)) +
  # geom_point() + # uncommnet this if you want without the jitter
  # manual color scale for move_type
  scale_colour_manual(values=c("#D7100D","#40B578","grey60"))+
  # sub plot by "staging post"
  facet_grid(Staging_Post~.,switch = "y")+
  # X axis configurations
  scale_x_datetime(date_labels="%H:%M",date_breaks = "3 hours",
                   limits = lims,
                   timezone = "CET",
                   expand = c(0,0))+
  # title and label
  ggtitle(label = "Anytown General Hospital | Wednesday 3rd September 2014 00:00 to 23:59\n",
          subtitle="A&E AND INPATIENT ARRIVALS, DEPARTURES AND TRANSFERS")+
  labs(x= NULL, y= NULL)+
  # Black and White theem
  theme_bw() + 
  # clean ups
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(axis.text.x=element_text(size=7)) +
  theme(axis.ticks.x=element_blank())+
  theme(legend.position="bottom")+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())+
  guides(color=guide_legend("Movement Type"))

gganimate(p, interval=.05)
