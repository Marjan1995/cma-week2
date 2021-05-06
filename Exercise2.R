## Load the necessary libraries ################################################

library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

## Import the downloaded csv ##################################################

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",")
wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)
wildschwein_BE <- group_by(wildschwein_BE,TierID)
wildschwein_BE$timelag  <- as.integer(difftime(lead(wildschwein_BE$DatetimeUTC), wildschwein_BE$DatetimeUTC, units = "sec"))
wildschwein_BE$timelag
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC)))
summarise(wildschwein_BE, min_time = min(DatetimeUTC),
          max_time = max(DatetimeUTC),
          Tracking_Period = difftime(max_time,min_time),
          min_lag = min(timelag, na.rm = T),
          max_lag = max(timelag, na.rm = T),
          median_lag = median(timelag, na.rm = T))
ggplot(wildschwein_BE)+geom_point(aes(x= wildschwein_BE$DatetimeUTC, y= wildschwein_BE$timelag, color=wildschwein_BE$TierID))
  
# Task 1
# There are 3 individuals, who were tracked. (TierID are only three: 002A,016A and 018A)
# 002A has been tracked for 338.5834 days, 016A for 234.6663 days and 018A for 261.6559 days.
# They were tracked concurrently.
# The temporal sampling interval is various.


# Task 2
wildschwein_BE <- group_by(wildschwein_BE,TierID)
wildschwein_BE$steplength <- as.numeric(sqrt((wildschwein_BE$E - lead(wildschwein_BE$E,1))^2 + (wildschwein_BE$N - lead(wildschwein_BE$N,1))^2))
wildschwein_BE$speed <- as.numeric(wildschwein_BE$steplength/wildschwein_BE$timelag)
wildschwein_BE <-  wildschwein_BE %>%
group_by(TierName) %>%
mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))
wildschwein_BE <- wildschwein_BE %>% 
group_by(TierName) %>%
mutate(speed = steplength/timelag)

# The speed unit is m per second. Because the unit of geometry is meter.

# Task 3
caro60 <- read_delim("caro60.csv",",")
caro60 <- st_as_sf(caro60, coords = c("E", "N"), crs = 2056, remove = FALSE)
caro_3 <- caro60[seq(1,nrow(caro60),by=3),]
caro_3 
caro_6 <- caro60[seq(1,nrow(caro60),by=6),]
caro_6
caro_9 <- caro60[seq(1,nrow(caro60),by=9),]
caro_9
nrow(caro60)
nrow(caro_3)
nrow(caro_6)
nrow(caro_9)
caro60 <- group_by(caro60,TierID)
caro60$timelag  <- as.integer(difftime(lead(caro60$DatetimeUTC), caro60$DatetimeUTC, units = "sec"))
caro60$timelag
caro60$steplength <- as.numeric(sqrt((caro60$E - lead(caro60$E,1))^2 + (caro60$N - lead(caro60$N,1))^2))
caro60$speed <- as.numeric(caro60$steplength/caro60$timelag)
caro_3 <- group_by(caro_3,TierID)
caro_3$timelag  <- as.integer(difftime(lead(caro_3$DatetimeUTC), caro_3$DatetimeUTC, units = "sec"))
caro_3$timelag
caro_3$steplength <- as.numeric(sqrt((caro_3$E - lead(caro_3$E,1))^2 + (caro_3$N - lead(caro_3$N,1))^2))
caro_3$speed <- as.numeric(caro_3$steplength/caro_3$timelag)
caro_6 <- group_by(caro_6,TierID)
caro_6$timelag  <- as.integer(difftime(lead(caro_6$DatetimeUTC), caro_6$DatetimeUTC, units = "sec"))
caro_6$timelag
caro_6$steplength <- as.numeric(sqrt((caro_6$E - lead(caro_6$E,1))^2 + (caro_6$N - lead(caro_6$N,1))^2))
caro_6$speed <- as.numeric(caro_6$steplength/caro_6$timelag)
caro_9 <- group_by(caro_9,TierID)
caro_9$timelag  <- as.integer(difftime(lead(caro_9$DatetimeUTC), caro_9$DatetimeUTC, units = "sec"))
caro_9$timelag
caro_9$steplength <- as.numeric(sqrt((caro_9$E - lead(caro_9$E,1))^2 + (caro_9$N - lead(caro_9$N,1))^2))
caro_9$speed <- as.numeric(caro_9$steplength/caro_9$timelag)

caro60_l <- caro60 %>%
group_by() %>%
summarise(do_union = FALSE) %>%
st_cast("LINESTRING")

caro_3_l <- caro_3 %>%
group_by() %>%
summarise(do_union = FALSE) %>%
st_cast("LINESTRING")

caro_6_l <- caro_6 %>%
group_by() %>%
summarise(do_union = FALSE) %>%
st_cast("LINESTRING")

caro_9_l <- caro_9 %>%
group_by() %>%
summarise(do_union = FALSE) %>%
st_cast("LINESTRING")



ggplot() + ggtitle("Caro60 bs Caro_3") + xlab("E") + ylab("N") + theme_set(theme_minimal()) + theme(aspect.ratio = 1) +
  geom_sf(data = caro60, aes(color = "1 Min"), alpha = 0.8) +
  geom_sf(data = caro60, aes(color = "1 Min"), alpha = 0.8) +
  geom_sf(data = caro_3, aes(color = "3 Min"), alpha = 0.8) +
  geom_sf(data = caro_3, aes(color = "3 Min"), alpha = 0.8) +
  coord_sf(datum = 2056)+
  scale_color_manual(name = "Interval", breaks = c("1 Min", "3 Min"), values = c("1 Min" = "cyan", "3 Min"= "magenta"))

ggplot() + ggtitle("Caro60 bs Caro_6") + xlab("E") + ylab("N") + theme_set(theme_minimal()) + theme(aspect.ratio = 1) +
  geom_sf(data = caro60, aes(color = "1 Min"), alpha = 0.8) +
  geom_sf(data = caro60_l, aes(color = "1 Min"), alpha = 0.8) +
  geom_sf(data = caro_6, aes(color = "6 Min"), alpha = 0.8) +
  geom_sf(data = caro_6_l, aes(color = "6 Min"), alpha = 0.8) +
  coord_sf(datum = 2056)+
  scale_color_manual(name = "Interval", breaks = c("1 Min", "6 Min"), values = c("1 Min" = "cyan", "6 Min"= "magenta"))

ggplot() + ggtitle("Caro60 bs Caro_9") + xlab("E") + ylab("N") + theme_set(theme_minimal()) + theme(aspect.ratio = 1) +
  geom_sf(data = caro60, aes(color = "1 Min"), alpha = 0.8) +
  geom_sf(data = caro60_l, aes(color = "1 Min"), alpha = 0.8) +
  geom_sf(data = caro_9, aes(color = "9 Min"), alpha = 0.8) +
  geom_sf(data = caro_9_l, aes(color = "9 Min"), alpha = 0.8) +
  coord_sf(datum = 2056)+
  scale_color_manual(name = "Interval", breaks = c("1 Min", "9 Min"), values = c("1 Min" = "cyan", "9 Min"= "magenta"))

ggplot()+
  ggtitle("Speed with different Interval")+
  ylab("Speed [m/s]")+
  xlab("Time [s]")+
  theme_set(theme_minimal()) +
  geom_line(data = caro60, aes(x = DatetimeUTC, y = speed, color ="1 Min"))+
  geom_line(data = caro_3, aes(x = DatetimeUTC, y = speed, color ="3 Min"))+
  geom_line(data = caro_6, aes(x = DatetimeUTC, y = speed, color ="6 Min"))+
  geom_line(data = caro_9, aes(x = DatetimeUTC, y = speed, color ="9 Min"))+
  scale_color_manual(name = "Sampling Interval", breaks = c("1 Min","3 Min", "6 Min","9 Min"), values = c("1 Min" = "cyan", "9 Min" = "magenta", "6 Min" = "blue", "3 Min" = "green"))
# Higher minutes have less difference in speed.


# Task 4
library(zoo)
example <- rnorm(10)
rollmean(example,k = 3,fill = NA,align = "left")
##  [1]  0.93634335  0.31709038  0.02370048  0.67869801  0.73369105  0.50401344
##  [7] -0.56144365 -0.56902598          NA          NA
rollmean(example,k = 4,fill = NA,align = "left")
##  [1]  0.6775521  0.2045005  0.5848215  0.5255629  0.3446928  0.1459635
##  [7] -0.4102301         NA         NA         NA

caro_RollWindow <- caro60 %>%
mutate(k3 = rollmean(speed, k=3, fill= NA, align = "left"),
       k6 = rollmean(speed, k=6, fill= NA, align = "left"),
       k9 = rollmean(speed, k=9, fill= NA, align = "left"),
       k30 = rollmean(speed, k=30, fill= NA, align = "left"))

ggplot(data = caro_RollWindow)+ ggtitle("Rolling Window")+
  ylab("Speed in m per second") +
  xlab("Time")+
    theme_set(theme_minimal())+
    geom_line(aes(x = DatetimeUTC, y = speed, color ="Original"))+
    geom_line(aes(x = DatetimeUTC, y = k3, color ="k3"))+
    geom_line(aes(x = DatetimeUTC, y = k6, color ="k6"))+
    geom_line(aes(x = DatetimeUTC, y = k9, color ="k9"))+
    geom_line(aes(x = DatetimeUTC, y = k30, color ="k30"))+
    scale_color_manual(name = "Size", breaks = c("Original","k3","k6","k9","k30"), values = c("Original" ="cyan", "k3"="magenta", "k6"="yellow", "k9"="red","k30"="green"))
  

    

