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
str(wildschwein_BE$timelag)
numbers <- 1:10
numbers

library(dplyr)
lead(numbers)
lead(numbers,n = 2)
lag(numbers)
lag(numbers,n = 5)
lag(numbers,n = 5, default = 0)
summary(wildschwein_BE$timelag)
wildschwein_BE <- mutate(wildschwein_BE,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC)))
summarise(wildschwein_BE, mean = mean(timelag, na.rm = T))
library(ggplot2)
ggplot(wildschwein_BE, 
       aes(x=wildschwein_BE$DatetimeUTC, y= wildschwein_BE$timelag, color=wildschwein_BE$TierID)) +
  geom_line(size=1) +
  labs(title = "Tracking")
  
# Task 1
# How many individuals were tracked? There are 3 individuals, who were tracked. (TierID are only three: 002A,016A and 018A)
# For how long were the individual tracked? Are there gaps? 002A has been tracked for 1286 sec, 016A for 1412 sec and 018A for 1599 sec
# Were all individuals tracked concurrently or sequentially? 002A was tracked seperately, but 016A and 018A werde tracked concurrently
# What is the temporal sampling interval between the locations? From October 2014 until July 2015


# Task 2
a <-c(wildschwein_BE$E)
b <- c(wildschwein_BE$N)
euclidean(a,b)
steplength <- as.numeric(euclidean (a,b), units="m")
steplength
speed_002A <- steplength/1286
speed_002A
speed_016A <- steplength/1412
speed_016A
speed_018A <- steplength/1599
speed_018A
# The speed unit is m per second. For 002A is 240300.6 m/s. For 016A is 218857.3 m/s. And for 018A is 193262.4 m/s.

# Task 3