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







# Task 1
# How many individuals were tracked? There are 3 individuals, who were tracked. (TierID are only three: 002A,016A and 018A)
# For how long were the individual tracked? Are there gaps? 002A has been tracked for 1286 sec, 016A for 1412 sec and 018A for 1599 sec.
# Were all individuals tracked concurrently or sequentially?
# What is the temporal sampling interval between the locations?