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
wildschwein_BE <- group_by(wildschwein_BE,TierID)
sabi <- filter(wildschwein_BE, TierID == "002A")
sabi
a <- c(sabi$E)
a
b <- c(sabi$N)
b
euclidean <- function(a,b) sqrt(sum(a-b)^2)
euclidean(a,b)
speedlenghtsabi <- euclidean(a,b)
speedsabi <- sppedlenghtsabi/1286
speedsabi

rosa <- filter(wildschwein_BE, TierID == "016A")
rosa
c <- c(rosa$E)
c
d <- c(rosa$N)
d
euclidean <- function(c,d) sqrt(sum(c-d)^2)
euclidean(c,d)
speedlengthrosa <- euclidean(c,d)
speedrosa <- speedlengthrosa/1412
speedrosa

ruth <- filter(wildschwein_BE, TierID == "018A")
ruth
e <- c(ruth$E)
e
f <- c(ruth$N)
f
euclidean <- function(e,f) sqrt(sum(e-f)^2)
euclidean(e,f)
speedlengthruth <- euclidean(e,f)
speedruth <- speedlengthruth/1599
speedruth

# The speed unit is m per second. For 002A is 24138006 m/s. For 016A is 13914487 m/s. And for 018A is 12049596 m/s.

# Task 3
caro60 <- read_delim("caro60.csv",",")
caro60 <- st_as_sf(caro60, coords = c("E", "N"), crs = 2056, remove = FALSE)
caro_3 <- seq(from = 1, to = 200, by=3)
caro_3 
caro_6 <- seq(from = 1, to = 200, by=6)
caro_6
caro_9 <- seq(from = 1, to = 200, by=9)
caro_9
nrow(caro60)
nrow(caro_3)
nrow(caro_6)
nrow(caro_9)
caro60 <- group_by(caro60,TierID)
caro60$timelag  <- as.integer(difftime(lead(caro60$DatetimeUTC), caro60$DatetimeUTC, units = "sec"))
caro60$timelag
str(caro60$timelag)
numbers <- 1:10
numbers
lead(numbers)
lead(numbers,n = 2)
lag(numbers)
lag(numbers,n = 5)
lag(numbers,n = 5, default = 0)
summary(caro60$timelag)
wildschwein_BE <- mutate(caro60,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC)))
summarise(caro60, mean = mean(timelag, na.rm = T))
# The timelag for the data set caro60 is 60 seconds. The steplength is 273095159 m. And the speed is 4551586 m/s.
caro60 <- group_by(caro60,TierID)
aa <- c(caro60$E)
aa
bb <- c(caro60$N)
bb
euclidean <- function(aa,bb) sqrt(sum(aa-bb)^2)
euclidean(aa,bb)
speedlenghtcaro <- euclidean(aa,bb)
speedcaro <- speedlenghtcaro/60
speedcaro


