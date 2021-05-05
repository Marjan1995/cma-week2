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
caro60$timelag  <- as.integer(difftime(lead(caro60$DatetimeUTC), caro60$DatetimeUTC, units = "sec"))
caro60$timelag
str(caro60$timelag)
numbers <- 1:10
numbers
library(dplyr)
lead(numbers)
lead(numbers,n = 2)
lag(numbers)
lag(numbers,n = 5)
lag(numbers,n = 5, default = 0)
summary(caro60$timelag)
caro60 <- mutate(caro60,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC)))
summarise(caro60, mean = mean(timelag, na.rm = T))
aa <- c(caro60$E)
aa
bb <- c(caro60$N)
bb
euclidean <- function(aa,bb) sqrt(sum(aa-bb)^2)
euclidean(aa,bb)
speedlenghtcaro60 <- euclidean(aa,bb)
speedcaro60 <- speedlenghtcaro60/1
speedcaro60
#For the dataset caro60 is the timelag 1s.The speedlenght is 273095159m. And the speed 273095159m/s.

caro_3$timelag  <- as.integer(difftime(lead(caro_3$DatetimeUTC), caro_3$DatetimeUTC, units = "sec"))
caro_3$timelag
str(caro_3$timelag)
numbers <- 1:10
numbers
library(dplyr)
lead(numbers)
lead(numbers,n = 2)
lag(numbers)
lag(numbers,n = 5)
lag(numbers,n = 5, default = 0)
summary(caro_3$timelag)
caro_3 <- mutate(caro_3,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC)))
summarise(caro_3, mean = mean(timelag, na.rm = T))
aaa <- c(caro_3$E)
aaa
bbb <- c(caro_3$N)
bbb
euclidean <- function(aaa,bbb) sqrt(sum(aaa-bbb)^2)
euclidean(aaa,bbb)
speedlenghtcaro_3 <- euclidean(aaa,bbb)
speedcaro_3 <- speedlenghtcaro_3/3
speedcaro_3
# For the dataset caro_3 there are this results:3 seconds,91486858 m and 30495619 m/s.

caro_6$timelag  <- as.integer(difftime(lead(caro_6$DatetimeUTC), caro_6$DatetimeUTC, units = "sec"))
caro_6$timelag
str(caro_6$timelag)
numbers <- 1:10
numbers
library(dplyr)
lead(numbers)
lead(numbers,n = 2)
lag(numbers)
lag(numbers,n = 5)
lag(numbers,n = 5, default = 0)
summary(caro_6$timelag)
caro_6 <- mutate(caro_6,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC)))
summarise(caro_6, mean = mean(timelag, na.rm = T))
ee <- c(caro_6$E)
ee
ff <- c(caro_6$N)
ff
euclidean <- function(ee,ff) sqrt(sum(ee-ff)^2)
euclidean(ee,ff)
speedlenghtcaro_6 <- euclidean(ee,ff)
speedcaro_6 <- speedlenghtcaro_6/6
speedcaro_6
# For caro_6: 6s, 46426223 m and 7737704 m/s.

caro_9$timelag  <- as.integer(difftime(lead(caro_9$DatetimeUTC), caro_9$DatetimeUTC, units = "sec"))
caro_9$timelag
str(caro_9$timelag)
numbers <- 1:10
numbers
library(dplyr)
lead(numbers)
lead(numbers,n = 2)
lag(numbers)
lag(numbers,n = 5)
lag(numbers,n = 5, default = 0)
summary(caro_9$timelag)
caro_9 <- mutate(caro_9,timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC)))
summarise(caro_9, mean = mean(timelag, na.rm = T))
eee <- c(caro_9$E)
eee
fff <- c(caro_9$N)
fff
euclidean <- function(eee,fff) sqrt(sum(eee-fff)^2)
euclidean(eee,fff)
speedlenghtcaro_9 <- euclidean(eee,fff)
speedcaro_6 <- speedlenghtcaro_6/6
speedcaro_6
#For caro_9: 9s, 31405957 m and 7737704 m/s.









# Task 4
library(zoo)

