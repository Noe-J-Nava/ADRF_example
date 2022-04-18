# Noé J Nava
# ERS - MTED - APM
# Noe.Nava@usda.gov

# Note: The work in this script is based on, and complemented by myself:
# https://rpubs.com/boyerag/297592

rm(list = ls())

# Notice that I mark the versions needed
library(ncdf4)          # (v 1.19) package for netcdf manipulation
library(raster)         # (v 3.5-15) package for raster manipulation
library(rgdal)          # (v 1.5-29) package for geospatial analysis
library(chron)          # (v 2.3-56) package to manipulate dates
library(exactextractr)  # (v 0.8.0) from raster to polygon means

# Reading the data:
# Notice that this is a ncdf4 object
nc_data <- nc_open('data/tasmin_day_CanESM5_ssp126_r1i1p1f1_gn_2015.nc')

# In order to work with them, we have to extract the dimension variables
# That is, we need longitude (long), latitude (lati), and time (time).
long <- ncvar_get(nc_data, "lon")  # lon is how it is called in the nc file
long <- 180 - long # Some necessary adjustments so longitudes match to world
lati <- ncvar_get(nc_data, "lat")  # lon is how it is called in the nc file
time <- ncvar_get(nc_data, "time") # lon is how it is called in the nc file

# checking the dimensions:
dim(long) #1440 -- West to East 
dim(lati) #600  -- South to North
dim(time) #365  -- 01/01/15 until 12/31/15

# Let us plot the day of my birthday, but before let us check
# how time-stamps work with nc.
# Notice that they're format is strage. They are based, as per documentaiton
# on "unit: days since 1850-01-01" on a 365-calendar. Meaning that those are
# days since 1850/01/01, but they will not consider leap years

# First, let us find-out the total of leap years between 1850 and 2015
manyLeaps <- function(year1, year0) {
  seqY <- seq(from = year0, to = year1, by = 1)
  many <- sum(leap.year(seqY))
  return(many)
}
many <- manyLeaps(2015, 1850) # We have a total of 40

# Next, let us change the time-stamp from their units to understandable units
time <- time - .5
time <- chron(time + many, # Check that we are adjusting for leap days
              origin. = c(1, 1, 1850), 
              format = c(dates = "mon/day/year"))

# Now, we get the index for the date I was born
noeBD <- which(time == "March/01/2015")

# Now, let us extract the exact look of the world on noeBD
tasmin <- ncvar_get(nc_data, "tasmin")
dim(tasmin) # Check that the dimensions match
# Let us first correct for some technicalities such as NAs
fillvalue <- ncatt_get(nc_data, "tasmin", "_FillValue")
tasmin[tasmin == fillvalue$value] <- NA
# Slice the data to focus on my birthday
tasmin_noeBD <- tasmin[,,noeBD]
# We create a raster out of the array:
r <- raster(t(tasmin_noeBD), 
            xmn = min(long), xmx = max(long), 
            ymn = min(lati), ymx = max(lati), 
            crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction = 'y')

# And this is how the world look like when I was born
plot(r)

# While it is cool to see how weather around the world look like on my birthday,
# researchers are mostly interested on specific political regions.
# The following is an example of how we can use the previous info for my birthday
# to calculate averages for U.S. states using a shapefile
USmap_st <- readOGR(dsn = 'assets/49_state',
                    layer = 'USmap_state')
stateNames <- USmap_st$NAME

noeBD_tasmin <- exact_extract(r, USmap_st, fun = 'mean')
noeBD_tasmin <- cbind.data.frame(stateNames, noeBD_tasmin)

# Notice that noeBD_tasmin is now a dataframe with average values for each state
head(noeBD_tasmin)
# But since I was in California on my bd in 2015, we can focus only in CA
noeBD_tasmin[which(noeBD_tasmin$stateNames == "California"),]
#end