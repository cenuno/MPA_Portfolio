#
# Author: Cristian E. Nuno
# Purpose: Map Visualization
# Date: March 3, 2017
#
# Necessary R packages
library( rgdal )
library( sp )
library( rgeos )
library( TeachingDemos )
library( maptools )
library( dplyr )

# Census Data - Selected socioeconomic indicators in Chicago, 2008 – 2012
dat <- read.csv( "https://data.cityofchicago.org/api/views/kn9c-c2s2/rows.csv?accessType=DOWNLOAD"
                 , header = TRUE
                 , stringsAsFactors = FALSE)
# The last row in this dataset is a row representing the entire city
city.wide.info <- tail( dat, n = 1)

# Remove the last row from the dataset
dat <- dat[ -nrow( dat ), ]

# City of Chicago Community Area Boundaries in GeoJSON format
chicago.com.area <- readOGR("https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON"
                            , "OGRGeoJSON"
                            , stringsAsFactors = FALSE
)
# Transform spatial polygon data frame's "community area number"" variable
# from character to integer
chicago.com.area@data$area_numbe <- as.integer( chicago.com.area@data$area_numbe )
chicago.com.area@data$area_num_1 <- as.integer( chicago.com.area@data$area_num_1 )

# str( chicago.com.area@data )
# 'data.frame':	77 obs. of  9 variables:
# str( dat )
# 'data.frame':	77 obs. of  9 variables:
# Successful merge will be 77 obs. of 18 variables

chicago.com.area@data <- data.frame( chicago.com.area@data
                                     , dat[
                                       match( chicago.com.area@data[ , "area_numbe" ]
                                              , dat[ , "Community.Area.Number"  ]
                                       )
                                       , ]
)
# take a look at newly merged dataframe to ensure a successful merge
str( chicago.com.area@data ) # 77 obs. of 18 variables

# Decide what colors I would like to use
# First hex value contains the group with the least value; the last hex value 
# contains the group with the highest value.
# For inspiration, see: http://www.colourlovers.com/palettes/most-loved/all-time/meta
col.schema <- c( "#CFF09E" # sunlit sea
                 , "#A8DBA8" #seafoam green
                 , "#79BD9A" # sea showing green
                 , "#3B8686" # there we could sail
                 , "#0B486B" # adrift in dreams
)

# Break up the values of the vectors into 5 levels according to their ranks and apply to each level one of the 5 colors.
# select( chicago.com.area@data, 5, 11, 17)
color.pop <- cut( chicago.com.area@data$PER.CAPITA.INCOME
                  , breaks = 5
                  , labels = col.schema
) 
# what the breaks are
# levels( color.pop )
# [1] "(8.12e+03,2.43e+04]" "(2.43e+04,4.04e+04]" "(4.04e+04,5.65e+04]"
# [4] "(5.65e+04,7.26e+04]" "(7.26e+04,8.87e+04]"

# Convert the assigned colors to each value into character
color.pop <- as.character( color.pop )

# Set plot parameters
#### be sure to stretch your RStudio plot space to give yourself as much room as possible!######
par( bg = "antiquewhite"
     , oma = c(0, 0, 0, 0)
)
# Let's finally plot!!
plot( chicago.com.area
      , col = color.pop
)
title( main = "Chicago's Per Capita Income by Community Area, 2008-2012" )
mtext( line = 0.5
       , adj = 0.5
       , cex = 0.8
       , text = "South of the Stevenson Expressway, per capita income never exceeded $40,400 between 2008 and 2012."
)
legend.text.pop = c(   "$  8,120 - $24,300"
                       , "$24,300 - $40,400"
                       , "$40,400 - $56,500"
                       , "$56,500 - $72,600"
                       , "$72,600 - $88,700"
)
legend( "topright"
        , pch = 20
        , pt.cex = 3
        , cex = 1
        , bg = "antiquewhite"
        , legend = legend.text.pop
        , col = col.schema
        , box.col = "antiquewhite"
        , title = "(In 2012 Dollars)"
)
# Community Area Number + Name
com.area.num.name <- select( chicago.com.area@data, 10, 11)
# order by community area
com.area.num.name <- com.area.num.name[ order( com.area.num.name$Community.Area.Number),]
# convert area number to character
com.area.num.name$Community.Area.Number <- as.character( com.area.num.name$Community.Area.Number )
legend("bottomleft"
       , legend = paste( com.area.num.name$Community.Area.Number
                         , com.area.num.name$COMMUNITY.AREA.NAME
                         , sep = "   "
       )
       , bty = "n"
       , title = "Community Area Name"
       , cex = 0.3
)
mtext( side = 1
       , line = 2
       , cex = 0.5
       , adj = 1
       , text = "Source: Cristian E. Nuno | 2008-2012 American Community Survey 5-year Estimates"
)
# add community area numbers
# NOTE:  gCentroid is from the 'rgeos' package
# NOTE: This knowledge was gained using Steven Brey's
#       work in: #http://mazamascience.com/WorkingWithData/?p=1277
# Find the center of each region and label lat and lon of centers
centroids <- gCentroid( chicago.com.area, byid = TRUE )
centroidLons <- coordinates(centroids)[,1] # obtain longitudinal coords
centroidLats <- coordinates(centroids)[,2] # obtain latitutde coords

# Label by area number using centroids
# Create white background for text
shadowtext( centroidLons, centroidLats
            , labels = chicago.com.area@data$area_numbe
            , col = "white"
            , bg = "black"
)
